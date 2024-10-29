import compiler.{type CompilerError}
import glance
import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string
import pprint
import project.{type Project, type SourceLocation}

pub type TypeId {
  TypeId(module: SourceLocation, name: String)
}

// fully resolved types

// TODO: do I need to put the TypeId on the type definition itself??
pub type TypeDefinition {
  CustomType(
    id: TypeId,
    publicity: glance.Publicity,
    parameters: List(String),
    opaque_: Bool,
    variants: List(Variant),
  )
  TypeAlias(
    id: TypeId,
    publicity: glance.Publicity,
    parameters: List(String),
    aliased: TypeReference,
  )
}

pub type TypeReference {
  IntType
  FloatType
  StringType
  ListType(item_type: TypeReference)
  IdentifiedType(
    generic_type: TypeId,
    assigned_types: Dict(String, TypeReference),
  )
  TupleType(elements: List(TypeReference))
  FunctionType(parameters: List(TypeReference), return: TypeReference)
  VariableType(name: String)
}

pub type Variant {
  Variant(name: String, fields: List(Field(TypeReference)))
}

pub type Field(t) {
  Field(label: Option(String), item: t)
}

// Detailed info on one module, high-level info on other modules
pub type Analysis {
  Analysis(module: ModuleDetail, modules: Dict(SourceLocation, Module))
}

// Will include only high level type information and signatures
pub type Module {
  // TODO: replace Module by SourceLocation for the imports
  Module(
    location: SourceLocation,
    imports: Dict(String, Module),
    types: Dict(String, TypeDefinition),
  )
}

// Will include types for expressions
pub type ModuleDetail {
  ModuleDetail(module: Module)
}

fn resolve_module(
  project: Project,
  importing_package: String,
  imported_module: String,
) -> Result(SourceLocation, CompilerError) {
  use _ <- result.try_recover(project.get_module_location(
    project,
    importing_package,
    imported_module,
  ))
  list.find_map(dict.keys(project.packages), project.get_module_location(
    project,
    _,
    imported_module,
  ))
  |> result.map_error(fn(_) { compiler.ReferenceError(imported_module) })
}

pub type ModuleInternals {
  ModuleInternals(
    project: Project,
    location: SourceLocation,
    imports: Dict(String, Module),
    types: Dict(String, TypeDefinition),
  )
}

fn union_type_variables(ts: List(TypeReference)) -> set.Set(String) {
  ts
  |> list.map(find_type_variables)
  |> list.fold(set.new(), set.union)
}

fn find_type_variables(t: TypeReference) -> set.Set(String) {
  case t {
    FunctionType(param_types, return_type) ->
      union_type_variables([return_type, ..param_types])
    IdentifiedType(_, assigned_types) ->
      union_type_variables(dict.values(assigned_types))
    ListType(item_type) -> find_type_variables(item_type)
    TupleType(item_types) -> union_type_variables(item_types)
    VariableType(name) -> set.from_list([name])
    _ -> set.new()
  }
}

pub fn resolve_type(
  data: ModuleInternals,
  prototypes: Dict(String, Prototype),
  declared_type: glance.Type,
) -> Result(TypeReference, CompilerError) {
  case declared_type {
    glance.NamedType(name, module_option, params) -> {
      let candidate = case module_option {
        None ->
          dict.get(prototypes, name)
          |> result.map_error(fn(_) { compiler.ReferenceError(name) })
        Some(module_alias) ->
          dict.get(data.imports, module_alias)
          |> result.map_error(fn(_) { compiler.ReferenceError(module_alias) })
          |> result.try(fn(module) {
            dict.get(module.types, name)
            |> result.map_error(fn(_) { compiler.ReferenceError(name) })
          })
          |> result.map(fn(def) { Prototype(def.id, def.parameters) })
      }
      case candidate {
        Ok(candidate_type) -> {
          case candidate_type, params {
            Prototype(parameters: exp_params, ..), act_params -> {
              list.strict_zip(exp_params, act_params)
              |> result.map_error(fn(_) {
                compiler.TypeArityError(
                  declared_type,
                  list.length(exp_params),
                  list.length(act_params),
                )
              })
              |> result.try(list.try_map(_, fn(pair) {
                let #(param_name, param_type) = pair
                resolve_type(data, prototypes, param_type)
                |> result.map(fn(resolved) { #(param_name, resolved) })
              }))
              |> result.map(fn(mappings) {
                IdentifiedType(candidate_type.id, dict.from_list(mappings))
              })
            }
          }
        }
        Error(e) ->
          case name, module_option, params {
            "Int", None, [] -> Ok(IntType)
            "Float", None, [] -> Ok(FloatType)
            "String", None, [] -> Ok(StringType)
            "List", None, [item_type] ->
              resolve_type(data, prototypes, item_type)
              |> result.map(fn(resolved) { ListType(resolved) })
            _, _, _ -> Error(e)
          }
      }
    }
    glance.TupleType(_) -> {
      todo
    }
    glance.FunctionType(function_parameters, return) -> {
      let resolved_parameters =
        list.try_map(function_parameters, resolve_type(data, prototypes, _))
      let resolved_return = resolve_type(data, prototypes, return)
      case resolved_parameters, resolved_return {
        Ok(p), Ok(r) -> Ok(FunctionType(p, r))
        Error(e), _ -> Error(e)
        _, Error(e) -> Error(e)
      }
    }
    glance.VariableType(name) -> Ok(VariableType(name))
    glance.HoleType(_) -> {
      todo
    }
  }
}

fn resolve_variant_field(
  data: ModuleInternals,
  prototypes: Dict(String, Prototype),
  parameters: set.Set(String),
  field: glance.Field(glance.Type),
) -> Result(Field(TypeReference), CompilerError) {
  resolve_type(data, prototypes, field.item)
  |> result.try(fn(t) {
    let undeclared =
      find_type_variables(t) |> set.difference(parameters) |> set.to_list
    case undeclared {
      [] -> Ok(t)
      [u, ..] -> Error(compiler.ReferenceError(u))
    }
  })
  |> result.map(Field(field.label, _))
}

fn resolve_variant(
  data: ModuleInternals,
  prototypes: Dict(String, Prototype),
  parameters: set.Set(String),
  variant: glance.Variant,
) -> Result(Variant, CompilerError) {
  list.map(variant.fields, resolve_variant_field(
    data,
    prototypes,
    parameters,
    _,
  ))
  |> result.all
  |> result.map(Variant(variant.name, _))
}

pub fn resolve_custom_type(
  data: ModuleInternals,
  prototypes: Dict(String, Prototype),
  parsed: glance.Definition(glance.CustomType),
) -> Result(#(String, TypeDefinition), CompilerError) {
  list.map(parsed.definition.variants, resolve_variant(
    data,
    prototypes,
    set.from_list(parsed.definition.parameters),
    _,
  ))
  |> result.all
  |> result.map(CustomType(
    id: TypeId(data.location, parsed.definition.name),
    publicity: parsed.definition.publicity,
    opaque_: parsed.definition.opaque_,
    parameters: parsed.definition.parameters,
    variants: _,
  ))
  |> result.map(fn(type_) { #(parsed.definition.name, type_) })
}

fn resolve_type_alias(
  data: ModuleInternals,
  prototypes: Dict(String, Prototype),
  parsed: glance.Definition(glance.TypeAlias),
) -> Result(#(String, TypeDefinition), CompilerError) {
  resolve_type(data, prototypes, parsed.definition.aliased)
  |> result.map(fn(resolved) {
    #(
      parsed.definition.name,
      TypeAlias(
        TypeId(data.location, parsed.definition.name),
        parsed.definition.publicity,
        parsed.definition.parameters,
        resolved,
      ),
    )
  })
}

pub type Prototype {
  Prototype(id: TypeId, parameters: List(String))
}

fn prototype_type_alias(
  loc: SourceLocation,
  def: glance.Definition(glance.TypeAlias),
) -> #(String, Prototype) {
  case def {
    glance.Definition(_, glance.TypeAlias(name: name, parameters: params, ..)) -> #(
      name,
      Prototype(TypeId(loc, name), params),
    )
  }
}

fn prototype_custom_type(
  loc: SourceLocation,
  def: glance.Definition(glance.CustomType),
) -> #(String, Prototype) {
  case def {
    glance.Definition(_, glance.CustomType(name: name, parameters: params, ..)) -> #(
      name,
      Prototype(TypeId(loc, name), params),
    )
  }
}

pub fn resolve_types(
  project: Project,
  location: SourceLocation,
  imports: Dict(String, Module),
  parsed: glance.Module,
) -> Result(Dict(String, TypeDefinition), CompilerError) {
  let custom_prototypes =
    parsed.custom_types
    |> list.map(prototype_custom_type(location, _))
  let alias_prototypes =
    parsed.type_aliases |> list.map(prototype_type_alias(location, _))
  let prototypes =
    dict.from_list(list.append(custom_prototypes, alias_prototypes))
  let internals = ModuleInternals(project, location, imports, dict.new())
  let aliases =
    list.map(parsed.type_aliases, resolve_type_alias(internals, prototypes, _))
  let custom_types =
    list.map(parsed.custom_types, resolve_custom_type(internals, prototypes, _))
  // TODO: ensure uniqueness?
  list.append(aliases, custom_types)
  |> result.all
  |> result.map(dict.from_list)
}

fn analyze_high_level(
  project: Project,
  location: SourceLocation,
  modules: Dict(SourceLocation, Module),
) -> Result(#(Module, Dict(SourceLocation, Module)), CompilerError) {
  use _ <- result.try_recover(
    dict.get(modules, location)
    |> result.map(fn(module) { #(module, modules) }),
  )
  use parsed <- result.try(project.parse_module(project, location))
  use #(imports, modules) <- result.try(
    list.try_fold(parsed.imports, #([], modules), fn(acc, import_) {
      let #(imports, modules) = acc
      use #(alias, location) <- result.try(case import_ {
        glance.Definition(
          definition: glance.Import(
            module: module_name,
            alias: alias,
            unqualified_types: _,
            unqualified_values: _,
          ),
          attributes: _,
        ) -> {
          resolve_module(project, location.package_name, module_name)
          |> result.map(fn(location) {
            case alias {
              Some(glance.Named(alias)) -> #(alias, location)
              Some(glance.Discarded(alias)) -> #(alias, location)
              None -> #(location.shorthand, location)
            }
          })
        }
      })
      use #(module, modules) <- result.map(analyze_high_level(
        project,
        location,
        modules,
      ))
      #([#(alias, module), ..imports], modules)
    }),
  )
  let imports = dict.from_list(imports)
  use types <- result.map(resolve_types(project, location, imports, parsed))
  // TODO: resolve constants and functions
  // TODO: filter to only public parts
  #(Module(location, imports, types), modules)
}

pub fn analyze_module(
  project: Project,
  location: SourceLocation,
  modules: Dict(SourceLocation, Module),
) -> Result(Analysis, CompilerError) {
  use #(module, modules) <- result.map(analyze_high_level(
    project,
    location,
    modules,
  ))
  Analysis(ModuleDetail(module), modules)
}

pub type ModDeps {
  ModDeps(module: String, deps: List(ModDeps))
}

fn to_dep_tree_internal(module: Module) -> ModDeps {
  ModDeps(
    module.location.module_path,
    list.map(dict.values(module.imports), to_dep_tree_internal),
  )
}

fn to_dep_tree(analysis: Analysis) {
  to_dep_tree_internal(analysis.module.module)
}

fn print_dep_tree(tree: ModDeps, indent: String) -> String {
  [
    indent <> tree.module,
    ..list.map(tree.deps, print_dep_tree(_, indent <> "  "))
  ]
  |> string.join("\n")
}

pub fn main() {
  project.scan_project(".")
  |> result.try(fn(project) {
    analyze_module(
      project,
      project.SourceLocation(project.name, "stub", "stub"),
      dict.new(),
    )
  })
  |> pprint.debug()
  |> result.map(to_dep_tree)
  |> result.map(print_dep_tree(_, ""))
  |> result.map(io.println)
  |> result.map(fn(_) { Nil })
  |> io.debug()
}
