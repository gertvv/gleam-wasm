import compiler.{type CompilerError}
import glance
import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/string
import project.{type Project, type SourceLocation}

pub type TypeId {
  TypeId(module: SourceLocation, name: String)
}

// fully resolved types
pub type TypeDefinition {
  CustomType(
    id: TypeId,
    publicity: glance.Publicity,
    opaque_: Bool,
    parameters: List(String),
    variants: List(Variant),
  )
  TypeAlias(id: TypeId, type_: TypeReference)
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

pub fn resolve_type(
  data: ModuleInternals,
  parameters: List(String),
  declared_type: glance.Type,
) -> Result(TypeReference, CompilerError) {
  case declared_type {
    glance.NamedType(name, module_option, params) -> {
      let candidate = case module_option {
        None ->
          dict.get(data.types, name)
          |> result.map_error(fn(_) { compiler.ReferenceError(name) })
        Some(module_alias) ->
          dict.get(data.imports, module_alias)
          |> result.map_error(fn(_) { compiler.ReferenceError(module_alias) })
          |> result.try(fn(module) {
            dict.get(module.types, name)
            |> result.map_error(fn(_) { compiler.ReferenceError(name) })
          })
      }
      case candidate {
        Ok(candidate_type) -> {
          case candidate_type, params {
            CustomType(parameters: exp_params, ..), act_params -> {
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
                resolve_type(data, parameters, param_type)
                |> result.map(fn(resolved) { #(param_name, resolved) })
              }))
              |> result.map(fn(mappings) {
                IdentifiedType(candidate_type.id, dict.from_list(mappings))
              })
            }
            _, _ -> todo
          }
        }
        Error(e) ->
          case name, module_option, params {
            "Int", None, [] -> Ok(IntType)
            "Float", None, [] -> Ok(FloatType)
            "String", None, [] -> Ok(StringType)
            "List", None, [item_type] ->
              resolve_type(data, [], item_type)
              |> result.map(fn(resolved) { ListType(resolved) })
            _, _, _ -> Error(e)
          }
      }
    }
    glance.VariableType(name) -> {
      case list.contains(parameters, name) {
        True -> Ok(VariableType(name))
        False -> Error(compiler.ReferenceError(name))
      }
    }
    _ -> todo
  }
}

fn resolve_variant_field(
  data: ModuleInternals,
  parameters: List(String),
  field: glance.Field(glance.Type),
) -> Result(Field(TypeReference), CompilerError) {
  resolve_type(data, parameters, field.item)
  |> result.map(Field(field.label, _))
}

fn resolve_variant(
  data: ModuleInternals,
  parameters: List(String),
  variant: glance.Variant,
) -> Result(Variant, CompilerError) {
  list.map(variant.fields, resolve_variant_field(data, parameters, _))
  |> result.all
  |> result.map(Variant(variant.name, _))
}

pub fn resolve_custom_type(
  data: ModuleInternals,
  parsed: glance.Definition(glance.CustomType),
) -> Result(#(String, TypeDefinition), CompilerError) {
  list.map(parsed.definition.variants, resolve_variant(
    data,
    parsed.definition.parameters,
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

fn attempt_resolve_types(
  custom_types: List(glance.Definition(glance.CustomType)),
  internals: ModuleInternals,
) {
  list.fold(custom_types, #(internals, []), fn(acc, custom_type) {
    let #(internals, errors) = acc
    resolve_custom_type(
      internals,
      custom_type,
    )
    |> result.map_error(fn(err) {
      #(internals, [#(custom_type, err), ..errors])
    })
    |> result.map(fn(type_) {
      #(
        ModuleInternals(
          ..internals,
          types: dict.insert(internals.types, type_.0, type_.1),
        ),
        errors,
      )
    })
    |> result.unwrap_both()
  })
}

// TODO: types can be mutually recursive so another strategy is needed
// I think we can define type identities ahead of time
fn attempt_resolve_types_recursive(
  custom_types: List(glance.Definition(glance.CustomType)),
  internals: ModuleInternals,
) {
  case attempt_resolve_types(custom_types, internals) {
    #(internals, []) -> Ok(internals)
    #(internals, errors) -> {
      case list.map(errors, pair.first) {
        remaining if remaining == custom_types ->
          Error(compiler.AnotherTypeError)
        // TODO: proper error
        remaining -> attempt_resolve_types_recursive(remaining, internals)
      }
    }
  }
}

// TODO: result type annotation
pub fn resolve_types(
  project: Project,
  location: SourceLocation,
  imports: Dict(String, Module),
  parsed: glance.Module,
) {
  attempt_resolve_types_recursive(parsed.custom_types, ModuleInternals(project, location, imports, dict.new()))
}

fn analyze_high_level(
  project: Project,
  location: SourceLocation,
  modules: Dict(SourceLocation, Module),
) -> Result(#(Module, Dict(SourceLocation, Module)), CompilerError) {
  // check if the module has already been analyzed
  use _ <- result.try_recover(
    dict.get(modules, location)
    |> result.map(fn(module) { #(module, modules) }),
  )
  // otherwise attempt to parse the source
  use parsed <- result.try(project.parse_module(project, location))
  // analyze all direct imports
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
              Some(glance.Discarded(alias)) -> todo
              // not sure what this means
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
  use internals <- result.map(resolve_types(project, location, imports, parsed))
  // TODO: resolve constants and functions
  // TODO: filter to only public parts
  #(Module(location, imports, internals.types), modules)
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
  |> result.map(to_dep_tree)
  |> result.map(print_dep_tree(_, ""))
  |> result.map(io.println)
  |> result.map(fn(_) { Nil })
  |> io.debug()
}
