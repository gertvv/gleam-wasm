import compiler.{type CompilerError}
import glance
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set
import gleam/string
import pprint
import project.{type ModuleId, type Project}

pub type TypeId {
  BuiltInType(name: String)
  ModuleType(module: ModuleId, name: String)
}

// fully resolved types

// TODO: keep TypeAlias here or can we have just CustomType?
// TODO: take publicity out?
pub type TypeDefinition {
  CustomType(
    publicity: glance.Publicity,
    parameters: List(String),
    opaque_: Bool,
    variants: List(Variant),
  )
  TypeAlias(
    publicity: glance.Publicity,
    parameters: List(String),
    aliased: Type,
  )
}

// TODO: at the module level could use GenericType to represent either TypeAlias or CustomType
// but will need to keep track of the CustomTypes separately
pub type GenericType {
  GenericType(typ: Type, parameters: List(String))
}

pub type Type {
  // TODO: rename TypeConstructor
  IdentifiedType(generic_type: TypeId, assigned_types: List(Type))
  // TODO: make TypeConstructor? (Add a TypeId constructor TupleType(arity))
  TupleType(elements: List(Type))
  // TODO: make TypeConstructor? (Add a TypeId constructor FunctionType(arity))
  FunctionType(parameters: List(Type), return: Type)
  // TODO: rename TypeVariable
  VariableType(name: String)
}

pub type Variant {
  Variant(name: String, fields: List(Field(Type)))
}

pub type Field(t) {
  Field(label: Option(String), item: t)
}

// Detailed info on one module, high-level info on other modules
pub type Analysis {
  Analysis(module: ModuleDetail, modules: Dict(ModuleId, Module))
}

// Will include only high level type information and signatures
pub type Module {
  Module(
    location: ModuleId,
    imports: Dict(String, ModuleId),
    types: Dict(String, TypeDefinition),
  )
}

// Will include types for expressions
pub type ModuleDetail {
  ModuleDetail(module: Module)
}

// Expression with types
pub type Expression {
  Int(val: String)
  String(val: String)
  Variable(typ: Type, name: String)
  Fn(
    typ: Type,
    argument_names: List(glance.AssignmentName),
    body: List(Expression),
    // TODO: should take Statement instead
  )
  BinaryOperator(
    typ: Type,
    name: glance.BinaryOperator,
    left: Expression,
    right: Expression,
  )
}

pub type Constraint {
  Equal(a: Type, b: Type)
}

pub type Context {
  Context(
    substitution: Dict(String, Type),
    constraints: List(Constraint),
    next_variable: Int,
  )
}

fn fresh_type_variable(context: Context) -> #(Context, Type) {
  let name = "$" <> int.to_string(context.next_variable)
  let type_variable = VariableType(name)
  #(
    Context(
      dict.insert(context.substitution, name, type_variable),
      constraints: context.constraints,
      next_variable: context.next_variable + 1,
    ),
    type_variable,
  )
}

// TODO: integrate with resolve_type
fn maybe_fresh_type_variable(
  context: Context,
  typ: Option(glance.Type),
) -> #(Context, Type) {
  case typ {
    Some(t) -> todo
    None -> fresh_type_variable(context)
  }
}

fn assignment_name_to_string(name: glance.AssignmentName) -> String {
  case name {
    glance.Discarded(_) -> todo
    glance.Named(str) -> str
  }
}

fn add_constraint(context: Context, constraint: Constraint) -> Context {
  Context(..context, constraints: [constraint, ..context.constraints])
}

fn add_substitution(context: Context, name: String, typ: Type) -> Context {
  Context(..context, substitution: dict.insert(context.substitution, name, typ))
}

// TODO: why not pass the environment via the context?
// TODO: integrate with resolve_type
pub fn init_inference(
  expr: glance.Expression,
  expected_type: Type,
  environment: Dict(String, Type),
  context: Context,
) -> #(Context, Expression) {
  case expr {
    glance.Variable(name) -> {
      let assert Ok(variable_type) = dict.get(environment, name)
      #(
        add_constraint(context, Equal(expected_type, variable_type)),
        Variable(variable_type, name),
      )
    }
    glance.Fn(args, return, [glance.Expression(body)]) -> {
      let #(context, return_type) = maybe_fresh_type_variable(context, return)
      let #(context, param_types) =
        args
        |> list.map(fn(p) { p.type_ })
        |> list.map_fold(from: context, with: maybe_fresh_type_variable)
      // TODO: should Discarded names even be added to the environment?
      let environment =
        list.map2(args, param_types, fn(arg, typ) {
          #(assignment_name_to_string(arg.name), typ)
        })
        |> dict.from_list
        |> dict.merge(environment, _)
      let #(context, body) =
        init_inference(body, return_type, environment, context)
      let fn_type = FunctionType(param_types, return_type)
      #(
        add_constraint(context, Equal(expected_type, fn_type)),
        Fn(fn_type, list.map(args, fn(arg) { arg.name }), [body]),
      )
    }
    glance.BinaryOperator(glance.MultInt, left, right) -> {
      let int_type = IdentifiedType(BuiltInType("Int"), [])
      let #(context, left) =
        init_inference(left, int_type, environment, context)
      let #(context, right) =
        init_inference(right, int_type, environment, context)
      #(
        Context(
          ..context,
          constraints: [Equal(expected_type, int_type), ..context.constraints],
        ),
        BinaryOperator(expected_type, glance.MultInt, left, right),
      )
    }
    _ -> todo
  }
}

pub fn get_sub(context: Context, t: Type) -> Option(Type) {
  case t {
    VariableType(i) ->
      dict.get(context.substitution, i)
      |> result.map(Some)
      |> result.unwrap(None)
    _ -> None
  }
}

fn occurs_in(var: String, t: Type) -> Bool {
  case t {
    VariableType(i) -> var == i
    FunctionType(args, ret) -> list.any([ret, ..args], occurs_in(var, _))
    IdentifiedType(_, sub) -> list.any(sub, occurs_in(var, _))
    TupleType(args) -> list.any(args, occurs_in(var, _))
  }
}

fn unify(context: Context, a: Type, b: Type) -> Result(Context, CompilerError) {
  let sub_a = get_sub(context, a) |> option.unwrap(a)
  let sub_b = get_sub(context, b) |> option.unwrap(b)
  case a, b {
    VariableType(i), VariableType(j) if i == j -> Ok(context)
    FunctionType(args_i, ret_i), FunctionType(args_j, ret_j) -> {
      case list.length(args_i) == list.length(args_j) {
        False -> Error(compiler.AnotherTypeError)
        True -> {
          list.zip([ret_i, ..args_i], [ret_j, ..args_j])
          |> list.try_fold(from: context, with: fn(context, pair) {
            unify(context, pair.0, pair.1)
          })
        }
      }
    }
    VariableType(_), _ if sub_a != a -> unify(context, sub_a, b)
    _, VariableType(_) if sub_b != b -> unify(context, a, sub_b)
    VariableType(i), _ ->
      case occurs_in(i, b) {
        True -> Error(compiler.AnotherTypeError)
        False -> Ok(add_substitution(context, i, b))
      }
    _, VariableType(j) ->
      case occurs_in(j, a) {
        True -> Error(compiler.AnotherTypeError)
        False -> Ok(add_substitution(context, j, a))
      }
    _, _ -> todo
  }
}

// TODO: maybe should return just a substitution?
pub fn solve_constraints(context: Context) -> Result(Context, CompilerError) {
  list.try_fold(
    over: context.constraints,
    from: context,
    with: fn(context, constraint) {
      let Equal(a, b) = constraint
      unify(context, a, b)
    },
  )
  |> result.map(fn(context) { Context(..context, constraints: []) })
}

pub fn substitute(t: Type, subs: Dict(String, Type)) -> Type {
  case t {
    FunctionType(params, return) ->
      FunctionType(
        list.map(params, substitute(_, subs)),
        substitute(return, subs),
      )
    IdentifiedType(id, assigned) ->
      IdentifiedType(id, list.map(assigned, substitute(_, subs)))
    TupleType(_) -> todo
    VariableType(name) -> dict.get(subs, name) |> result.unwrap(t)
  }
}

pub fn substitute_expression(
  expr: Expression,
  subs: Dict(String, Type),
) -> Expression {
  case expr {
    BinaryOperator(t, op, l, r) ->
      BinaryOperator(
        substitute(t, subs),
        op,
        substitute_expression(l, subs),
        substitute_expression(r, subs),
      )
    Fn(t, param_names, body) ->
      Fn(
        substitute(t, subs),
        param_names,
        list.map(body, substitute_expression(_, subs)),
      )
    Int(_) -> expr
    String(_) -> expr
    Variable(t, name) -> Variable(substitute(t, subs), name)
  }
}

fn resolve_module(
  project: Project,
  importing_package: String,
  imported_module: String,
) -> Result(ModuleId, CompilerError) {
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

// TODO: split public_types vs private_types
pub type ModuleInternals {
  ModuleInternals(
    project: Project,
    location: ModuleId,
    imports: Dict(String, Module),
    types: Dict(String, TypeDefinition),
  )
}

fn union_type_variables(ts: List(Type)) -> set.Set(String) {
  ts
  |> list.map(find_type_variables)
  |> list.fold(set.new(), set.union)
}

fn find_type_variables(t: Type) -> set.Set(String) {
  case t {
    FunctionType(param_types, return_type) ->
      union_type_variables([return_type, ..param_types])
    IdentifiedType(_, assigned_types) -> union_type_variables(assigned_types)
    TupleType(item_types) -> union_type_variables(item_types)
    VariableType(name) -> set.from_list([name])
  }
}

pub fn resolve_type(
  data: ModuleInternals,
  prototypes: Dict(String, Prototype),
  declared_type: glance.Type,
) -> Result(Type, CompilerError) {
  case declared_type {
    glance.NamedType(name, module_option, params) -> {
      let candidate = case module_option {
        None ->
          dict.get(prototypes, name)
          |> result.map(fn(t) { #(data.location, t) })
          |> result.map_error(fn(_) { compiler.ReferenceError(name) })
        Some(module_alias) ->
          dict.get(data.imports, module_alias)
          |> result.map_error(fn(_) { compiler.ReferenceError(module_alias) })
          |> result.try(fn(module) {
            dict.get(module.types, name)
            |> result.map_error(fn(_) { compiler.ReferenceError(name) })
            |> result.map(fn(def) {
              #(module.location, Prototype(def.parameters))
            })
          })
      }
      case candidate {
        Ok(#(location, candidate_type)) -> {
          case candidate_type, params {
            Prototype(parameters: exp_params), act_params -> {
              list.strict_zip(exp_params, act_params)
              |> result.map_error(fn(_) {
                compiler.TypeArityError(
                  declared_type,
                  list.length(exp_params),
                  list.length(act_params),
                )
              })
              |> result.try(list.try_map(_, fn(pair) {
                let #(_, param_type) = pair
                resolve_type(data, prototypes, param_type)
              }))
              |> result.map(IdentifiedType(ModuleType(location, name), _))
            }
          }
        }
        Error(e) ->
          // TODO: can probably handle via lookup instead
          case name, module_option, params {
            "Int", None, [] -> Ok(IdentifiedType(BuiltInType("Int"), []))
            "Float", None, [] -> Ok(IdentifiedType(BuiltInType("Float"), []))
            "String", None, [] -> Ok(IdentifiedType(BuiltInType("String"), []))
            "List", None, [item_type] ->
              resolve_type(data, prototypes, item_type)
              |> result.map(fn(resolved) {
                IdentifiedType(BuiltInType("List"), [resolved])
              })
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
) -> Result(Field(Type), CompilerError) {
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
    publicity: parsed.definition.publicity,
    opaque_: parsed.definition.opaque_,
    parameters: parsed.definition.parameters,
    variants: _,
  ))
  |> result.map(fn(type_) { #(parsed.definition.name, type_) })
}

// TODO: keep a TypeAlias as a TypeId reference or substitute it?
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
        parsed.definition.publicity,
        parsed.definition.parameters,
        resolved,
      ),
    )
  })
}

pub type Prototype {
  Prototype(parameters: List(String))
}

fn prototype_type_alias(
  def: glance.Definition(glance.TypeAlias),
) -> #(String, Prototype) {
  case def {
    glance.Definition(_, glance.TypeAlias(name: name, parameters: params, ..)) -> #(
      name,
      Prototype(params),
    )
  }
}

fn prototype_custom_type(
  def: glance.Definition(glance.CustomType),
) -> #(String, Prototype) {
  case def {
    glance.Definition(_, glance.CustomType(name: name, parameters: params, ..)) -> #(
      name,
      Prototype(params),
    )
  }
}

pub fn resolve_types(
  project: Project,
  location: ModuleId,
  imports: Dict(String, ModuleId),
  modules: Dict(ModuleId, Module),
  parsed: glance.Module,
) -> Result(Dict(String, TypeDefinition), CompilerError) {
  let custom_prototypes =
    parsed.custom_types
    |> list.map(prototype_custom_type)
  let alias_prototypes = parsed.type_aliases |> list.map(prototype_type_alias)
  let prototypes =
    dict.from_list(list.append(custom_prototypes, alias_prototypes))
  let imported_modules =
    imports
    |> dict.map_values(fn(_, loc) {
      let assert Ok(module) = dict.get(modules, loc)
      module
    })
  let internals =
    ModuleInternals(project, location, imported_modules, dict.new())
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
  location: ModuleId,
  modules: Dict(ModuleId, Module),
) -> Result(#(Module, Dict(ModuleId, Module)), CompilerError) {
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
              None -> #(project.shorthand(location), location)
            }
          })
        }
      })
      use #(module, modules) <- result.map(analyze_high_level(
        project,
        location,
        modules,
      ))
      #(
        [#(alias, module.location), ..imports],
        dict.insert(modules, location, module),
      )
    }),
  )
  let imports = dict.from_list(imports)
  use types <- result.map(resolve_types(
    project,
    location,
    imports,
    modules,
    parsed,
  ))
  // TODO: resolve constants and functions
  // TODO: filter to only public parts
  #(Module(location, imports, types), modules)
}

pub fn analyze_module(
  project: Project,
  location: ModuleId,
  modules: Dict(ModuleId, Module),
) -> Result(Analysis, CompilerError) {
  use #(module, modules) <- result.map(analyze_high_level(
    project,
    location,
    modules,
  ))
  Analysis(ModuleDetail(module), dict.insert(modules, location, module))
}

pub type ModDeps {
  ModDeps(module: String, deps: List(ModDeps))
}

fn to_dep_tree_internal(
  location: ModuleId,
  modules: Dict(ModuleId, Module),
) -> ModDeps {
  ModDeps(
    location.module_path,
    list.map(
      dict.get(modules, location)
        |> result.map(fn(module) { dict.values(module.imports) })
        |> result.unwrap([]),
      to_dep_tree_internal(_, modules),
    ),
  )
}

fn to_dep_tree(analysis: Analysis) {
  to_dep_tree_internal(analysis.module.module.location, analysis.modules)
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
      project.SourceLocation(project.name, "stub"),
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
