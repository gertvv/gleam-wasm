import compiler.{type CompilerError}
import glance
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set
import gleam/string
import pprint
import project.{type ModuleId, type Project}

pub type TypeId {
  BuiltInType(name: String)
  TypeFromModule(module: ModuleId, name: String)
}

// fully resolved types

pub type CustomType {
  CustomType(parameters: List(String), opaque_: Bool, variants: List(Variant))
}

pub type GenericType {
  GenericType(typ: Type, parameters: List(String))
}

pub type Type {
  ModuleType(module: Module)
  TypeConstructor(generic_type: TypeId, assigned_types: List(Type))
  // TODO: make TypeConstructor? (Add a TypeId constructor TupleType(arity))
  TupleType(elements: List(Type))
  // TODO: make TypeConstructor? (Add a TypeId constructor FunctionType(arity))
  FunctionType(parameters: List(Type), return: Type)
  TypeVariable(name: String)
}

const int_type = TypeConstructor(BuiltInType("Int"), [])

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
    types: Dict(String, GenericType),
    custom_types: Dict(String, CustomType),
    // TODO: functions require more meta-data than just Type
    functions: Dict(String, Type),
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
  FunctionReference(typ: Type, module: ModuleId, name: String)
  Fn(
    typ: Type,
    argument_names: List(glance.AssignmentName),
    body: List(Expression),
    // TODO: should take Statement instead
  )
  FieldAccess(typ: Type, container: Expression, label: String)
  Call(function: Expression, arguments: List(Expression))
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
    internals: ModuleInternals,
    substitution: Dict(String, Type),
    constraints: List(Constraint),
    next_variable: Int,
  )
}

fn fresh_type_variable(context: Context) -> #(Context, Type) {
  let name = "$" <> int.to_string(context.next_variable)
  let type_variable = TypeVariable(name)
  #(
    Context(
      ..context,
      substitution: dict.insert(context.substitution, name, type_variable),
      next_variable: context.next_variable + 1,
    ),
    type_variable,
  )
}

fn resolve_optional_type(
  context: Context,
  typ: Option(glance.Type),
) -> Result(#(Context, Type), CompilerError) {
  case typ {
    Some(t) ->
      resolve_type(context.internals, t)
      |> result.map(fn(t) { #(context, t) })
    None -> Ok(fresh_type_variable(context))
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
// TODO: add modules and unqualified imports to the environment
pub fn init_inference(
  expr: glance.Expression,
  expected_type: Type,
  environment: Dict(String, Type),
  context: Context,
) -> Result(#(Context, Expression), CompilerError) {
  case expr {
    glance.Int(value) -> {
      Ok(#(add_constraint(context, Equal(expected_type, int_type)), Int(value)))
    }
    glance.Variable(name) -> {
      use variable_type <- result.map(
        dict.get(environment, name)
        |> result.map_error(fn(_) { compiler.ReferenceError(name) }),
      )
      #(
        add_constraint(context, Equal(expected_type, variable_type)),
        Variable(variable_type, name),
      )
    }
    glance.Fn(args, return, [glance.Expression(body)]) -> {
      use #(context, return_type) <- result.try(resolve_optional_type(
        context,
        return,
      ))
      use #(context, param_types) <- result.try(
        args
        |> list.try_fold(from: #(context, []), with: fn(pair, param) {
          let #(context, acc) = pair
          resolve_optional_type(context, param.type_)
          |> result.map(fn(r) { #(r.0, [r.1, ..acc]) })
        }),
      )
      let param_types = list.reverse(param_types)

      // TODO: should Discarded names even be added to the environment?
      let environment =
        list.map2(args, param_types, fn(arg, typ) {
          #(assignment_name_to_string(arg.name), typ)
        })
        |> dict.from_list
        |> dict.merge(environment, _)
      use #(context, body) <- result.map(init_inference(
        body,
        return_type,
        environment,
        context,
      ))
      let fn_type = FunctionType(param_types, return_type)
      #(
        add_constraint(context, Equal(expected_type, fn_type)),
        Fn(fn_type, list.map(args, fn(arg) { arg.name }), [body]),
      )
    }
    glance.Call(function, args) -> {
      let #(context, arg_types) =
        args
        |> list.map_fold(from: context, with: fn(context, _) {
          fresh_type_variable(context)
        })
      let fn_type = FunctionType(arg_types, expected_type)
      use #(context, function) <- result.try(init_inference(
        function,
        fn_type,
        environment,
        context,
      ))
      use #(context, args) <- result.map(
        list.zip(args, arg_types)
        |> list.try_fold(from: #(context, []), with: fn(acc_pair, arg_pair) {
          let #(context, acc) = acc_pair
          let #(arg, arg_type) = arg_pair
          init_inference(arg.item, arg_type, environment, context)
          |> result.map(fn(r) { #(r.0, [r.1, ..acc]) })
        }),
      )
      #(context, Call(function, list.reverse(args)))
    }
    glance.FieldAccess(container, label) -> {
      // short-cut module field access if possible
      case container {
        glance.Variable(name) -> {
          Ok(name)
        }
        _ -> Error(Nil)
      }
      |> result.try(dict.get(environment, _))
      |> result.try(fn(val) {
        case val {
          ModuleType(module) -> Ok(module)
          _ -> Error(Nil)
        }
      })
      |> result.try(fn(module) {
        dict.get(module.functions, label)
        |> result.map(fn(function_type) {
          #(
            add_constraint(context, Equal(expected_type, function_type)),
            FunctionReference(function_type, module.location, label),
          )
        })
      })
      |> result.try_recover(fn(_) { Error(compiler.AnotherTypeError) })
      //|> result.try_recover(fn() {
      // let #(context, container_type) = fresh_type_variable(context)
      // let #(context, container) =
      //   init_inference(container, container_type, environment, context)
      // todo
      //})
    }
    glance.BinaryOperator(glance.MultInt, left, right) -> {
      use #(context, left) <- result.try(init_inference(
        left,
        int_type,
        environment,
        context,
      ))
      use #(context, right) <- result.map(init_inference(
        right,
        int_type,
        environment,
        context,
      ))
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
    TypeVariable(i) ->
      dict.get(context.substitution, i)
      |> result.map(Some)
      |> result.unwrap(None)
    _ -> None
  }
}

fn occurs_in(var: String, t: Type) -> Bool {
  case t {
    TypeVariable(i) -> var == i
    FunctionType(args, ret) -> list.any([ret, ..args], occurs_in(var, _))
    TypeConstructor(_, sub) -> list.any(sub, occurs_in(var, _))
    TupleType(args) -> list.any(args, occurs_in(var, _))
    ModuleType(_) -> False
  }
}

fn unify(context: Context, a: Type, b: Type) -> Result(Context, CompilerError) {
  let sub_a = get_sub(context, a) |> option.unwrap(a)
  let sub_b = get_sub(context, b) |> option.unwrap(b)
  case a, b {
    TypeVariable(i), TypeVariable(j) if i == j -> Ok(context)
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
    TypeVariable(_), _ if sub_a != a -> unify(context, sub_a, b)
    _, TypeVariable(_) if sub_b != b -> unify(context, a, sub_b)
    TypeVariable(i), _ ->
      case occurs_in(i, b) {
        True -> Error(compiler.AnotherTypeError)
        False -> Ok(add_substitution(context, i, b))
      }
    _, TypeVariable(j) ->
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
    ModuleType(_) -> t
    FunctionType(params, return) ->
      FunctionType(
        list.map(params, substitute(_, subs)),
        substitute(return, subs),
      )
    TypeConstructor(id, assigned) ->
      TypeConstructor(id, list.map(assigned, substitute(_, subs)))
    TupleType(_) -> todo
    TypeVariable(name) -> dict.get(subs, name) |> result.unwrap(t)
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
    Call(function, args) ->
      Call(
        substitute_expression(function, subs),
        list.map(args, substitute_expression(_, subs)),
      )
    FieldAccess(_, _, _) -> todo
    FunctionReference(t, loc, name) ->
      FunctionReference(substitute(t, subs), loc, name)
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
    types: Dict(String, GenericType),
  )
}

fn union_type_variables(ts: List(Type)) -> set.Set(String) {
  ts
  |> list.map(find_type_variables)
  |> list.fold(set.new(), set.union)
}

fn find_type_variables(t: Type) -> set.Set(String) {
  case t {
    ModuleType(_) -> set.new()
    FunctionType(param_types, return_type) ->
      union_type_variables([return_type, ..param_types])
    TypeConstructor(_, assigned_types) -> union_type_variables(assigned_types)
    TupleType(item_types) -> union_type_variables(item_types)
    TypeVariable(name) -> set.from_list([name])
  }
}

fn prototype(parameters: List(String)) -> GenericType {
  GenericType(TypeVariable(""), parameters)
}

pub fn resolve_type(
  data: ModuleInternals,
  declared_type: glance.Type,
) -> Result(Type, CompilerError) {
  case declared_type {
    glance.NamedType(name, module_option, params) -> {
      let candidate = case module_option {
        None ->
          dict.get(data.types, name)
          |> result.map(fn(t) { #(data.location, t) })
          |> result.map_error(fn(_) { compiler.ReferenceError(name) })
        Some(module_alias) ->
          dict.get(data.imports, module_alias)
          |> result.map_error(fn(_) { compiler.ReferenceError(module_alias) })
          |> result.try(fn(module) {
            dict.get(module.types, name)
            |> result.map_error(fn(_) { compiler.ReferenceError(name) })
            |> result.map(fn(def) {
              #(module.location, prototype(def.parameters))
            })
          })
      }
      case candidate {
        Ok(#(location, candidate_type)) -> {
          case candidate_type, params {
            GenericType(typ: nested_type, parameters: exp_params), act_params -> {
              list.strict_zip(exp_params, act_params)
              |> result.map_error(fn(_) {
                compiler.TypeArityError(
                  declared_type,
                  list.length(exp_params),
                  list.length(act_params),
                )
              })
              |> result.try(list.try_map(_, fn(param_def) {
                let #(param_name, param_type) = param_def
                resolve_type(data, param_type)
                |> result.map(pair.new(param_name, _))
              }))
              |> result.map(fn(params) {
                case nested_type {
                  ModuleType(_) -> nested_type
                  TypeConstructor(_, _) | FunctionType(_, _) | TupleType(_) ->
                    substitute(nested_type, dict.from_list(params))
                  TypeVariable(_) ->
                    // If the top-level type we find is a TypeVariable then it is a prototype (placeholder)
                    // So we replace it by a reference to that (not-yet-constructed) type
                    TypeConstructor(
                      TypeFromModule(location, name),
                      params |> list.map(pair.second),
                    )
                }
              })
            }
          }
        }
        Error(e) ->
          // TODO: can probably handle via lookup instead
          case name, module_option, params {
            "Int", None, [] -> Ok(TypeConstructor(BuiltInType("Int"), []))
            "Float", None, [] -> Ok(TypeConstructor(BuiltInType("Float"), []))
            "String", None, [] -> Ok(TypeConstructor(BuiltInType("String"), []))
            "List", None, [item_type] ->
              resolve_type(data, item_type)
              |> result.map(fn(resolved) {
                TypeConstructor(BuiltInType("List"), [resolved])
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
        list.try_map(function_parameters, resolve_type(data, _))
      let resolved_return = resolve_type(data, return)
      case resolved_parameters, resolved_return {
        Ok(p), Ok(r) -> Ok(FunctionType(p, r))
        Error(e), _ -> Error(e)
        _, Error(e) -> Error(e)
      }
    }
    glance.VariableType(name) -> Ok(TypeVariable(name))
    glance.HoleType(_) -> {
      todo
    }
  }
}

fn resolve_variant_field(
  data: ModuleInternals,
  parameters: set.Set(String),
  field: glance.Field(glance.Type),
) -> Result(Field(Type), CompilerError) {
  resolve_type(data, field.item)
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
  parameters: set.Set(String),
  variant: glance.Variant,
) -> Result(Variant, CompilerError) {
  list.map(variant.fields, resolve_variant_field(data, parameters, _))
  |> result.all
  |> result.map(Variant(variant.name, _))
}

pub fn resolve_custom_type(
  data: ModuleInternals,
  parsed: glance.Definition(glance.CustomType),
) -> Result(#(String, CustomType), CompilerError) {
  list.map(parsed.definition.variants, resolve_variant(
    data,
    set.from_list(parsed.definition.parameters),
    _,
  ))
  |> result.all
  |> result.map(CustomType(
    opaque_: parsed.definition.opaque_,
    parameters: parsed.definition.parameters,
    variants: _,
  ))
  |> result.map(fn(type_) { #(parsed.definition.name, type_) })
}

fn resolve_type_alias(
  data: ModuleInternals,
  parsed: glance.Definition(glance.TypeAlias),
) -> Result(#(String, GenericType), CompilerError) {
  resolve_type(data, parsed.definition.aliased)
  |> result.map(fn(resolved) {
    #(
      parsed.definition.name,
      GenericType(resolved, parsed.definition.parameters),
    )
  })
}

pub type Prototype {
  Prototype(parameters: List(String))
}

fn prototype_type_alias(
  def: glance.Definition(glance.TypeAlias),
) -> #(String, GenericType) {
  case def {
    glance.Definition(_, glance.TypeAlias(name: name, parameters: params, ..)) -> #(
      name,
      prototype(params),
    )
  }
}

fn prototype_custom_type(
  def: glance.Definition(glance.CustomType),
) -> #(String, GenericType) {
  case def {
    glance.Definition(_, glance.CustomType(name: name, parameters: params, ..)) -> #(
      name,
      prototype(params),
    )
  }
}

// TODO: split into one for aliases and one for custom types
pub fn resolve_types(
  project: Project,
  location: ModuleId,
  imports: Dict(String, ModuleId),
  modules: Dict(ModuleId, Module),
  parsed: glance.Module,
) -> Result(
  #(Dict(String, GenericType), Dict(String, CustomType)),
  CompilerError,
) {
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
    ModuleInternals(project, location, imported_modules, prototypes)

  use aliases <- result.try(
    result.all(list.map(parsed.type_aliases, resolve_type_alias(internals, _))),
  )
  let internals =
    ModuleInternals(
      ..internals,
      types: dict.merge(prototypes, dict.from_list(aliases)),
    )
  use custom_types <- result.map(
    result.all(list.map(parsed.custom_types, resolve_custom_type(internals, _))),
  )

  let custom_types_generic =
    dict.map_values(dict.from_list(custom_types), fn(name, custom_type) {
      GenericType(
        TypeConstructor(
          TypeFromModule(location, name),
          custom_type.parameters |> list.map(TypeVariable(_)),
        ),
        custom_type.parameters,
      )
    })
  // TODO: ensure uniqueness?
  #(
    dict.merge(dict.from_list(aliases), custom_types_generic),
    dict.from_list(custom_types),
  )
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
  //io.debug(parsed)
  //panic
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
  use #(types, custom_types) <- result.map(resolve_types(
    project,
    location,
    imports,
    modules,
    parsed,
  ))
  // TODO: resolve constants and functions
  // TODO: filter to only public parts
  #(Module(location, imports, types, custom_types, dict.new()), modules)
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
