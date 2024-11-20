import compiler.{type CompilerError}
import glance
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/pair
import gleam/result
import gleam/set
import gleam/string
import pprint
import project.{type ModuleId, type Project}

// TODO: next steps?
// - reduce number of expression-types by adding a FunctionId and BuiltInFunction
//    - Tuple constructors
//    - Tuple indexing
//    - List constructors
//    - List patterns
//    - Field access
//    - Unary and binary operators
//    - Bool constructors?
// - complete inference for all expression types
// - constructors for lists: Empty, NonEmpty & results: Ok, Error
// - pattern matching
// - add constructors to a module's functions
// - inference for mutually recursive functions? call graph analysis?
// - constants

pub type BuiltInType {
  NilType
  IntType
  FloatType
  BoolType
  StringType
  ListType
  TupleType(Int)
}

pub type TypeId {
  BuiltInType(BuiltInType)
  TypeFromModule(module: ModuleId, name: String)
}

pub type FieldIndex {
  ByLabel(String)
  ByPosition(Int)
}

pub type BuiltInFunction {
  TupleConstructor(Int)
  //IndexTuple
  EmptyListConstructor
  NonEmptyListConstructor
  AccessField(variant: Option(String), field: FieldIndex)
  NegateInt
  NegateBool
  BinaryOperator(glance.BinaryOperator)
}

pub type FunctionId {
  BuiltInFunction(BuiltInFunction)
  FunctionFromModule(module: ModuleId, name: String)
}

// fully resolved types

pub type CustomType {
  CustomType(parameters: List(String), opaque_: Bool, variants: List(Variant))
}

pub type GenericType {
  GenericType(typ: Type, parameters: List(String))
}

pub type Type {
  TypeConstructor(generic_type: TypeId, assigned_types: List(Type))
  // TODO: make TypeConstructor? (Add a TypeId constructor FunctionType(arity))
  FunctionType(parameters: List(Type), return: Type)
  TypeVariable(name: String)
}

pub const nil_type = TypeConstructor(BuiltInType(NilType), [])

pub const int_type = TypeConstructor(BuiltInType(IntType), [])

pub const float_type = TypeConstructor(BuiltInType(FloatType), [])

pub const bool_type = TypeConstructor(BuiltInType(BoolType), [])

pub const string_type = TypeConstructor(BuiltInType(StringType), [])

pub fn list_type(item_type: Type) {
  TypeConstructor(BuiltInType(ListType), [item_type])
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

pub type TrapKind {
  Panic
  Todo
}

// Expression with types
pub type Expression {
  Int(val: String)
  Float(val: String)
  String(val: String)
  Variable(typ: Type, name: String)
  Trap(typ: Type, kind: TrapKind, detail: Option(Expression))
  // TODO: maybe should have FunctionId type and a BuiltInFunction constructor to grab built-in stuff as functions?
  FunctionReference(typ: Type, id: FunctionId)
  Fn(
    typ: Type,
    argument_names: List(glance.AssignmentName),
    body: List(Statement),
  )
  // TODO: represent as Call of a built-in function?
  FieldAccess(typ: Type, container: Expression, label: String)
  // TODO: consider storing a Type on Call otherwise it may be a pain
  Call(function: Expression, arguments: List(Expression))
}

pub type Statement {
  // TODO: generalize (patterns)
  Assignment(name: String, value: Expression)
  Expression(expression: Expression)
}

pub type Constraint {
  Equal(a: Type, b: Type)
  HasFieldOfType(container_type: Type, field: FieldIndex, field_type: Type)
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

fn init_inference_block_internal(
  list: List(glance.Statement),
  expected_type: Type,
  environment: Dict(String, Type),
  context: Context,
  acc: List(Statement),
) -> Result(#(Context, List(Statement)), CompilerError) {
  case list {
    [] -> {
      // empty blocks have nil type
      Ok(#(add_constraint(context, Equal(expected_type, nil_type)), []))
    }
    [stmt] -> {
      // last statement in the block must produce the block's expected_type
      init_inference_statement(stmt, expected_type, environment, context)
      |> result.map(fn(res) {
        let #(context, _, stmt) = res
        #(context, [stmt, ..acc])
      })
    }
    [stmt, ..tail] -> {
      // other statements in the block expand the environment
      let #(context, variable_type) = fresh_type_variable(context)
      init_inference_statement(stmt, variable_type, environment, context)
      |> result.try(fn(res) {
        let #(context, environment, stmt) = res
        init_inference_block_internal(
          tail,
          expected_type,
          environment,
          context,
          [stmt, ..acc],
        )
      })
    }
  }
}

pub fn init_inference_block(
  list: List(glance.Statement),
  expected_type: Type,
  environment: Dict(String, Type),
  context: Context,
) -> Result(#(Context, List(Statement)), CompilerError) {
  init_inference_block_internal(list, expected_type, environment, context, [])
  |> result.map(pair.map_second(_, list.reverse))
}

pub fn init_inference_statement(
  stmt: glance.Statement,
  expected_type: Type,
  environment: Dict(String, Type),
  context: Context,
) -> Result(#(Context, Dict(String, Type), Statement), CompilerError) {
  case stmt {
    glance.Assignment(
      kind: glance.Let,
      pattern: glance.PatternVariable(name),
      annotation: annotation,
      value: expr,
    ) -> {
      use #(context, assignment_type) <- result.try(resolve_optional_type(
        context,
        annotation,
      ))
      use #(context, expr) <- result.map(init_inference(
        expr,
        assignment_type,
        environment,
        context,
      ))
      #(
        context,
        dict.insert(environment, name, assignment_type),
        Assignment(name, expr),
      )
    }
    glance.Assignment(_, _, _, _) -> todo
    glance.Expression(expr) ->
      init_inference(expr, expected_type, environment, context)
      |> result.map(fn(res) {
        let #(context, expr) = res
        #(context, environment, Expression(expr))
      })
    glance.Use(_, _) -> todo
  }
}

fn init_inference_call(
  function: Expression,
  arg_types: List(Type),
  args: List(glance.Expression),
  environment: Dict(String, Type),
  context: Context,
) -> Result(#(Context, Expression), CompilerError) {
  list.zip(args, arg_types)
  |> list.try_fold(from: #(context, []), with: fn(acc_pair, arg_pair) {
    let #(context, acc) = acc_pair
    let #(arg, arg_type) = arg_pair
    init_inference(arg, arg_type, environment, context)
    |> result.map(fn(res) {
      let #(context, expr) = res
      #(context, [expr, ..acc])
    })
  })
  |> result.map(fn(res) {
    let #(context, args) = res
    #(context, Call(function, list.reverse(args)))
  })
}

fn init_inference_call_builtin(
  built_in_function: BuiltInFunction,
  args: List(glance.Expression),
  expected_type: Type,
  environment: Dict(String, Type),
  context: Context,
) {
  let #(context, arg_types, return_type) = case built_in_function {
    BinaryOperator(operator) -> {
      let #(operand_type, result_type) = case operator {
        glance.And | glance.Or -> #(bool_type, bool_type)
        glance.Eq | glance.NotEq -> #(TypeVariable("a"), bool_type)
        glance.LtInt | glance.LtEqInt | glance.GtEqInt | glance.GtInt -> #(
          int_type,
          bool_type,
        )
        glance.LtFloat | glance.LtEqFloat | glance.GtEqFloat | glance.GtFloat -> #(
          float_type,
          bool_type,
        )
        glance.AddInt
        | glance.SubInt
        | glance.MultInt
        | glance.DivInt
        | glance.RemainderInt -> #(int_type, int_type)
        glance.AddFloat | glance.SubFloat | glance.MultFloat | glance.DivFloat -> #(
          float_type,
          float_type,
        )
        glance.Concatenate -> #(string_type, string_type)
        glance.Pipe -> panic
      }
      #(context, [operand_type, operand_type], result_type)
    }
    NegateBool -> #(context, [bool_type], bool_type)
    NegateInt -> #(context, [int_type], int_type)
    TupleConstructor(arity) -> {
      let #(context, types) =
        list.range(1, arity)
        |> list.map_fold(context, fn(context, _) {
          fresh_type_variable(context)
        })
      #(context, types, TypeConstructor(BuiltInType(TupleType(arity)), types))
    }
    AccessField(_, index) -> {
      let #(context, custom_type) = fresh_type_variable(context)
      #(
        add_constraint(
          context,
          HasFieldOfType(custom_type, index, expected_type),
        ),
        [custom_type],
        expected_type,
      )
    }
    EmptyListConstructor -> {
      let #(context, item_type) = fresh_type_variable(context)
      #(
        add_constraint(context, Equal(expected_type, list_type(item_type))),
        [],
        expected_type,
      )
    }
    NonEmptyListConstructor -> {
      let #(context, item_type) = fresh_type_variable(context)
      #(
        add_constraint(context, Equal(expected_type, list_type(item_type))),
        [item_type, list_type(item_type)],
        expected_type,
      )
    }
  }
  let function_type = FunctionType(arg_types, return_type)
  let function =
    FunctionReference(function_type, BuiltInFunction(built_in_function))
  init_inference_call(function, arg_types, args, environment, context)
  |> result.map(fn(res) {
    let #(context, expr) = res
    #(add_constraint(context, Equal(expected_type, return_type)), expr)
  })
}

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
    glance.Float(value) -> {
      Ok(#(
        add_constraint(context, Equal(expected_type, float_type)),
        Float(value),
      ))
    }
    glance.String(value) -> {
      Ok(#(
        add_constraint(context, Equal(expected_type, string_type)),
        String(value),
      ))
    }
    glance.Variable(name) -> {
      use <- result.lazy_or(
        dict.get(environment, name)
        |> result.map_error(fn(_) { compiler.ReferenceError(name) })
        |> result.map(fn(variable_type) {
          #(
            add_constraint(context, Equal(expected_type, variable_type)),
            Variable(variable_type, name),
          )
        }),
      )
      dict.get(context.internals.functions, name)
      |> result.map_error(fn(_) { compiler.ReferenceError(name) })
      |> result.map(fn(function_type) {
        #(
          add_constraint(context, Equal(expected_type, function_type)),
          FunctionReference(
            function_type,
            FunctionFromModule(context.internals.location, name),
          ),
        )
      })
    }
    glance.NegateInt(expr) ->
      init_inference_call_builtin(
        NegateInt,
        [expr],
        expected_type,
        environment,
        context,
      )
    glance.NegateBool(expr) ->
      init_inference_call_builtin(
        NegateBool,
        [expr],
        expected_type,
        environment,
        context,
      )
    glance.Block(body) -> {
      use #(context, body) <- result.map(init_inference_block(
        body,
        expected_type,
        environment,
        context,
      ))
      #(context, Call(Fn(FunctionType([], expected_type), [], body), []))
    }
    glance.Panic(maybe_expr) | glance.Todo(maybe_expr) -> {
      let construct = case expr {
        glance.Panic(_) -> Trap(expected_type, Panic, _)
        _ -> Trap(expected_type, Todo, _)
      }
      case maybe_expr {
        None -> Ok(#(context, construct(None)))
        Some(expr) ->
          init_inference(expr, string_type, environment, context)
          |> result.map(fn(res) {
            let #(context, expr) = res
            #(context, construct(Some(expr)))
          })
      }
    }
    glance.Tuple(args) -> {
      init_inference_call_builtin(
        TupleConstructor(list.length(args)),
        args,
        expected_type,
        environment,
        context,
      )
    }
    glance.List(items, rest) -> {
      case items, rest {
        [], None ->
          init_inference_call_builtin(
            EmptyListConstructor,
            [],
            expected_type,
            environment,
            context,
          )
        [], Some(tail_expr) -> {
          let #(context, item_type) = fresh_type_variable(context)
          init_inference(tail_expr, list_type(item_type), environment, context)
        }
        [item, ..more], _ ->
          init_inference_call_builtin(
            NonEmptyListConstructor,
            [item, glance.List(more, rest)],
            expected_type,
            environment,
            context,
          )
      }
    }
    glance.Fn(args, return, body) -> {
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
      use #(context, body) <- result.map(init_inference_block(
        body,
        return_type,
        environment,
        context,
      ))
      let fn_type = FunctionType(param_types, return_type)
      #(
        add_constraint(context, Equal(expected_type, fn_type)),
        Fn(fn_type, list.map(args, fn(arg) { arg.name }), body),
      )
    }
    glance.RecordUpdate(maybe_module, constructor, record, fields) -> {
      // get the constructors and custom types for the module
      use #(functions, custom_types) <- result.try(
        case maybe_module {
          Some(module_name) -> {
            dict.get(context.internals.imports, module_name)
            |> result.map(fn(module) {
              #(module.functions, module.custom_types)
            })
          }
          None ->
            Ok(#(context.internals.functions, context.internals.custom_types))
        }
        |> result.map_error(fn(_) { compiler.AnotherTypeError }),
      )
      // identify the custom type and variant used
      use #(module_id, custom_type, variant) <- result.try(case
        dict.get(functions, constructor)
      {
        Ok(FunctionType(
          _params,
          TypeConstructor(TypeFromModule(module_id, type_name), _) as return_type,
        )) -> {
          dict.get(custom_types, type_name)
          |> result.try(fn(custom_type) {
            list.find(custom_type.variants, fn(variant) {
              variant.name == constructor
            })
            |> result.map(fn(variant) { #(module_id, return_type, variant) })
          })
          |> result.map_error(fn(_) { compiler.AnotherTypeError })
        }
        _ -> Error(compiler.AnotherTypeError)
      })
      // TODO: check all fields that are attempted to be set, exist
      // TODO: check that the variant has any record fields
      //
      // map variant fields to either how they are set in the record update, or their existing value
      use #(context, args) <- result.try(
        list.index_map(variant.fields, fn(field, index) { #(index, field) })
        |> list.try_fold(from: #(context, []), with: fn(acc, pair) {
          let #(context, exprs) = acc
          let #(index, field) = pair
          case field {
            Field(Some(label), field_type) ->
              case list.find(fields, fn(pair) { pair.0 == label }) {
                Ok(#(_, expr)) ->
                  init_inference(expr, field_type, environment, context)
                Error(Nil) ->
                  // TODO: generate None for common fields?
                  init_inference_call_builtin(
                    AccessField(Some(variant.name), ByLabel(label)),
                    [record],
                    field_type,
                    environment,
                    context,
                  )
              }
            Field(None, field_type) ->
              init_inference_call_builtin(
                AccessField(Some(variant.name), ByPosition(index)),
                [record],
                field_type,
                environment,
                context,
              )
          }
          |> result.map(fn(res) {
            let #(context, expr) = res
            #(context, [expr, ..exprs])
          })
        }),
      )
      let args = list.reverse(args)
      // now call the constructor
      dict.get(functions, variant.name)
      |> result.map_error(fn(_) { compiler.AnotherTypeError })
      |> result.map(fn(constructor_type) {
        #(
          add_constraint(context, Equal(expected_type, custom_type)),
          Call(
            FunctionReference(
              constructor_type,
              FunctionFromModule(module_id, variant.name),
            ),
            args,
          ),
        )
      })
    }
    glance.FieldAccess(container, label) -> {
      // short-cut module field access if possible
      case container {
        glance.Variable(name) -> {
          Ok(name)
        }
        _ -> Error(Nil)
      }
      |> result.try(dict.get(context.internals.imports, _))
      |> result.try(fn(module) {
        dict.get(module.functions, label)
        |> result.map(fn(function_type) {
          #(
            add_constraint(context, Equal(expected_type, function_type)),
            FunctionReference(
              function_type,
              FunctionFromModule(module.location, label),
            ),
          )
        })
      })
      |> result.map_error(fn(_) { compiler.AnotherTypeError })
      |> result.lazy_or(fn() {
        init_inference_call_builtin(
          AccessField(None, ByLabel(label)),
          [container],
          expected_type,
          environment,
          context,
        )
      })
    }
    glance.Call(function, args) -> {
      let args = list.map(args, fn(arg) { arg.item })
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
      init_inference_call(function, arg_types, args, environment, context)
    }
    glance.TupleIndex(_, _) -> todo
    glance.FnCapture(_, _, _, _) -> todo
    glance.BitString(_) -> todo
    glance.Case(_, _) -> todo
    glance.BinaryOperator(glance.Pipe, left, right) ->
      case right {
        glance.Call(function, args) ->
          init_inference(
            glance.Call(function, [glance.Field(None, left), ..args]),
            expected_type,
            environment,
            context,
          )
        glance.Variable(_) ->
          init_inference(
            glance.Call(right, [glance.Field(None, left)]),
            expected_type,
            environment,
            context,
          )
        glance.Fn(args, return_annotation, body) -> todo
        _ -> todo
      }
    glance.BinaryOperator(operator, left, right) -> {
      init_inference_call_builtin(
        BinaryOperator(operator),
        [left, right],
        expected_type,
        environment,
        context,
      )
    }
  }
}

pub fn get_sub(context: Context, t: Type) -> Option(Type) {
  case t {
    TypeVariable(i) ->
      dict.get(context.substitution, i)
      |> result.map(fn(r) {
        // TODO: worry about infinite recursion?
        case r {
          TypeVariable(j) if i != j -> get_sub(context, r)
          _ -> Some(r)
        }
      })
      |> result.unwrap(None)
    _ -> None
  }
}

fn occurs_in(var: String, t: Type) -> Bool {
  case t {
    TypeVariable(i) -> var == i
    FunctionType(args, ret) -> list.any([ret, ..args], occurs_in(var, _))
    TypeConstructor(_, sub) -> list.any(sub, occurs_in(var, _))
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
    TypeConstructor(id_i, args_i), TypeConstructor(id_j, args_j) -> {
      case id_i == id_j && list.length(args_i) == list.length(args_j) {
        False -> Error(compiler.AnotherTypeError)
        True -> {
          list.zip(args_i, args_j)
          |> list.try_fold(from: context, with: fn(context, pair) {
            unify(context, pair.0, pair.1)
          })
        }
      }
    }
    FunctionType(_, _), TypeConstructor(_, _)
    | TypeConstructor(_, _), FunctionType(_, _)
    -> Error(compiler.AnotherTypeError)
  }
}

pub fn solve_constraints(
  context: Context,
) -> Result(Dict(String, Type), CompilerError) {
  // TODO: rather than sort, probably need an iterative / recursive solve
  list.sort(context.constraints, fn(a, b) {
    case a, b {
      Equal(_, _), HasFieldOfType(_, _, _) -> order.Lt
      Equal(_, _), Equal(_, _) -> order.Eq
      HasFieldOfType(_, _, _), HasFieldOfType(_, _, _) -> order.Eq
      HasFieldOfType(_, _, _), Equal(_, _) -> order.Gt
    }
  })
  |> list.try_fold(from: context, with: fn(context, constraint) {
    case constraint {
      Equal(a, b) -> unify(context, a, b)
      HasFieldOfType(container_type, ByLabel(field_name), field_type) -> {
        use #(module_id, type_name) <- result.try(case container_type {
          TypeConstructor(TypeFromModule(module_id, type_name), _params) ->
            Ok(#(module_id, type_name))
          FunctionType(_, _) -> Error(compiler.AnotherTypeError)
          TypeConstructor(BuiltInType(_), _) -> Error(compiler.AnotherTypeError)
          TypeVariable(_) ->
            case get_sub(context, container_type) {
              None -> Error(compiler.AnotherTypeError)
              Some(TypeConstructor(
                TypeFromModule(module_id, type_name),
                _params,
              )) -> Ok(#(module_id, type_name))
              Some(thing) -> {
                io.debug("Got some unexpected container type")
                io.debug(thing)
                pprint.debug(context.constraints)
                Error(compiler.AnotherTypeError)
              }
            }
        })
        // TODO: function for looking up custom types
        use custom_type <- result.try(
          dict.values(context.internals.imports)
          |> list.find(fn(module) { module.location == module_id })
          |> result.map(fn(module) { module.custom_types })
          |> result.lazy_or(fn() {
            case context.internals.location == module_id {
              False -> Error(Nil)
              True -> Ok(context.internals.custom_types)
            }
          })
          |> result.try(dict.get(_, type_name))
          |> result.map_error(fn(_) {
            io.debug("Could not find type")
            compiler.AnotherTypeError
          }),
        )
        // TODO: add fields as an attribute of custom types
        list.try_map(custom_type.variants, fn(variant) {
          list.find(variant.fields, fn(field) {
            case field {
              Field(label: Some(label), ..) -> label == field_name
              _ -> False
            }
          })
        })
        |> result.map_error(fn(_) {
          io.debug("Not all variants have field")
          compiler.AnotherTypeError
        })
        |> result.try(fn(fields) {
          let types =
            list.map(fields, fn(field) { field.item })
            |> list.unique
          case types {
            [t] -> Ok(t)
            _ -> {
              io.debug("Field type not consistent across variants")
              Error(compiler.AnotherTypeError)
            }
          }
        })
        |> result.try(unify(context, _, field_type))
      }
      HasFieldOfType(_, ByPosition(_), _) -> todo
    }
  })
  |> result.map(fn(context) { context.substitution })
}

pub fn substitute(t: Type, subs: Dict(String, Type)) -> Type {
  case t {
    FunctionType(params, return) ->
      FunctionType(
        list.map(params, substitute(_, subs)),
        substitute(return, subs),
      )
    TypeConstructor(id, assigned) ->
      TypeConstructor(id, list.map(assigned, substitute(_, subs)))
    TypeVariable(name) ->
      dict.get(subs, name)
      |> result.unwrap(t)
      |> fn(type_) {
        // TODO: recurse here or improve solve_constraints to eliminate the need?
        case type_ {
          TypeVariable(other_name) if other_name == name -> type_
          _ -> substitute(type_, subs)
        }
      }
  }
}

pub fn substitute_statement_list(
  list: List(Statement),
  subs: Dict(String, Type),
) -> List(Statement) {
  list.map(list, substitute_statement(_, subs))
}

pub fn substitute_statement(
  stmt: Statement,
  subs: Dict(String, Type),
) -> Statement {
  case stmt {
    Expression(expr) -> Expression(substitute_expression(expr, subs))
    Assignment(name, expr) ->
      Assignment(name, substitute_expression(expr, subs))
  }
}

pub fn substitute_expression(
  expr: Expression,
  subs: Dict(String, Type),
) -> Expression {
  case expr {
    Int(_) | Float(_) | String(_) -> expr
    Fn(t, param_names, body) ->
      Fn(
        substitute(t, subs),
        param_names,
        substitute_statement_list(body, subs),
      )
    Variable(t, name) -> Variable(substitute(t, subs), name)
    Call(function, args) ->
      Call(
        substitute_expression(function, subs),
        list.map(args, substitute_expression(_, subs)),
      )
    FieldAccess(_, _, _) -> todo
    FunctionReference(t, id) -> FunctionReference(substitute(t, subs), id)
    Trap(t, kind, maybe_expr) ->
      Trap(
        substitute(t, subs),
        kind,
        option.map(maybe_expr, substitute_expression(_, subs)),
      )
  }
}

pub fn infer(
  expr: glance.Expression,
  data: ModuleInternals,
) -> Result(Expression, CompilerError) {
  init_inference(
    expr,
    TypeVariable("$1"),
    dict.new(),
    Context(data, dict.from_list([#("$1", TypeVariable("$1"))]), [], 2),
  )
  |> result.try(fn(initd) {
    let #(context, initd_fn) = initd
    solve_constraints(context)
    |> result.map(substitute_expression(initd_fn, _))
  })
}

pub fn infer_function(
  data: ModuleInternals,
  def: glance.Definition(glance.Function),
) -> Result(#(String, Expression), CompilerError) {
  infer(
    glance.Fn(
      def.definition.parameters
        |> list.map(fn(p) { glance.FnParameter(p.name, p.type_) }),
      def.definition.return,
      def.definition.body,
    ),
    data,
  )
  |> result.map(fn(expr) { #(def.definition.name, expr) })
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
    custom_types: Dict(String, CustomType),
    functions: Dict(String, Type),
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
    TypeConstructor(_, assigned_types) -> union_type_variables(assigned_types)
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
                  TypeConstructor(_, _) | FunctionType(_, _) ->
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
            "Int", None, [] -> Ok(int_type)
            "Float", None, [] -> Ok(float_type)
            "String", None, [] -> Ok(string_type)
            "List", None, [item_type] ->
              resolve_type(data, item_type)
              |> result.map(list_type)
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
    ModuleInternals(
      project: project,
      location: location,
      imports: imported_modules,
      types: prototypes,
      custom_types: dict.new(),
      functions: dict.new(),
    )

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
  use #(types, custom_types) <- result.try(resolve_types(
    project,
    location,
    imports,
    modules,
    parsed,
  ))
  // TODO: resolve constants
  // TODO: fix duplication of this with resolve_types
  let imported_modules =
    imports
    |> dict.map_values(fn(_, loc) {
      let assert Ok(module) = dict.get(modules, loc)
      module
    })
  let internals =
    ModuleInternals(
      project: project,
      location: location,
      imports: imported_modules,
      types: types,
      custom_types: dict.new(),
      functions: dict.new(),
    )
  // TODO: hack!
  use functions <- result.map(
    parsed.functions
    |> list.try_map(infer_function(internals, _))
    |> result.map(list.map(_, fn(fndef) {
      let assert #(name, Fn(typ: t, ..)) = fndef
      #(name, t)
    }))
    |> result.map(dict.from_list),
  )

  // TODO: filter to only public parts
  #(Module(location, imports, types, custom_types, functions), modules)
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
      project.SourceLocation(project.name, "example"),
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
