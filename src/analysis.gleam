import compiler.{type CompilerError}
import glance
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import gleam/string
import graph
import pprint
import project.{type ModuleId, type Project}

// TODO: next steps?
// - constructors that have the same name as their type?
// - unqualified imports
// - identify which variables a closure captures from its environment
// - constants
// - complete inference for all expression types

fn try_map_fold(
  over list: List(a),
  from initial: b,
  with fun: fn(b, a) -> Result(#(b, c), e),
) -> Result(#(b, List(c)), e) {
  list.try_fold(over: list, from: #(initial, []), with: fn(acc_pair, arg) {
    let #(context, acc) = acc_pair
    fun(context, arg)
    |> result.map(fn(res) {
      let #(context, data) = res
      #(context, [data, ..acc])
    })
  })
  |> result.map(pair.map_second(_, list.reverse))
}

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
  EmptyListConstructor
  NonEmptyListConstructor
  BoolConstructor(Bool)
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
  CustomType(
    parameters: List(String),
    opaque_: Bool,
    variants: List(FunctionSignature),
  )
}

pub type GenericType {
  GenericType(typ: Type, parameters: List(String))
}

pub type Labeled(a) {
  Labeled(label: String, item: a)
}

// TODO: should type parameters be part of the signature?
pub type FunctionSignature {
  FunctionSignature(
    name: String,
    positional_parameters: List(Type),
    labeled_parameters: List(Labeled(Type)),
    return: Type,
  )
}

pub type FunctionBody {
  GleamBody(List(Statement))
  External(path: String, name: String)
}

pub type Function {
  Function(
    signature: FunctionSignature,
    argument_names: List(glance.AssignmentName),
    body: FunctionBody,
  )
}

pub type Type {
  TypeConstructor(generic_type: TypeId, assigned_types: List(Type))
  // TODO: make TypeConstructor? (Add a TypeId constructor FunctionType(arity))
  FunctionType(parameters: List(Type), return: Type)
  TypeVariable(name: String)
}

fn flatten_params(signature: FunctionSignature) -> List(Type) {
  let FunctionSignature(_name, pos, lab, _ret) = signature
  list.append(pos, list.map(lab, fn(l) { l.item }))
}

pub fn signature_type(signature: FunctionSignature) -> Type {
  let FunctionSignature(_name, _pos, _lab, ret) = signature
  FunctionType(flatten_params(signature), ret)
}

pub const nil_type = TypeConstructor(BuiltInType(NilType), [])

pub const int_type = TypeConstructor(BuiltInType(IntType), [])

pub const float_type = TypeConstructor(BuiltInType(FloatType), [])

pub const bool_type = TypeConstructor(BuiltInType(BoolType), [])

pub const string_type = TypeConstructor(BuiltInType(StringType), [])

pub fn list_type(item_type: Type) {
  TypeConstructor(BuiltInType(ListType), [item_type])
}

pub const gleam_module_id = project.SourceLocation("gleam", "gleam")

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
    functions: Dict(String, FunctionSignature),
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

pub type Clause {
  Clause(
    patterns: List(List(Pattern)),
    variables: Dict(String, Type),
    guard: Expression,
    body: Expression,
  )
}

// Expression with types
pub type Expression {
  // TODO: merge literals into one?
  Int(val: String)
  Float(val: String)
  String(val: String)
  Variable(typ: Type, name: String)
  Trap(typ: Type, kind: TrapKind, detail: Option(Expression))
  FunctionReference(typ: Type, id: FunctionId)
  Fn(
    typ: Type,
    argument_names: List(glance.AssignmentName),
    body: List(Statement),
  )
  // TODO: consider storing a Type on Call otherwise it may be a pain
  Call(function: Expression, arguments: List(Expression))
  Case(typ: Type, subjects: List(Expression), clauses: List(Clause))
}

pub type PatternList {
  PatternEmpty
  PatternTail(Pattern)
  PatternNonEmpty(head: Pattern, tail: PatternList)
}

/// Patterns used in assignments and case expressions
pub type Pattern {
  PatternInt(value: String)
  PatternFloat(value: String)
  PatternString(value: String)
  PatternDiscard(name: String)
  PatternVariable(name: String)
  PatternTuple(elems: List(Pattern))
  PatternList(PatternList)
  PatternAssignment(pattern: Pattern, name: String)
  PatternConcatenate(left: String, right: Pattern)
  /// PatternBitString not implemented yet
  PatternBitString
  /// Variant constructor. Normalized to use positional arguments. Any unspecified fields are represented by `PatternDiscard("")`.
  PatternConstructor(
    custom_type: TypeId,
    variant: FunctionSignature,
    arguments: List(Pattern),
  )
}

pub type PatternWithVariables {
  PatternWithVariables(pattern: Pattern, variables: Dict(String, Type))
}

pub type Statement {
  Assignment(
    kind: glance.AssignmentKind,
    pattern: PatternWithVariables,
    value: Expression,
  )
  Expression(expression: Expression)
}

pub type Constraint {
  Equal(a: Type, b: Type)
  HasFieldOfType(
    container_type: Type,
    variant: Option(String),
    field: FieldIndex,
    field_type: Type,
  )
}

pub type Context {
  Context(
    internals: ModuleInternals,
    substitution: Dict(String, Type),
    constraints: List(Constraint),
    next_variable: Int,
    next_generated_name: Int,
  )
}

pub fn new_context(internals: ModuleInternals) -> Context {
  Context(
    internals:,
    substitution: dict.new(),
    constraints: [],
    next_variable: 1,
    next_generated_name: 1,
  )
}

pub fn fresh_type_variable(context: Context) -> #(Context, Type) {
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

fn fresh_generated_name(context: Context) -> #(Context, glance.AssignmentName) {
  #(
    Context(..context, next_generated_name: context.next_generated_name + 1),
    glance.Named("$" <> int.to_string(context.next_generated_name)),
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
  case list, acc {
    [], [] -> {
      // empty blocks have nil type
      Ok(#(add_constraint(context, Equal(expected_type, nil_type)), []))
    }
    [], _ -> {
      Ok(#(context, acc))
    }
    [stmt, ..rest], _ -> {
      // each statement expands the environment, and use statements can eat the
      // rest of the block
      init_inference_statement(stmt, rest, expected_type, environment, context)
      |> result.try(fn(res) {
        let #(rest, context, environment, stmt) = res
        init_inference_block_internal(
          rest,
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

fn is_positional_param(arg: glance.Field(a)) -> Bool {
  option.is_none(arg.label)
}

pub fn pair_args(
  expected signature: FunctionSignature,
  actual args: List(glance.Field(b)),
  at location: compiler.ErrorLocation,
) -> Result(List(#(Type, Option(b))), CompilerError) {
  // Number of arguments provided may not exceed the expected
  let n_params =
    list.length(signature.positional_parameters)
    + list.length(signature.labeled_parameters)
  use <- bool.guard(
    n_params < list.length(args),
    Error(compiler.ArityError(location, n_params, list.length(args))),
  )
  let #(pos_args, label_args) = list.split_while(args, is_positional_param)
  // Positional arguments are not allowed after labeled arguments
  use <- bool.guard(
    list.any(label_args, is_positional_param),
    Error(compiler.PositionalArgsAfterLabeledArgsError(location)),
  )
  let params =
    list.append(
      signature.positional_parameters |> list.map(Field(None, _)),
      signature.labeled_parameters
        |> list.map(fn(e) { Field(Some(e.label), e.item) }),
    )
  let #(pos_params, label_params) = list.split(params, list.length(pos_args))
  // TODO: check labeled arguments are unique
  //
  // Actual labeled arguments must be expected
  let unexpected_arg =
    set.difference(
      list.map(label_args, fn(field) { field.label }) |> set.from_list,
      list.map(label_params, fn(field) { field.label }) |> set.from_list,
    )
    |> set.to_list
    |> list.first
  use _ <- result.try(case unexpected_arg {
    Ok(Some(arg)) -> Error(compiler.NoSuchFieldError(location, arg))
    Ok(None) -> Error(compiler.NoSuchFieldError(location, ""))
    Error(Nil) -> Ok(Nil)
  })
  // Find value provided for labeled arguments (if provided)
  let label_pairs =
    list.map(label_params, fn(param) {
      case list.find(label_args, fn(arg) { arg.label == param.label }) {
        Ok(arg) -> #(param, Some(arg))
        Error(Nil) -> #(param, None)
      }
    })
  // Pair up positional arguments
  let pos_pairs = list.zip(pos_params, pos_args |> list.map(Some))
  // Map all pairs to result format 
  list.append(pos_pairs, label_pairs)
  |> list.map(fn(pair) {
    case pair {
      #(Field(item: a, ..), Some(glance.Field(item: b, ..))) -> #(a, Some(b))
      #(Field(item: a, ..), None) -> #(a, None)
    }
  })
  |> Ok
}

/// Treat the `ModuleInternals` as a `Module`.
/// Enables uniform lookup of type and function data.
fn internals_as_module(internals: ModuleInternals) -> Module {
  Module(
    internals.location,
    dict.map_values(internals.imports, fn(_, m) { m.location }),
    internals.types,
    internals.custom_types,
    internals.functions,
  )
}

/// Convert `ModuleInternals` to a `Module`.
/// Removes reference to any private types and functions.
fn module_from_internals(internals: ModuleInternals) -> Module {
  Module(
    internals.location,
    dict.map_values(internals.imports, fn(_, m) { m.location }),
    dict.filter(internals.types, fn(k, _v) {
      set.contains(internals.public_types, k)
    }),
    dict.filter(internals.custom_types, fn(k, _v) {
      set.contains(internals.public_types, k)
    }),
    dict.filter(internals.functions, fn(k, _v) {
      set.contains(internals.public_functions, k)
    }),
  )
}

fn find_module_by_name(
  internals: ModuleInternals,
  maybe_module_name: Option(String),
) -> Result(Module, compiler.CompilerError) {
  case maybe_module_name {
    None -> Ok(internals_as_module(internals))
    Some(module_name) -> lookup(internals.imports, module_name)
  }
}

fn find_module_by_id(
  internals: ModuleInternals,
  module_id: ModuleId,
) -> Result(Module, compiler.CompilerError) {
  dict.values(internals.imports)
  |> list.find(fn(module) { module.location == module_id })
  |> result.lazy_or(fn() {
    case internals.location == module_id {
      False -> Error(Nil)
      True -> Ok(internals_as_module(internals))
    }
  })
  |> result.replace_error(compiler.AnotherTypeError("Could not find module"))
}

fn lookup(
  dict: Dict(String, b),
  key: String,
) -> Result(b, compiler.CompilerError) {
  dict.get(dict, key) |> result.replace_error(compiler.ReferenceError(key))
}

fn unzip_patterns_with_variables(
  patterns: List(PatternWithVariables),
) -> #(List(Pattern), Dict(String, Type)) {
  #(
    patterns |> list.map(fn(pattern) { pattern.pattern }),
    patterns
      |> list.fold(from: dict.new(), with: fn(variables, pattern) {
        dict.merge(variables, pattern.variables)
      }),
  )
}

fn init_inference_pattern_list(
  items: List(glance.Pattern),
  rest: Option(glance.Pattern),
  expected_type: Type,
  environment: Dict(String, Type),
  context: Context,
) -> Result(#(Context, PatternList, Dict(String, Type)), CompilerError) {
  let #(context, item_type) = fresh_type_variable(context)
  case items, rest {
    [], None ->
      Ok(#(
        add_constraint(context, Equal(expected_type, list_type(item_type))),
        PatternEmpty,
        dict.new(),
      ))
    [], Some(tail_pattern) -> {
      use #(context, pattern) <- result.map(init_inference_pattern(
        tail_pattern,
        list_type(item_type),
        environment,
        context,
      ))
      #(
        add_constraint(context, Equal(expected_type, list_type(item_type))),
        PatternTail(pattern.pattern),
        pattern.variables,
      )
    }
    [item], None -> {
      use #(context, item_pattern) <- result.map(init_inference_pattern(
        item,
        item_type,
        environment,
        context,
      ))
      #(
        add_constraint(context, Equal(expected_type, list_type(item_type))),
        PatternNonEmpty(item_pattern.pattern, PatternEmpty),
        item_pattern.variables,
      )
    }
    [item, ..more], _ -> {
      use #(context, item_pattern) <- result.try(init_inference_pattern(
        item,
        item_type,
        environment,
        context,
      ))
      use #(context, more_pattern, more_variables) <- result.map(
        init_inference_pattern_list(
          more,
          rest,
          list_type(item_type),
          environment,
          context,
        ),
      )
      #(
        add_constraint(context, Equal(expected_type, list_type(item_type))),
        PatternNonEmpty(item_pattern.pattern, more_pattern),
        dict.merge(item_pattern.variables, more_variables),
      )
    }
  }
}

pub fn init_inference_pattern(
  pattern: glance.Pattern,
  expected_type: Type,
  environment: Dict(String, Type),
  context: Context,
) -> Result(#(Context, PatternWithVariables), CompilerError) {
  case pattern {
    glance.PatternInt(value) ->
      Ok(#(
        add_constraint(context, Equal(expected_type, int_type)),
        PatternWithVariables(PatternInt(value), dict.new()),
      ))
    glance.PatternFloat(value) ->
      Ok(#(
        add_constraint(context, Equal(expected_type, float_type)),
        PatternWithVariables(PatternFloat(value), dict.new()),
      ))
    glance.PatternString(value) ->
      Ok(#(
        add_constraint(context, Equal(expected_type, string_type)),
        PatternWithVariables(PatternString(value), dict.new()),
      ))
    glance.PatternDiscard(name) -> {
      Ok(#(context, PatternWithVariables(PatternDiscard(name), dict.new())))
    }
    glance.PatternVariable(name) -> {
      Ok(#(
        context,
        PatternWithVariables(
          PatternVariable(name),
          dict.from_list([#(name, expected_type)]),
        ),
      ))
    }
    glance.PatternTuple(elems) -> {
      try_map_fold(over: elems, from: context, with: fn(context, elem) {
        let #(context, elem_type) = fresh_type_variable(context)
        init_inference_pattern(elem, elem_type, environment, context)
        |> result.map(fn(res) {
          let #(context, pattern) = res
          #(context, #(elem_type, pattern))
        })
      })
      |> result.map(fn(res) {
        let #(context, typed_patterns) = res
        let #(types, patterns) = typed_patterns |> list.unzip
        #(
          add_constraint(
            context,
            Equal(
              TypeConstructor(BuiltInType(TupleType(2)), types),
              expected_type,
            ),
          ),
          unzip_patterns_with_variables(patterns)
            |> fn(unzipped) {
              let #(patterns, variables) = unzipped
              PatternWithVariables(PatternTuple(patterns), variables)
            },
        )
      })
    }
    glance.PatternList(items, rest) -> {
      use #(context, list_pattern, variables) <- result.map(
        init_inference_pattern_list(
          items,
          rest,
          expected_type,
          environment,
          context,
        ),
      )
      #(context, PatternWithVariables(PatternList(list_pattern), variables))
    }
    glance.PatternAssignment(pattern, name) -> {
      init_inference_pattern(pattern, expected_type, environment, context)
      |> result.map(fn(res) {
        let #(context, pattern) = res
        #(
          context,
          PatternWithVariables(
            PatternAssignment(pattern.pattern, name),
            dict.insert(pattern.variables, name, expected_type),
          ),
        )
      })
    }
    glance.PatternConcatenate(left, right) -> {
      Ok(#(
        context,
        PatternWithVariables(
          PatternConcatenate(left, case right {
            glance.Named(name) -> PatternVariable(name)
            glance.Discarded(name) -> PatternDiscard(name)
          }),
          case right {
            glance.Named(name) -> dict.from_list([#(name, string_type)])
            _ -> dict.new()
          },
        ),
      ))
    }
    glance.PatternBitString(_) -> todo
    glance.PatternConstructor(
      maybe_module_name,
      constructor_name,
      args,
      with_spread,
    ) -> {
      // TODO: support unqualified constructor imports & aliased constructors
      //
      // resolve the module
      use module <- result.try(find_module_by_name(
        context.internals,
        maybe_module_name,
      ))
      // find the constructor & related types
      use #(context, type_id, signature, constructed_type) <- result.try(case
        dict.get(module.functions, constructor_name)
      {
        Ok(signature) -> {
          case signature_replace_free_type_variables(signature, context) {
            #(
              context,
              FunctionSignature(
                _name,
                _pos_params,
                _lab_params,
                TypeConstructor(type_id, _type_params) as constructed_type,
              ) as signature,
            ) -> {
              Ok(#(context, type_id, signature, constructed_type))
            }
            _ -> Error(compiler.AnotherTypeError("Unexpected constructor type"))
          }
        }
        Error(Nil) -> Error(compiler.ReferenceError(constructor_name))
      })
      // pair up arguments
      use arg_pairs <- result.try(pair_args(
        expected: signature,
        actual: args,
        at: compiler.AtPattern(pattern),
      ))
      use <- bool.guard(
        when: !with_spread
          && list.any(arg_pairs, fn(pair) { option.is_none(pair.second(pair)) }),
        return: Error(compiler.AnotherTypeError(
          "Constructor pattern without spread must specify all arguments",
        )),
      )
      // TODO: deduplicate with tuple arguments?
      try_map_fold(over: arg_pairs, from: context, with: fn(context, arg_pair) {
        let #(param_type, maybe_pattern) = arg_pair
        let pattern = option.unwrap(maybe_pattern, glance.PatternDiscard(""))
        init_inference_pattern(pattern, param_type, environment, context)
      })
      |> result.map(fn(res) {
        let #(context, patterns) = res
        #(
          add_constraint(context, Equal(expected_type, constructed_type)),
          unzip_patterns_with_variables(patterns)
            |> fn(unzipped) {
              let #(patterns, variables) = unzipped
              PatternWithVariables(
                PatternConstructor(type_id, signature, patterns),
                variables,
              )
            },
        )
      })
    }
  }
}

pub fn init_inference_statement(
  stmt: glance.Statement,
  rest: List(glance.Statement),
  expected_type: Type,
  environment: Dict(String, Type),
  context: Context,
) -> Result(
  #(List(glance.Statement), Context, Dict(String, Type), Statement),
  CompilerError,
) {
  case stmt {
    glance.Assignment(
      kind: glance.Let,
      pattern: pattern,
      annotation: annotation,
      value: expr,
    ) -> {
      use #(context, assignment_type) <- result.try(resolve_optional_type(
        context,
        annotation,
      ))
      use #(context, expr) <- result.try(init_inference(
        expr,
        assignment_type,
        environment,
        context,
      ))
      use #(context, pattern) <- result.map(init_inference_pattern(
        pattern,
        assignment_type,
        environment,
        context,
      ))
      // in tail position, must generate the expected type
      let context = case rest {
        [] -> add_constraint(context, Equal(expected_type, assignment_type))
        _ -> context
      }
      #(
        rest,
        context,
        dict.merge(environment, pattern.variables),
        Assignment(glance.Let, pattern, expr),
      )
    }
    glance.Assignment(_, _, _, _) -> todo
    glance.Expression(expr) -> {
      let #(context, expected_type) = case rest {
        [] -> #(context, expected_type)
        _ -> fresh_type_variable(context)
      }
      init_inference(expr, expected_type, environment, context)
      |> result.map(fn(res) {
        let #(context, expr) = res
        #(rest, context, environment, Expression(expr))
      })
    }
    glance.Use(patterns, function) -> {
      let #(context, args) =
        list.map_fold(patterns, context, fn(context, pattern) {
          case pattern {
            glance.PatternVariable(name) -> #(context, glance.Named(name))
            glance.PatternDiscard(name) -> #(context, glance.Discarded(name))
            _ -> fresh_generated_name(context)
          }
        })
      use #(context, let_statements) <- result.try(
        list.zip(args, patterns)
        |> try_map_fold(from: context, with: fn(context, p) {
          case p {
            #(_, glance.PatternVariable(_)) | #(_, glance.PatternDiscard(_)) ->
              Ok(#(context, None))
            #(glance.Named(assignment_name), pattern) ->
              Ok(#(
                context,
                Some(glance.Assignment(
                  glance.Let,
                  pattern,
                  None,
                  glance.Variable(assignment_name),
                )),
              ))
            _ ->
              Error(compiler.AnotherTypeError(
                "This should not have happened...",
              ))
          }
        })
        |> result.map(pair.map_second(_, option.values)),
      )

      let callback_fn =
        glance.Fn(
          list.map(args, fn(arg) { glance.FnParameter(arg, None) }),
          None,
          list.append(let_statements, rest),
        )

      let #(outer_fn, outer_args) = case function {
        glance.Call(outer_fn, outer_args) -> #(outer_fn, outer_args)
        glance.Variable(_) -> #(function, [])
        _ -> todo
      }

      use #(context, expr) <- result.map(init_inference_call(
        outer_fn,
        outer_args,
        Some(callback_fn),
        stmt,
        expected_type,
        environment,
        context,
      ))

      #([], context, environment, Expression(expr))
    }
  }
}

fn init_inference_call(
  function: glance.Expression,
  args: List(glance.Field(glance.Expression)),
  use_callback: Option(glance.Expression),
  at_stmt: glance.Statement,
  expected_type: Type,
  environment: Dict(String, Type),
  context: Context,
) -> Result(#(Context, Expression), CompilerError) {
  let #(context, arg_types) =
    case use_callback {
      Some(_) -> [Nil, ..list.map(args, fn(_) { Nil })]
      None -> list.map(args, fn(_) { Nil })
    }
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
  use args <- result.try(case function {
    FunctionReference(_, FunctionFromModule(module_id, function_name)) -> {
      // if we're calling a global function, look up the signature for labels
      use signature <- result.try(
        find_module_by_id(context.internals, module_id)
        |> result.try(fn(module) { lookup(module.functions, function_name) }),
      )
      use paired <- result.try(pair_args(
        signature,
        args,
        compiler.AtStatement(at_stmt),
      ))
      try_map_fold(over: paired, from: use_callback, with: fn(cb, p) {
        case p, cb {
          #(_t, Some(a)), _ -> Ok(#(cb, a))
          _, Some(a) -> Ok(#(None, a))
          _, _ -> Error(compiler.AnotherTypeError("Missing argument"))
        }
      })
      |> result.try(fn(res) {
        let #(cb, args) = res
        case cb {
          None -> Ok(args)
          Some(_) ->
            Error(compiler.ArityError(
              compiler.AtStatement(at_stmt),
              list.length(args),
              list.length(args) + 1,
            ))
        }
      })
    }
    _ -> {
      // if we're not calling a global function, treat arguments as positional
      list.try_map(
        case use_callback {
          Some(cb) -> list.append(args, [glance.Field(None, cb)])
          None -> args
        },
        fn(arg) {
          case arg.label {
            Some(_) ->
              Error(compiler.AnotherTypeError("Labeled args not allowed here"))
            None -> Ok(arg.item)
          }
        },
      )
    }
  })
  init_inference_call_internal(function, arg_types, args, environment, context)
}

fn init_inference_call_internal(
  function: Expression,
  arg_types: List(Type),
  args: List(glance.Expression),
  environment: Dict(String, Type),
  context: Context,
) -> Result(#(Context, Expression), CompilerError) {
  // TODO: use strict_zip?
  list.zip(args, arg_types)
  |> try_map_fold(from: context, with: fn(context, arg_pair) {
    let #(arg, arg_type) = arg_pair
    init_inference(arg, arg_type, environment, context)
  })
  |> result.map(pair.map_second(_, Call(function, _)))
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
      let #(context, operand_type, result_type) = case operator {
        glance.And | glance.Or -> #(context, bool_type, bool_type)
        glance.Eq | glance.NotEq -> {
          let #(context, var) = fresh_type_variable(context)
          #(context, var, bool_type)
        }
        glance.LtInt | glance.LtEqInt | glance.GtEqInt | glance.GtInt -> #(
          context,
          int_type,
          bool_type,
        )
        glance.LtFloat | glance.LtEqFloat | glance.GtEqFloat | glance.GtFloat -> #(
          context,
          float_type,
          bool_type,
        )
        glance.AddInt
        | glance.SubInt
        | glance.MultInt
        | glance.DivInt
        | glance.RemainderInt -> #(context, int_type, int_type)
        glance.AddFloat | glance.SubFloat | glance.MultFloat | glance.DivFloat -> #(
          context,
          float_type,
          float_type,
        )
        glance.Concatenate -> #(context, string_type, string_type)
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
    AccessField(maybe_variant, index) -> {
      let #(context, container_type) = fresh_type_variable(context)
      #(
        add_constraint(
          context,
          HasFieldOfType(container_type, maybe_variant, index, expected_type),
        ),
        [container_type],
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
    BoolConstructor(_) -> {
      #(add_constraint(context, Equal(expected_type, bool_type)), [], bool_type)
    }
  }
  let function_type = FunctionType(arg_types, return_type)
  let function =
    FunctionReference(function_type, BuiltInFunction(built_in_function))
  init_inference_call_internal(function, arg_types, args, environment, context)
  |> result.map(fn(res) {
    let #(context, expr) = res
    #(add_constraint(context, Equal(expected_type, return_type)), expr)
  })
}

pub fn init_inference_clause(
  clause: glance.Clause,
  expected_type: Type,
  subjects: List(#(Expression, Type)),
  environment: Dict(String, Type),
  context: Context,
) -> Result(#(Context, Clause), CompilerError) {
  let glance.Clause(alternative_patterns, guard, body) = clause
  // a clause can have one or more alternative sets of patterns
  use #(context, alternative_patterns) <- result.try(
    try_map_fold(alternative_patterns, context, fn(context, patterns) {
      use <- bool.guard(
        list.length(subjects) != list.length(patterns),
        Error(compiler.AnotherTypeError(
          "Number of patterns must match number of subjects",
        )),
      )
      // infer types for each pattern, expecting the type of the corresponding subject
      use #(context, patterns) <- result.map(
        list.zip(subjects, patterns)
        |> try_map_fold(from: context, with: fn(context, pair) {
          let #(#(_subj, subj_type), pattern) = pair
          init_inference_pattern(pattern, subj_type, environment, context)
        }),
      )
      // collect the variables produced by the patterns
      let #(patterns, variables) =
        patterns
        |> list.fold(from: #([], dict.new()), with: fn(acc, pattern) {
          let #(patterns, variables) = acc
          #(
            [pattern.pattern, ..patterns],
            dict.merge(variables, pattern.variables),
          )
        })
        |> pair.map_first(list.reverse)
      #(context, #(patterns, variables))
    }),
  )
  // check all alternative patterns spawn the same variables
  let alternative_variables =
    alternative_patterns |> list.map(fn(patterns) { patterns.1 })
  use variables <- result.try(
    list.first(alternative_variables)
    |> result.replace_error(compiler.AnotherTypeError(
      "There must be at least one set of patterns",
    )),
  )
  use <- bool.guard(
    when: list.any(alternative_variables, fn(vars) { vars != variables }),
    return: Error(compiler.AnotherTypeError(
      "All alternative patterns must set the same variables",
    )),
  )
  let environment = dict.merge(environment, variables)
  // construct the guard expression
  use #(context, guard) <- result.try(case guard {
    None ->
      Ok(#(
        context,
        Call(
          FunctionReference(
            FunctionType([], bool_type),
            BuiltInFunction(BoolConstructor(True)),
          ),
          [],
        ),
      ))
    Some(expr) -> init_inference(expr, bool_type, environment, context)
  })
  // analyze the body
  use #(context, body) <- result.map(init_inference(
    body,
    expected_type,
    environment,
    context,
  ))
  #(
    context,
    Clause(alternative_patterns |> list.map(pair.first), variables, guard, body),
  )
}

pub fn init_inference_function(
  def: glance.Definition(glance.Function),
  expected_type: Type,
  environment: Dict(String, Type),
  context: Context,
) -> Result(#(Context, Function), CompilerError) {
  let exp_target = context.internals.project.target
  let maybe_external =
    list.find_map(def.attributes, fn(attr) {
      case attr {
        glance.Attribute(
          name: "external",
          arguments: [
            glance.Variable(target),
            glance.String(path),
            glance.String(name),
          ],
        )
          if target == exp_target
        -> Ok(External(path, name))
        _ -> Error(Nil)
      }
    })
  case maybe_external {
    Ok(external) ->
      init_inference_external_function(
        def.definition,
        external,
        expected_type,
        context,
      )
    Error(Nil) ->
      init_inference_gleam_function(
        def.definition,
        expected_type,
        environment,
        context,
      )
  }
}

fn init_inference_external_function(
  def: glance.Function,
  external: FunctionBody,
  expected_type: Type,
  context: Context,
) -> Result(#(Context, Function), CompilerError) {
  let types_required =
    compiler.AnotherTypeError("External functions must have type declarations")
  use #(pos_params, label_params) <- result.try(separate_positional_and_labeled(
    def.parameters
    |> list.map(fn(param) { glance.Field(param.label, param.type_) }),
  ))
  use pos_param_types <- result.try(
    pos_params
    |> option.all
    |> option.to_result(types_required)
    |> result.try(fn(l) { list.try_map(l, resolve_type(context.internals, _)) }),
  )
  use label_param_types <- result.try(
    label_params
    |> list.map(fn(field) { field.item })
    |> option.all
    |> option.to_result(types_required)
    |> result.try(fn(l) { list.try_map(l, resolve_type(context.internals, _)) }),
  )
  use return_type <- result.map(
    def.return
    |> option.to_result(types_required)
    |> result.try(resolve_type(context.internals, _)),
  )

  let signature =
    FunctionSignature(
      def.name,
      pos_param_types,
      list.zip(label_params, label_param_types)
        |> list.map(fn(p) { Labeled({ p.0 }.label, p.1) }),
      return_type,
    )

  #(
    add_constraint(context, Equal(expected_type, signature_type(signature))),
    Function(signature, def.parameters |> list.map(param_to_arg_name), external),
  )
}

fn param_to_arg_name(param: glance.FunctionParameter) -> glance.AssignmentName {
  param.name
}

fn fn_param_to_arg_name(param: glance.FnParameter) -> glance.AssignmentName {
  param.name
}

fn init_inference_gleam_function(
  def: glance.Function,
  expected_type: Type,
  environment: Dict(String, Type),
  context: Context,
) -> Result(#(Context, Function), CompilerError) {
  use #(pos_params, label_params) <- result.try(separate_positional_and_labeled(
    def.parameters
    |> list.map(fn(param) { glance.Field(param.label, param.type_) }),
  ))

  use #(context, fun) <- result.map(init_inference(
    glance.Fn(
      def.parameters
        |> list.map(fn(p) { glance.FnParameter(p.name, p.type_) }),
      def.return,
      def.body,
    ),
    expected_type,
    environment,
    context,
  ))
  let assert Fn(FunctionType(param_types, return_type), _names, body) = fun

  let #(pos_param_types, label_param_types) =
    list.split(param_types, list.length(pos_params))
  let fun =
    Function(
      FunctionSignature(
        def.name,
        pos_param_types,
        list.zip(label_params, label_param_types)
          |> list.map(fn(p) { Labeled({ p.0 }.label, p.1) }),
        return_type,
      ),
      def.parameters |> list.map(param_to_arg_name),
      GleamBody(body),
    )
  #(context, fun)
}

pub fn infer_functions(
  internals: ModuleInternals,
  defs: List(glance.Definition(glance.Function)),
) -> Result(List(Function), CompilerError) {
  pprint.debug(list.map(defs, fn(def) { def.definition.name }))
  let context = new_context(internals)
  let #(context, func_types) =
    list.map_fold(defs, context, fn(context, func) {
      fresh_type_variable(context)
      |> pair.map_second(fn(t) { #(func.definition.name, t) })
    })
  let environment = dict.from_list(func_types)
  use #(context, functions) <- result.try(
    list.map(func_types, pair.second)
    |> list.zip(defs)
    |> try_map_fold(from: context, with: fn(context, pair) {
      let #(typ, def) = pair
      init_inference_function(def, typ, environment, context)
    }),
  )
  solve_constraints(context)
  |> result.map(fn(subs) { list.map(functions, substitute_function(_, subs)) })
}

pub fn infer_function(
  internals: ModuleInternals,
  def: glance.Definition(glance.Function),
) -> Result(Function, CompilerError) {
  infer_functions(internals, [def])
  |> result.map(fn(res) {
    let assert Ok(function) = list.first(res)
    function
  })
}

fn maybe_call_no_arg_constructor(
  module_id: ModuleId,
  signature: FunctionSignature,
  expected_type: Type,
  context: Context,
) -> #(Context, Expression) {
  let #(context, function_type) =
    replace_free_type_variables(signature_type(signature), context)
  let func_ref =
    FunctionReference(
      function_type,
      FunctionFromModule(module_id, signature.name),
    )
  case signature.name == string.capitalise(signature.name), function_type {
    True, FunctionType([], return_type) -> #(
      add_constraint(context, Equal(expected_type, return_type)),
      Call(func_ref, []),
    )
    _, _ -> #(
      add_constraint(context, Equal(expected_type, function_type)),
      func_ref,
    )
  }
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
        lookup(environment, name)
        |> result.map(fn(variable_type) {
          #(
            add_constraint(context, Equal(expected_type, variable_type)),
            Variable(variable_type, name),
          )
        }),
      )
      lookup(context.internals.functions, name)
      |> result.map(maybe_call_no_arg_constructor(
        context.internals.location,
        _,
        expected_type,
        context,
      ))
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
        |> try_map_fold(from: context, with: fn(context, param) {
          resolve_optional_type(context, param.type_)
        }),
      )

      // Populate environment with any named arguments
      let environment =
        list.zip(args, param_types)
        |> list.filter(fn(p) {
          case p.0 {
            glance.FnParameter(glance.Named(_), _) -> True
            glance.FnParameter(glance.Discarded(_), _) -> False
          }
        })
        |> list.map(pair.map_first(_, fn(p) {
          assignment_name_to_string(p.name)
        }))
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
        Fn(fn_type, list.map(args, fn_param_to_arg_name), body),
      )
    }
    glance.RecordUpdate(maybe_module, constructor_name, record, fields) -> {
      // get the constructors and custom types for the module
      use module <- result.try(find_module_by_name(
        context.internals,
        maybe_module,
      ))
      // identify the custom type and variant used
      use #(module_id, custom_type, signature) <- result.try(case
        dict.get(module.functions, constructor_name)
      {
        Ok(
          FunctionSignature(
            _name,
            _pos,
            _lab,
            TypeConstructor(TypeFromModule(module_id, _), _) as return_type,
          ) as signature,
        ) -> {
          Ok(#(module_id, return_type, signature))
        }
        _ -> Error(compiler.AnotherTypeError("Unexpected constructor type"))
      })
      // TODO: check all fields that are attempted to be set, exist
      // TODO: check that the variant has any record fields
      //
      let fields =
        list.map(fields, fn(field) {
          let #(name, expr) = field
          glance.Field(Some(name), expr)
        })
      // map variant fields to either how they are set in the record update, or their existing value
      use #(context, args) <- result.map(
        pair_args(signature, fields, compiler.AtExpression(expr))
        |> result.map(list.index_map(_, fn(v, i) { #(i, v) }))
        |> result.try(try_map_fold(
          over: _,
          from: context,
          with: fn(context, arg) {
            let #(pos, #(field_type, maybe_expr)) = arg
            case maybe_expr {
              None ->
                init_inference_call_builtin(
                  AccessField(Some(signature.name), ByPosition(pos)),
                  [record],
                  field_type,
                  environment,
                  context,
                )
              Some(expr) ->
                init_inference(expr, field_type, environment, context)
            }
          },
        )),
      )
      // now call the constructor
      #(
        add_constraint(context, Equal(expected_type, custom_type)),
        Call(
          FunctionReference(
            signature_type(signature),
            FunctionFromModule(module_id, constructor_name),
          ),
          args,
        ),
      )
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
        |> result.map(maybe_call_no_arg_constructor(
          module.location,
          _,
          expected_type,
          context,
        ))
      })
      |> result.replace_error(compiler.AnotherTypeError("Field access error"))
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
      init_inference_call(
        function,
        args,
        None,
        glance.Expression(expr),
        expected_type,
        environment,
        context,
      )
    }
    glance.TupleIndex(tuple, index) -> {
      init_inference_call_builtin(
        AccessField(None, ByPosition(index)),
        [tuple],
        expected_type,
        environment,
        context,
      )
    }
    glance.FnCapture(_label, function, args_before, args_after) -> {
      // TODO: proper arg name
      let arg_name = "?"
      let #(context, arg_type) = fresh_type_variable(context)
      let #(context, return_type) = fresh_type_variable(context)
      init_inference(
        glance.Call(
          function,
          list.append(args_before, [
            glance.Field(None, glance.Variable(arg_name)),
            ..args_after
          ]),
        ),
        return_type,
        dict.insert(environment, arg_name, arg_type),
        context,
      )
      |> result.map(fn(res) {
        let #(context, expr) = res
        let fn_type = FunctionType([arg_type], return_type)
        #(
          add_constraint(context, Equal(expected_type, fn_type)),
          Fn(expected_type, [glance.Named(arg_name)], [Expression(expr)]),
        )
      })
    }
    glance.BitString(_) -> todo
    glance.Case(subjects, clauses) -> {
      use #(context, subjects) <- result.try(
        try_map_fold(over: subjects, from: context, with: fn(context, subject) {
          let #(context, subj_type) = fresh_type_variable(context)
          init_inference(subject, subj_type, environment, context)
          |> result.map(fn(res) {
            let #(context, subj) = res
            #(context, #(subj, subj_type))
          })
        }),
      )
      use #(context, clauses) <- result.map(
        try_map_fold(over: clauses, from: context, with: fn(context, clause) {
          init_inference_clause(
            clause,
            expected_type,
            subjects,
            environment,
            context,
          )
        }),
      )
      #(context, Case(expected_type, subjects |> list.map(pair.first), clauses))
    }
    glance.BinaryOperator(glance.Pipe, left, right) ->
      case right {
        glance.Call(function, args) ->
          init_inference(
            glance.Call(function, [glance.Field(None, left), ..args]),
            expected_type,
            environment,
            context,
          )
        _ ->
          init_inference(
            glance.Call(right, [glance.Field(None, left)]),
            expected_type,
            environment,
            context,
          )
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
        False -> {
          Error(compiler.AnotherTypeError(
            "Unable to unify argument lists of different lengths",
          ))
        }
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
        True ->
          Error(compiler.AnotherTypeError(
            "Can not substitute variable by expression that contains it",
          ))
        False -> Ok(add_substitution(context, i, b))
      }
    _, TypeVariable(j) ->
      case occurs_in(j, a) {
        True ->
          Error(compiler.AnotherTypeError(
            "Can not substitute variable by expression that contains it",
          ))
        False -> Ok(add_substitution(context, j, a))
      }
    TypeConstructor(id_i, args_i), TypeConstructor(id_j, args_j) -> {
      case id_i == id_j && list.length(args_i) == list.length(args_j) {
        False -> {
          pprint.debug(#(a, b))
          Error(compiler.AnotherTypeError("Mismatching TypeConstructors"))
        }
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
    -> {
      pprint.debug(#(a, b))
      Error(compiler.AnotherTypeError("Unification not implemented"))
    }
  }
}

pub fn solve_constraints(
  context: Context,
) -> Result(Dict(String, Type), CompilerError) {
  // TODO: rather than sort, probably need an iterative / recursive solve
  list.sort(context.constraints, fn(a, b) {
    case a, b {
      Equal(_, _), HasFieldOfType(_, _, _, _) -> order.Lt
      Equal(_, _), Equal(_, _) -> order.Eq
      HasFieldOfType(_, _, _, _), HasFieldOfType(_, _, _, _) -> order.Eq
      HasFieldOfType(_, _, _, _), Equal(_, _) -> order.Gt
    }
  })
  |> list.try_fold(from: context, with: fn(context, constraint) {
    case constraint {
      Equal(a, b) -> unify(context, a, b)
      HasFieldOfType(container_type, _, ByLabel(field_name), field_type) -> {
        use #(module_id, type_name) <- result.try(case container_type {
          TypeConstructor(TypeFromModule(module_id, type_name), _params) ->
            Ok(#(module_id, type_name))
          FunctionType(_, _) ->
            Error(compiler.AnotherTypeError(
              "Can not access field of function type",
            ))
          TypeConstructor(BuiltInType(_), _) ->
            Error(compiler.AnotherTypeError(
              "Can not access field of built-in type",
            ))
          TypeVariable(_) ->
            case get_sub(context, container_type) {
              None ->
                Error(compiler.AnotherTypeError(
                  "Type variable does not have substitution",
                ))
              Some(TypeConstructor(
                TypeFromModule(module_id, type_name),
                _params,
              )) -> Ok(#(module_id, type_name))
              Some(_) -> {
                Error(compiler.AnotherTypeError("Unexpected container type"))
              }
            }
        })
        use custom_type <- result.try(
          find_module_by_id(context.internals, module_id)
          |> result.try(fn(module) { lookup(module.custom_types, type_name) }),
        )
        // TODO: add fields as an attribute of custom types
        list.try_map(custom_type.variants, fn(variant) {
          list.find(variant.labeled_parameters, fn(field) {
            field.label == field_name
          })
        })
        |> result.replace_error(compiler.AnotherTypeError(
          "Not all variants have field",
        ))
        |> result.try(fn(fields) {
          let types =
            list.map(fields, fn(field) { field.item })
            |> list.unique
          case types {
            [t] -> Ok(t)
            _ -> {
              Error(compiler.AnotherTypeError(
                "Field type not consistent across variants",
              ))
            }
          }
        })
        |> result.try(unify(context, _, field_type))
      }
      HasFieldOfType(container_type, maybe_variant, ByPosition(i), field_type) -> {
        use tuple_params <- result.try(case container_type {
          FunctionType(_, _) ->
            Error(compiler.AnotherTypeError("Functions do not have fields"))
          TypeConstructor(BuiltInType(TupleType(_n)), params) -> Ok(params)
          TypeConstructor(_, _) ->
            Error(compiler.AnotherTypeError(
              "ByPosition constraint not implemented for Constructor",
            ))
          TypeVariable(_) ->
            case get_sub(context, container_type) {
              None ->
                Error(compiler.AnotherTypeError(
                  "No substitution for type variable",
                ))
              Some(TypeConstructor(BuiltInType(TupleType(_n)), params)) ->
                Ok(params)
              Some(TypeConstructor(
                TypeFromModule(module_id, type_name),
                _params,
              )) -> {
                // TODO: do I need to worry about the type params?
                case maybe_variant {
                  Some(variant_name) -> {
                    find_module_by_id(context.internals, module_id)
                    |> result.try(fn(module) {
                      lookup(module.custom_types, type_name)
                    })
                    |> result.try(fn(custom_type) {
                      list.find(custom_type.variants, fn(variant) {
                        variant.name == variant_name
                      })
                      |> result.replace_error(compiler.ReferenceError(
                        variant_name,
                      ))
                    })
                    |> result.map(fn(variant) {
                      list.append(
                        variant.positional_parameters,
                        list.map(variant.labeled_parameters, fn(p) { p.item }),
                      )
                    })
                  }
                  None ->
                    Error(compiler.AnotherTypeError(
                      "Indexing into custom type by position expects variant",
                    ))
                }
              }
              Some(_) ->
                Error(compiler.AnotherTypeError(
                  "Unexpected type for ByPosition indexing",
                ))
            }
        })
        // TODO: obviously tuple_params no longer makes sense here
        list.split(tuple_params, i)
        |> pair.second
        |> list.first
        |> result.replace_error(compiler.AnotherTypeError(
          "Tuple does not have element at the given index",
        ))
        |> result.try(unify(context, _, field_type))
      }
    }
  })
  |> result.map(fn(context) { context.substitution })
}

pub fn signature_find_free_type_variables(s: FunctionSignature) -> List(String) {
  [
    find_free_type_variables(s.return),
    ..list.append(
      list.map(s.positional_parameters, find_free_type_variables),
      list.map(s.labeled_parameters, fn(e) { find_free_type_variables(e.item) }),
    )
  ]
  |> list.fold(from: [], with: list.append)
}

pub fn signature_replace_free_type_variables(
  s: FunctionSignature,
  context: Context,
) -> #(Context, FunctionSignature) {
  let #(context, substitution) =
    signature_find_free_type_variables(s)
    |> list.unique
    |> list.map_fold(from: context, with: fn(context, name) {
      let #(context, var) = fresh_type_variable(context)
      #(context, #(name, var))
    })
    |> pair.map_second(dict.from_list)
  #(context, substitute_signature(s, substitution))
}

pub fn find_free_type_variables(t: Type) -> List(String) {
  case t {
    FunctionType(params, return) ->
      [return, ..params] |> list.flat_map(find_free_type_variables)
    TypeConstructor(_id, params) ->
      params |> list.flat_map(find_free_type_variables)
    TypeVariable(name) -> [name]
  }
}

pub fn replace_free_type_variables(
  t: Type,
  context: Context,
) -> #(Context, Type) {
  let #(context, substitution) =
    find_free_type_variables(t)
    |> list.unique
    |> list.map_fold(from: context, with: fn(context, name) {
      let #(context, var) = fresh_type_variable(context)
      #(context, #(name, var))
    })
    |> pair.map_second(dict.from_list)
  #(context, substitute(t, substitution))
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

pub fn substitute_function(f: Function, subs: Dict(String, Type)) -> Function {
  Function(
    signature: substitute_signature(f.signature, subs),
    argument_names: f.argument_names,
    body: case f.body {
      GleamBody(body) -> GleamBody(substitute_statement_list(body, subs))
      _ as extern -> extern
    },
  )
}

pub fn substitute_signature(
  s: FunctionSignature,
  subs: Dict(String, Type),
) -> FunctionSignature {
  FunctionSignature(
    s.name,
    list.map(s.positional_parameters, substitute(_, subs)),
    list.map(s.labeled_parameters, fn(e) {
      Labeled(e.label, substitute(e.item, subs))
    }),
    substitute(s.return, subs),
  )
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
    Assignment(kind, PatternWithVariables(pattern, vars), expr) ->
      Assignment(
        kind,
        PatternWithVariables(
          pattern,
          dict.map_values(vars, fn(_name, typ) { substitute(typ, subs) }),
        ),
        substitute_expression(expr, subs),
      )
  }
}

pub fn substitute_clause(clause: Clause, subs: Dict(String, Type)) -> Clause {
  let Clause(patterns, variables, guard, body) = clause
  Clause(
    patterns,
    dict.map_values(variables, fn(_name, t) { substitute(t, subs) }),
    substitute_expression(guard, subs),
    substitute_expression(body, subs),
  )
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
    FunctionReference(t, id) -> FunctionReference(substitute(t, subs), id)
    Trap(t, kind, maybe_expr) ->
      Trap(
        substitute(t, subs),
        kind,
        option.map(maybe_expr, substitute_expression(_, subs)),
      )
    Case(t, subjects, clauses) ->
      Case(
        substitute(t, subs),
        list.map(subjects, substitute_expression(_, subs)),
        list.map(clauses, substitute_clause(_, subs)),
      )
  }
}

// TODO: also return a list of free type variables?
// TODO: delete this function?
pub fn infer(
  expr: glance.Expression,
  data: ModuleInternals,
) -> Result(Expression, CompilerError) {
  let #(context, type_var) = fresh_type_variable(new_context(data))
  init_inference(expr, type_var, dict.new(), context)
  |> result.try(fn(initd) {
    let #(context, initd_fn) = initd
    solve_constraints(context)
    |> result.map(substitute_expression(initd_fn, _))
  })
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
  |> result.replace_error(compiler.ReferenceError(imported_module))
}

pub type ModuleInternals {
  ModuleInternals(
    project: Project,
    location: ModuleId,
    imports: Dict(String, Module),
    types: Dict(String, GenericType),
    custom_types: Dict(String, CustomType),
    functions: Dict(String, FunctionSignature),
    public_types: Set(String),
    public_functions: Set(String),
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
          lookup(data.types, name)
          |> result.map(fn(t) { #(data.location, t) })
        Some(module_alias) ->
          lookup(data.imports, module_alias)
          |> result.try(fn(module) {
            lookup(module.types, name)
            |> result.map(fn(def) {
              #(module.location, prototype(def.parameters))
            })
          })
      }
      case candidate {
        Ok(#(location, candidate_type)) -> {
          case candidate_type, params {
            GenericType(typ: nested_type, parameters: exp_params), act_params -> {
              // replace type variables to prevent clashes
              let rep_params =
                list.index_map(exp_params, fn(_, i) { "$" <> int.to_string(i) })
              let nested_type =
                list.map2(exp_params, rep_params, fn(a, b) {
                  #(a, TypeVariable(b))
                })
                |> dict.from_list
                |> substitute(nested_type, _)
              // now match and replace type parameters
              list.strict_zip(rep_params, act_params)
              |> result.replace_error(compiler.TypeArityError(
                declared_type,
                list.length(exp_params),
                list.length(act_params),
              ))
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
            "Bool", None, [] -> Ok(bool_type)
            "Nil", None, [] -> Ok(nil_type)
            "List", None, [item_type] ->
              resolve_type(data, item_type)
              |> result.map(list_type)
            _, _, _ -> Error(e)
          }
      }
    }
    glance.TupleType(elems) -> {
      list.try_map(elems, resolve_type(data, _))
      |> result.map(fn(elem_types) {
        TypeConstructor(BuiltInType(TupleType(list.length(elems))), elem_types)
      })
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
  field_type: glance.Type,
) -> Result(Type, CompilerError) {
  resolve_type(data, field_type)
  |> result.try(fn(t) {
    let undeclared =
      find_type_variables(t) |> set.difference(parameters) |> set.to_list
    case undeclared {
      [] -> Ok(t)
      [u, ..] -> Error(compiler.ReferenceError(u))
    }
  })
}

fn separate_positional_and_labeled(
  parameters: List(glance.Field(a)),
) -> Result(#(List(a), List(Labeled(a))), compiler.CompilerError) {
  let #(pos_params, label_params) =
    list.split_while(parameters, is_positional_param)
  // Positional arguments are not allowed after labeled arguments
  use <- bool.guard(
    list.any(label_params, is_positional_param),
    Error(compiler.AnotherTypeError(
      "Positional params not allowed after labeled params",
    )),
    // TODO: Error(compiler.PositionalArgsAfterLabeledArgsError(location)),
  )
  let pos_params = list.map(pos_params, fn(field) { field.item })
  let label_params =
    list.map(label_params, fn(field) {
      let assert Some(name) = field.label
      Labeled(name, field.item)
    })
  Ok(#(pos_params, label_params))
}

fn resolve_variant(
  data: ModuleInternals,
  parameters: set.Set(String),
  variant: glance.Variant,
  return_type: Type,
) -> Result(FunctionSignature, CompilerError) {
  use #(pos_params, label_params) <- result.try(separate_positional_and_labeled(
    variant.fields,
  ))
  use pos_params <- result.try(
    list.try_map(pos_params, fn(field_type) {
      resolve_variant_field(data, parameters, field_type)
    }),
  )
  use label_params <- result.map(
    list.try_map(label_params, fn(labeled_field) {
      let Labeled(name, field_type) = labeled_field
      resolve_variant_field(data, parameters, field_type)
      |> result.map(Labeled(name, _))
    }),
  )
  FunctionSignature(variant.name, pos_params, label_params, return_type)
}

pub fn resolve_custom_type(
  data: ModuleInternals,
  parsed: glance.Definition(glance.CustomType),
) -> Result(#(String, CustomType), CompilerError) {
  let type_id =
    TypeConstructor(
      TypeFromModule(data.location, parsed.definition.name),
      parsed.definition.parameters |> list.map(TypeVariable),
    )
  list.map(parsed.definition.variants, resolve_variant(
    data,
    set.from_list(parsed.definition.parameters),
    _,
    type_id,
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

fn referenced_functions_clause(clause: Clause) -> List(FunctionId) {
  let Clause(_patterns, _variables, _guard, body) = clause
  referenced_functions_expr(body)
}

fn referenced_functions_expr(expr: Expression) -> List(FunctionId) {
  case expr {
    Call(fun, args) -> [fun, ..args] |> list.flat_map(referenced_functions_expr)
    Case(_type, subjects, clauses) ->
      list.append(
        subjects |> list.flat_map(referenced_functions_expr),
        clauses |> list.flat_map(referenced_functions_clause),
      )
    Fn(_type, _arg_names, body) -> referenced_functions(body)
    FunctionReference(_type, id) -> [id]
    Int(_) | Float(_) | String(_) | Trap(_, _, _) | Variable(_, _) -> []
  }
}

fn referenced_functions(statement_list: List(Statement)) -> List(FunctionId) {
  statement_list
  |> list.flat_map(fn(stmt) {
    case stmt {
      Assignment(_kind, _pattern, expr) -> referenced_functions_expr(expr)
      Expression(expr) -> referenced_functions_expr(expr)
    }
  })
}

pub fn resolve_types(
  project: Project,
  location: ModuleId,
  imports: Dict(String, ModuleId),
  modules: Dict(ModuleId, Module),
  parsed: glance.Module,
) -> Result(ModuleInternals, CompilerError) {
  // Create prototypes (placeholders) for all declared types
  let custom_prototypes =
    parsed.custom_types
    |> list.map(prototype_custom_type)
  let alias_prototypes = parsed.type_aliases |> list.map(prototype_type_alias)
  let prototypes =
    dict.from_list(list.append(custom_prototypes, alias_prototypes))
  // Resolve imports
  let imported_modules =
    imports
    |> dict.map_values(fn(_, loc) {
      let assert Ok(module) = dict.get(modules, loc)
      module
    })
  // Construct initial ModuleInternals using the prototypes
  let internals =
    ModuleInternals(
      project: project,
      location: location,
      imports: imported_modules,
      types: prototypes,
      custom_types: dict.new(),
      functions: dict.new(),
      public_types: set.new(),
      public_functions: set.new(),
    )

  // TODO: hack or OK?
  use unqualified_type_imports <- result.try(
    list.flat_map(parsed.imports, fn(module) {
      list.map(module.definition.unqualified_types, fn(unq) {
        #(
          module.definition.module,
          unq.name,
          unq.alias |> option.unwrap(unq.name),
        )
      })
    })
    |> list.try_map(fn(unq) {
      let #(module_path, name, alias) = unq
      // TODO: is resolve_module right here?
      resolve_module(project, internals.location.package_name, module_path)
      |> result.try(fn(module_id) {
        dict.get(modules, module_id)
        |> result.replace_error(compiler.ReferenceError(module_path))
      })
      |> result.try(fn(module) { lookup(module.types, name) })
      |> result.map(fn(typ) { #(alias, typ) })
    })
    |> result.map(dict.from_list),
  )

  // Resolve the type definitions
  use aliases <- result.try(
    result.all(list.map(parsed.type_aliases, resolve_type_alias(internals, _))),
  )
  let internals =
    ModuleInternals(
      ..internals,
      types: dict.merge(
        unqualified_type_imports,
        dict.merge(prototypes, dict.from_list(aliases)),
      ),
    )
  use custom_types <- result.map(
    result.all(list.map(parsed.custom_types, resolve_custom_type(internals, _))),
  )

  // Wrap custom types for uniformity with type aliases
  let custom_types_generic =
    list.map(custom_types, fn(custom_type_def) {
      let #(name, custom_type) = custom_type_def
      let generic =
        GenericType(
          TypeConstructor(
            TypeFromModule(location, name),
            custom_type.parameters |> list.map(TypeVariable(_)),
          ),
          custom_type.parameters,
        )
      #(name, generic)
    })

  // TODO: ensure uniqueness?

  // Find all the public types
  let public_types =
    list.append(
      list.map(parsed.type_aliases, fn(alias) {
        let glance.Definition(
          definition: glance.TypeAlias(
            name:,
            publicity:,
            ..,
          ),
          ..,
        ) = alias
        #(name, publicity)
      }),
      list.map(parsed.custom_types, fn(alias) {
        let glance.Definition(
          definition: glance.CustomType(
            name:,
            publicity:,
            ..,
          ),
          ..,
        ) = alias
        #(name, publicity)
      }),
    )
    |> list.filter(fn(pair) { pair.second(pair) == glance.Public })
    |> list.map(pair.first)

  ModuleInternals(
    ..internals,
    types: unqualified_type_imports
      |> dict.merge(dict.from_list(aliases))
      |> dict.merge(dict.from_list(custom_types_generic)),
    custom_types: dict.from_list(custom_types),
    public_types: set.from_list(public_types),
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
  pprint.debug(
    ">>> analyze_high_level "
    <> location.module_path
    <> " "
    <> location.package_name,
  )
  use parsed <- result.try(project.parse_module(project, location))
  use #(modules, imports) <- result.try(
    try_map_fold(parsed.imports, modules, fn(modules, import_) {
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
      #(dict.insert(modules, location, module), #(alias, module.location))
    }),
  )
  let imports = dict.from_list(imports)
  pprint.debug(
    ">>> analyze_high_level -- resolve types "
    <> location.module_path
    <> " "
    <> location.package_name,
  )
  use internals <- result.try(resolve_types(
    project,
    location,
    imports,
    modules,
    parsed,
  ))

  // TODO: hack
  let result_type =
    TypeConstructor(TypeFromModule(gleam_module_id, "Result"), [
      TypeVariable("a"),
      TypeVariable("b"),
    ])
  let internals =
    ModuleInternals(
      ..internals,
      custom_types: dict.insert(
          internals.custom_types,
          "Result",
          CustomType(parameters: ["a", "b"], opaque_: False, variants: [
            FunctionSignature("Ok", [TypeVariable("a")], [], result_type),
            FunctionSignature("Error", [TypeVariable("b")], [], result_type),
          ]),
        )
        |> dict.insert(
          "Nil",
          CustomType(parameters: [], opaque_: False, variants: [
            FunctionSignature("Nil", [], [], nil_type),
          ]),
        )
        |> dict.insert(
          "Bool",
          CustomType(parameters: [], opaque_: False, variants: [
            FunctionSignature("True", [], [], bool_type),
            FunctionSignature("False", [], [], bool_type),
          ]),
        ),
      types: dict.insert(
          internals.types,
          "Result",
          GenericType(result_type, ["a", "b"]),
        )
        |> dict.insert("Nil", GenericType(nil_type, []))
        |> dict.insert("Bool", GenericType(bool_type, [])),
    )

  pprint.debug(
    ">>> analyze_high_level -- make function prototypes "
    <> location.module_path
    <> " "
    <> location.package_name,
  )

  use function_prototypes <- result.try(
    list.try_map(parsed.functions, fn(def) {
      let context = new_context(internals)
      let glance.Definition(
        definition: glance.Function(
          name:,
          parameters:,
          return:,
          ..,
        ),
        ..,
      ) = def
      use #(pos_args, lab_args) <- result.try(
        list.map(parameters, fn(param) {
          let glance.FunctionParameter(label:, type_:, ..) = param
          glance.Field(label, type_)
        })
        |> separate_positional_and_labeled,
      )
      use #(context, pos_args) <- result.try(
        pos_args |> try_map_fold(context, resolve_optional_type),
      )
      use #(context, lab_args) <- result.try(
        lab_args
        |> try_map_fold(context, fn(context, arg) {
          let Labeled(name, typ) = arg
          resolve_optional_type(context, typ)
          |> result.map(fn(res) {
            let #(context, typ) = res
            #(context, Labeled(name, typ))
          })
        }),
      )
      use #(_context, return) <- result.map(resolve_optional_type(
        context,
        return,
      ))
      #(name, FunctionSignature(name, pos_args, lab_args, return))
    })
    |> result.map(dict.from_list),
  )

  let constructor_prototypes =
    internals.custom_types
    |> dict.values()
    |> list.flat_map(fn(ct) { ct.variants })
    |> list.map(fn(constructor) { #(constructor.name, constructor) })
    |> dict.from_list

  let internals =
    ModuleInternals(
      ..internals,
      functions: dict.merge(function_prototypes, constructor_prototypes),
    )
  // TODO: resolve constants

  pprint.debug(
    ">>> analyze_high_level -- call graph "
    <> location.module_path
    <> " "
    <> location.package_name,
  )
  // call graph analysis
  // TODO: probably ID referenced functions from the raw inputs and run inference only afterwards
  use functions <- result.try(
    parsed.functions
    |> list.try_map(infer_function(internals, _))
    |> result.map(list.map(_, fn(fun) { #(fun.signature.name, fun) }))
    |> result.map(dict.from_list),
  )
  let edges =
    dict.to_list(functions)
    |> list.flat_map(fn(fun) {
      let #(from_name, Function(body:, ..)) = fun
      case body {
        GleamBody(body) -> referenced_functions(body)
        _ -> []
      }
      |> list.filter_map(fn(ref_fn) {
        case ref_fn {
          FunctionFromModule(module_id, to_name)
            if module_id == internals.location && to_name != from_name
          -> Ok(#(from_name, to_name))
          _ -> Error(Nil)
        }
      })
    })
  let nodes = dict.keys(functions)
  let #(nodes, edges) = graph.squash_cycles(nodes, edges)
  use internals <- result.map(
    graph.topological_sort(nodes, edges)
    |> result.replace_error(compiler.AnotherTypeError("Cycle appeared..."))
    |> result.try(list.try_fold(
      over: _,
      from: internals,
      with: fn(internals, fn_names) {
        list.try_map(fn_names, fn(name) {
          list.find(parsed.functions, fn(fun) { fun.definition.name == name })
        })
        |> result.replace_error(compiler.AnotherTypeError(
          "Function disappeared...",
        ))
        |> result.try(infer_functions(internals, _))
        |> result.map(fn(functions) {
          // TODO: maybe not throw the function bodies away :'(
          ModuleInternals(
            ..internals,
            functions: list.fold(functions, internals.functions, fn(d, fun) {
              dict.insert(d, fun.signature.name, fun.signature)
            }),
          )
        })
      },
    )),
  )

  let public_constructors =
    list.filter_map(parsed.custom_types, fn(ct) {
      let glance.Definition(_attr, glance.CustomType(publicity:, variants:, ..)) =
        ct
      case publicity {
        glance.Public -> Ok(list.map(variants, fn(variant) { variant.name }))
        glance.Private -> Error(Nil)
      }
    })
    |> list.flatten

  let public_functions =
    list.map(parsed.functions, fn(fun) {
      let glance.Definition(_attr, glance.Function(name:, publicity:, ..)) = fun
      #(name, publicity)
    })
    |> list.filter(fn(pair) { pair.second(pair) == glance.Public })
    |> list.map(pair.first)
    |> list.append(public_constructors)
    |> set.from_list

  #(
    module_from_internals(ModuleInternals(..internals, public_functions:)),
    modules,
  )
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
  project.scan_project(".", "javascript")
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
