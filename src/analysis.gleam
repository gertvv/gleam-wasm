import compiler.{type CompilerError}
import glance
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
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

// TODO:
// - constructors that have the same name as their type?
// - unqualified imports
// - identify which variables a closure captures from its environment
// - constants
// - bit string support
// - hole types

// TODO: move somewhere
pub fn try_map_fold(
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
  UtfCodepointType
  BitArrayType
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

pub fn flatten_params(signature: FunctionSignature) -> List(Type) {
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

pub const utf_codepoint_type = TypeConstructor(
  BuiltInType(UtfCodepointType),
  [],
)

pub const bit_array_type = TypeConstructor(BuiltInType(BitArrayType), [])

pub const string_type = TypeConstructor(BuiltInType(StringType), [])

pub fn list_type(item_type: Type) {
  TypeConstructor(BuiltInType(ListType), [item_type])
}

pub fn tuple_type(element_types: List(Type)) {
  TypeConstructor(
    BuiltInType(TupleType(list.length(element_types))),
    element_types,
  )
}

pub const gleam_module_id = project.ModuleId("gleam", "gleam")

pub type Field(t) {
  Field(label: Option(String), item: t)
}

// Will include only high level type information and signatures
pub type ModuleInterface {
  ModuleInterface(
    location: ModuleId,
    imports: Dict(String, ModuleId),
    types: Dict(String, GenericType),
    custom_types: Dict(String, CustomType),
    functions: Dict(String, FunctionSignature),
  )
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
  Block(typ: Type, body: List(Statement))
  Variable(typ: Type, name: String)
  Trap(typ: Type, kind: TrapKind, detail: Option(Expression))
  FunctionReference(typ: Type, id: FunctionId)
  Fn(
    typ: Type,
    argument_names: List(glance.AssignmentName),
    body: List(Statement),
    captures: List(#(String, Type)),
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
    locals: Set(String),
    captures: List(#(String, Type)),
  )
}

pub fn new_context(internals: ModuleInternals) -> Context {
  Context(
    internals:,
    substitution: dict.new(),
    constraints: [],
    next_variable: 1,
    next_generated_name: 1,
    locals: set.new(),
    captures: [],
  )
}

fn context_new_fn(context: Context) -> Context {
  Context(..context, locals: set.new(), captures: [])
}

fn context_restore(context: Context, old: Context) -> Context {
  Context(..context, locals: old.locals, captures: old.captures)
}

fn context_let_var(context: Context, name: String) -> Context {
  Context(..context, locals: set.insert(context.locals, name))
}

fn context_let_vars(context: Context, vars: List(String)) -> Context {
  list.fold(over: vars, from: context, with: fn(context, var) {
    context_let_var(context, var)
  })
}

fn context_ref_var(context: Context, name: String, t: Type) -> Context {
  case set.contains(context.locals, name) {
    True -> context
    False -> Context(..context, captures: [#(name, t), ..context.captures])
  }
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

/// Treat the `ModuleInternals` as a `ModuleInterface`.
/// Enables uniform lookup of type and function data.
fn internals_as_module(internals: ModuleInternals) -> ModuleInterface {
  ModuleInterface(
    internals.location,
    dict.map_values(internals.imports, fn(_, m) { m.location }),
    internals.types,
    internals.custom_types,
    internals.functions,
  )
}

/// Convert `ModuleInternals` to a `ModuleInterface`.
/// Removes reference to any private types and functions.
pub fn module_from_internals(internals: ModuleInternals) -> ModuleInterface {
  ModuleInterface(
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
) -> Result(ModuleInterface, compiler.CompilerError) {
  case maybe_module_name {
    None -> Ok(internals_as_module(internals))
    Some(module_name) -> lookup(internals.imports, module_name)
  }
}

fn find_module_by_id(
  internals: ModuleInternals,
  module_id: ModuleId,
) -> Result(ModuleInterface, compiler.CompilerError) {
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
        context_let_vars(context, dict.keys(pattern.variables)),
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
  use #(context, args) <- result.try(case function {
    FunctionReference(_, FunctionFromModule(module_id, function_name)) -> {
      // if we're calling a global function, look up the signature for labels
      use signature <- result.try(
        find_module_by_id(context.internals, module_id)
        |> result.try(fn(module) { lookup(module.functions, function_name) }),
      )
      // TODO: inject fresh type variables -- check if safe
      let #(context, signature) =
        signature_replace_free_type_variables(signature, context)
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
          None -> Ok(#(context, args))
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
      |> result.map(fn(args) { #(context, args) })
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
    when: list.any(alternative_variables, fn(vars) {
      dict.keys(vars) != dict.keys(variables)
    }),
    return: Error(compiler.AnotherTypeError(
      "All alternative patterns must set the same variables",
    )),
  )
  let environment = dict.merge(environment, variables)

  // variables in alternative patterns must have the same type
  let sort_variable_types = fn(variables) {
    dict.to_list(variables)
    |> list.sort(fn(a, b) { string.compare(a.0, b.0) })
    |> list.map(pair.second)
  }
  let sorted_types = sort_variable_types(variables)
  let context =
    list.drop(alternative_variables, 1)
    |> list.fold(context, fn(context, other_variables) {
      sort_variable_types(other_variables)
      |> list.zip(sorted_types)
      |> list.fold(context, fn(context, pair) {
        let #(t1, t2) = pair
        add_constraint(context, Equal(t1, t2))
      })
    })

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
  let assert Fn(typ: FunctionType(param_types, return_type), body:, ..) = fun

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
  // TODO: problem! new_context may not be appropriate if functions have been prototyped before... re-prototype?
  //use func_types <- result.try(
  //  list.try_map(defs, fn(func) {
  //    lookup(internals.functions, func.definition.name)
  //    |> result.map(signature_type)
  //  }),
  //)
  let #(context, func_types) =
    list.map_fold(defs, context, fn(context, _def) {
      fresh_type_variable(context)
    })
  use #(context, functions) <- result.try(
    list.zip(func_types, defs)
    |> try_map_fold(from: context, with: fn(context, pair) {
      let #(typ, def) = pair
      init_inference_function(def, typ, dict.new(), context)
    }),
  )
  solve_constraints(context)
  |> result.map(fn(subs) { list.map(functions, substitute_function(_, subs)) })
}

fn init_inference_function(
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
  let #(context, signature) =
    signature_replace_free_type_variables(signature, context)
  let function_type = signature_type(signature)
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
            add_constraint(context, Equal(expected_type, variable_type))
              |> context_ref_var(name, variable_type),
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
      #(context, Block(expected_type, body))
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
      let named_args =
        list.zip(args, param_types)
        |> list.filter(fn(p) {
          case p.0 {
            glance.FnParameter(glance.Named(_), _) -> True
            glance.FnParameter(glance.Discarded(_), _) -> False
          }
        })
        |> list.map(
          pair.map_first(_, fn(p) { assignment_name_to_string(p.name) }),
        )
        |> dict.from_list
      let environment = dict.merge(environment, named_args)

      // Start a new function context
      let old_context = context
      let context =
        context_new_fn(context) |> context_let_vars(dict.keys(named_args))

      use #(context, body) <- result.map(init_inference_block(
        body,
        return_type,
        environment,
        context,
      ))
      let fn_type = FunctionType(param_types, return_type)
      #(
        add_constraint(context, Equal(expected_type, fn_type))
          |> context_restore(old_context),
        Fn(
          fn_type,
          list.map(args, fn_param_to_arg_name),
          body,
          context.captures,
        ),
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
        |> result.try(
          try_map_fold(over: _, from: context, with: fn(context, arg) {
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
          }),
        ),
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
      let #(context, arg_name) = fresh_generated_name(context)
      let #(context, arg_type) = fresh_type_variable(context)
      let #(context, return_type) = fresh_type_variable(context)
      init_inference(
        glance.Call(
          function,
          list.append(args_before, [
            glance.Field(
              None,
              glance.Variable(assignment_name_to_string(arg_name)),
            ),
            ..args_after
          ]),
        ),
        return_type,
        dict.insert(environment, assignment_name_to_string(arg_name), arg_type),
        context,
      )
      |> result.map(fn(res) {
        let #(context, expr) = res
        let fn_type = FunctionType([arg_type], return_type)
        #(
          add_constraint(context, Equal(expected_type, fn_type)),
          Fn(expected_type, [arg_name], [Expression(expr)], []),
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

pub fn get_sub(context: Context, t: Type) -> Result(Type, Nil) {
  case t {
    TypeVariable(i) ->
      dict.get(context.substitution, i)
      |> result.try(fn(r) {
        // TODO: worry about infinite recursion?
        case r {
          TypeVariable(j) if i != j -> get_sub(context, r) |> result.or(Ok(r))
          _ -> Ok(r)
        }
      })
    _ -> Error(Nil)
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
  let sub_a = get_sub(context, a)
  let sub_b = get_sub(context, b)
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
    TypeVariable(_), _ if sub_a != Error(Nil) && sub_a != Ok(a) -> {
      let assert Ok(sub_a) = sub_a
      unify(context, sub_a, b)
    }
    _, TypeVariable(_) if sub_b != Error(Nil) && sub_b != Ok(b) -> {
      let assert Ok(sub_b) = sub_b
      unify(context, a, sub_b)
    }
    TypeVariable(i), _ if sub_a != Error(Nil) ->
      case occurs_in(i, b) {
        True ->
          Error(compiler.AnotherTypeError(
            "Can not substitute variable by expression that contains it",
          ))
        False -> Ok(add_substitution(context, i, b))
      }
    _, TypeVariable(j) if sub_b != Error(Nil) ->
      case occurs_in(j, a) {
        True ->
          Error(compiler.AnotherTypeError(
            "Can not substitute variable by expression that contains it",
          ))
        False -> Ok(add_substitution(context, j, a))
      }
    TypeVariable(_), _ | _, TypeVariable(_) -> {
      pprint.debug(#(context.substitution, a, sub_a, b, sub_b))
      Error(compiler.AnotherTypeError(
        "Could not find a substitutable type variable",
      ))
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
              Error(Nil) ->
                Error(compiler.AnotherTypeError(
                  "Type variable does not have substitution",
                ))
              Ok(TypeConstructor(TypeFromModule(module_id, type_name), _params)) ->
                Ok(#(module_id, type_name))
              Ok(_) -> {
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
              Error(Nil) ->
                Error(compiler.AnotherTypeError(
                  "No substitution for type variable",
                ))
              Ok(TypeConstructor(BuiltInType(TupleType(_n)), params)) ->
                Ok(params)
              Ok(TypeConstructor(TypeFromModule(module_id, type_name), _params)) -> {
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
              Ok(_) ->
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
        // TODO: if I put field_type third rather than second I get infinite loops?
        |> result.try(unify(context, field_type, _))
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
    Block(typ:, body:) ->
      Block(
        typ: substitute(typ, subs),
        body: substitute_statement_list(body, subs),
      )
    Fn(typ:, argument_names:, body:, captures:) ->
      Fn(
        typ: substitute(typ, subs),
        argument_names:,
        body: substitute_statement_list(body, subs),
        captures:,
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

// TODO: distinguish between types/functions accessible within the module vs defined within it?
pub type ModuleInternals {
  ModuleInternals(
    project: Project,
    location: ModuleId,
    imports: Dict(String, ModuleInterface),
    types: Dict(String, GenericType),
    custom_types: Dict(String, CustomType),
    functions: Dict(String, FunctionSignature),
    implemented_functions: List(List(Function)),
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
              |> result.try(
                list.try_map(_, fn(param_def) {
                  let #(param_name, param_type) = param_def
                  resolve_type(data, param_type)
                  |> result.map(pair.new(param_name, _))
                }),
              )
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
            "UtfCodepoint", None, [] -> Ok(utf_codepoint_type)
            "BitArray", None, [] -> Ok(bit_array_type)
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

pub fn referenced_types(fun: Function) -> List(TypeId) {
  let signature_types = case fun {
    Function(
      FunctionSignature(_, positional_parameters:, labeled_parameters:, return:),
      _,
      _,
    ) ->
      [
        positional_parameters,
        list.map(labeled_parameters, fn(labeled) { labeled.item }),
        [return],
      ]
      |> list.flatten
      |> list.flat_map(referenced_types_type)
  }
  let body_types = case fun {
    Function(_, _, GleamBody(block)) -> referenced_types_block(block)
    _ -> []
  }
  list.append(signature_types, body_types)
}

fn referenced_types_block(statement_list: List(Statement)) -> List(TypeId) {
  list.flat_map(statement_list, fn(stmt) {
    case stmt {
      Assignment(value:, ..) -> {
        // TODO: I don't think the pattern can reference an types that the value doesn't but double check!
        referenced_types_expr(value)
      }
      Expression(expr) -> referenced_types_expr(expr)
    }
  })
}

fn referenced_types_expr(expr: Expression) -> List(TypeId) {
  case expr {
    Call(function:, arguments:) ->
      list.flat_map([function, ..arguments], referenced_types_expr)
    Case(typ:, subjects:, clauses:) ->
      [
        referenced_types_type(typ),
        list.flat_map(subjects, referenced_types_expr),
        list.flat_map(clauses, referenced_types_clause),
      ]
      |> list.flatten
    Float(_) -> [BuiltInType(FloatType)]
    Fn(typ:, body:, ..) | Block(typ:, body:) ->
      [referenced_types_type(typ), referenced_types_block(body)] |> list.flatten
    FunctionReference(typ:, ..) -> referenced_types_type(typ)
    Int(_) -> [BuiltInType(IntType)]
    String(_) -> [BuiltInType(StringType)]
    Trap(typ:, detail:, ..) ->
      [
        referenced_types_type(typ),
        option.map(detail, referenced_types_expr) |> option.unwrap([]),
      ]
      |> list.flatten
    Variable(typ, _) -> referenced_types_type(typ)
  }
}

fn referenced_types_clause(c: Clause) -> List(TypeId) {
  // TODO: again we assume no new types occur in the Pattern or Variables
  case c {
    Clause(guard:, body:, ..) ->
      list.flat_map([guard, body], referenced_types_expr)
  }
}

fn referenced_types_type(t: Type) -> List(TypeId) {
  case t {
    FunctionType(parameters:, return:) ->
      list.flat_map([return, ..parameters], referenced_types_type)
    TypeConstructor(generic_type:, assigned_types:) -> [
      generic_type,
      ..list.flat_map(assigned_types, referenced_types_type)
    ]
    TypeVariable(_) -> []
  }
}

pub fn referenced_functions(fun: Function) -> List(FunctionId) {
  let Function(body:, ..) = fun
  case body {
    GleamBody(body) -> referenced_functions_block(body)
    _ -> []
  }
}

fn referenced_functions_block(
  statement_list: List(Statement),
) -> List(FunctionId) {
  list.flat_map(statement_list, fn(stmt) {
    case stmt {
      Assignment(_kind, _pattern, expr) -> referenced_functions_expr(expr)
      Expression(expr) -> referenced_functions_expr(expr)
    }
  })
}

fn referenced_functions_expr(expr: Expression) -> List(FunctionId) {
  case expr {
    Call(fun, args) -> [fun, ..args] |> list.flat_map(referenced_functions_expr)
    Case(_type, subjects, clauses) ->
      list.append(
        subjects |> list.flat_map(referenced_functions_expr),
        clauses |> list.flat_map(referenced_functions_clause),
      )
    Fn(body:, ..) | Block(body:, ..) -> referenced_functions_block(body)
    FunctionReference(_type, id) -> [id]
    Int(_) | Float(_) | String(_) | Trap(_, _, _) | Variable(_, _) -> []
  }
}

fn referenced_functions_clause(clause: Clause) -> List(FunctionId) {
  let Clause(_patterns, _variables, _guard, body) = clause
  referenced_functions_expr(body)
}

pub fn resolve_types(
  project: Project,
  location: ModuleId,
  imports: Dict(String, ModuleId),
  modules: Dict(ModuleId, ModuleInterface),
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
      implemented_functions: [],
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

pub fn prototype_function_signature(
  internals: ModuleInternals,
  def: glance.Definition(glance.Function),
) -> Result(FunctionSignature, CompilerError) {
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
  use #(_context, return) <- result.map(resolve_optional_type(context, return))
  FunctionSignature(name, pos_args, lab_args, return)
}

fn parse_module_imports(
  project: Project,
  module_id: ModuleId,
) -> Result(List(ModuleId), CompilerError) {
  use parsed <- result.try(project.parse_module(project, module_id))
  list.try_map(parsed.imports, fn(import_) {
    case import_ {
      glance.Definition(definition: glance.Import(module: module_name, ..), ..) -> {
        resolve_module(project, module_id.package_name, module_name)
        |> result.map(fn(imported_module_id) { imported_module_id })
      }
    }
  })
}

pub fn analyze_project_imports(
  p: Project,
) -> Result(graph.Graph(ModuleId), CompilerError) {
  dict.values(p.packages)
  |> list.try_map(fn(package) {
    case package {
      project.GleamPackage(name:, modules:, ..) -> {
        let modules =
          set.to_list(modules)
          |> list.map(fn(module) { project.ModuleId(name, module) })
        use imports <- result.map(
          list.try_map(modules, fn(module_id) {
            parse_module_imports(p, module_id)
            |> result.map(
              list.map(_, fn(imported_module_id) {
                #(module_id, imported_module_id)
              }),
            )
          })
          |> result.map(list.flatten),
        )
        #(modules, imports)
      }
      _ -> Ok(#([], []))
    }
  })
  |> result.map(
    list.fold(_, #([], []), fn(a, b) {
      #(list.append(a.0, b.0), list.append(a.1, b.1))
    }),
  )
}

pub fn analyze_module(
  project: Project,
  location: ModuleId,
  modules: Dict(ModuleId, ModuleInterface),
) -> Result(ModuleInternals, CompilerError) {
  pprint.debug(
    ">>> analyze_high_level "
    <> location.module_path
    <> " "
    <> location.package_name,
  )
  use parsed <- result.try(project.parse_module(project, location))
  use imports <- result.try(
    list.try_map(parsed.imports, fn(import_) {
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
      use module <- result.map(
        dict.get(modules, location)
        |> result.replace_error(compiler.ReferenceError(
          location.package_name <> ":" <> location.module_path,
        )),
      )
      #(alias, module.location)
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

  let target_functions =
    list.filter(parsed.functions, exists_for_target(_, project.target))

  use function_prototypes <- result.try(
    list.try_map(target_functions, fn(def) {
      prototype_function_signature(internals, def)
      |> result.map(fn(signature) { #(signature.name, signature) })
    })
    |> result.map(dict.from_list),
  )

  let constructor_prototypes =
    internals.custom_types
    |> dict.values()
    |> list.flat_map(fn(ct) { ct.variants })
    |> list.map(fn(constructor) { #(constructor.name, constructor) })
    |> dict.from_list

  // TODO: currently assuming all imported values are functions
  // TODO: aliased unqualified imports
  use unqualified_imports <- result.try(
    list.flat_map(parsed.imports, fn(import_) {
      let glance.Definition(_, glance.Import(module:, unqualified_values:, ..)) =
        import_
      list.map(unqualified_values, fn(value) { #(module, value.name) })
    })
    |> list.try_map(fn(import_) {
      let #(module_name, value_name) = import_
      use module_id <- result.try(resolve_module(
        project,
        location.package_name,
        module_name,
      ))
      use module <- result.try(
        dict.get(modules, module_id)
        |> result.replace_error(compiler.ReferenceError(module_name)),
      )
      lookup(module.functions, value_name)
      |> result.map(fn(signature) { #(value_name, signature) })
    })
    |> result.map(dict.from_list),
  )

  let internals =
    ModuleInternals(
      ..internals,
      functions: dict.merge(function_prototypes, constructor_prototypes)
        |> dict.merge(unqualified_imports),
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
    target_functions
    |> list.try_map(infer_function(internals, _))
    |> result.map(list.map(_, fn(fun) { #(fun.signature.name, fun) }))
    |> result.map(dict.from_list),
  )
  let edges =
    dict.to_list(functions)
    |> list.flat_map(fn(func) {
      let #(from_name, func) = func
      referenced_functions(func)
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
  use sorted_functions <- result.try(
    graph.topological_sort(nodes, edges)
    |> result.replace_error(compiler.CircularDependencyError),
  )
  use internals <- result.map(
    list.try_fold(
      over: sorted_functions,
      from: internals,
      with: fn(internals, fn_names) {
        list.try_map(fn_names, fn(name) {
          list.find(target_functions, fn(fun) { fun.definition.name == name })
        })
        |> result.replace_error(compiler.AnotherTypeError(
          "Function disappeared...",
        ))
        |> result.try(infer_functions(internals, _))
        |> result.map(fn(functions) {
          ModuleInternals(
            ..internals,
            functions: list.fold(functions, internals.functions, fn(d, func) {
              dict.insert(d, func.signature.name, func.signature)
            }),
            implemented_functions: [
              functions,
              ..internals.implemented_functions
            ],
          )
        })
      },
    ),
  )
  let internals =
    ModuleInternals(
      ..internals,
      implemented_functions: list.reverse(internals.implemented_functions),
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
    list.map(target_functions, fn(fun) {
      let glance.Definition(_attr, glance.Function(name:, publicity:, ..)) = fun
      #(name, publicity)
    })
    |> list.filter(fn(pair) { pair.second(pair) == glance.Public })
    |> list.map(pair.first)
    |> list.append(public_constructors)
    |> set.from_list

  ModuleInternals(..internals, public_functions:)
}

fn exists_for_target(
  def: glance.Definition(glance.Function),
  target: String,
) -> Bool {
  let glance.Definition(attributes, _) = def
  let externals =
    list.filter_map(attributes, fn(attr) {
      case attr {
        glance.Attribute("external", args) -> Ok(args)
        _ -> Error(Nil)
      }
    })
  let target_external =
    list.find(externals, fn(ext) {
      case ext {
        [glance.Variable(name), ..] if name == target -> True
        _ -> False
      }
    })
  case list.is_empty(externals), target_external, def.definition.body {
    True, _, _ -> True
    _, Ok(_), _ -> True
    _, _, [_, ..] -> True
    _, _, _ -> False
  }
}
