import compiler.{type CompilerError, type ExpressionState, type LocalState}
import glance.{type Clause, type Expression, type Statement, type Type}
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import signature_encoder
import type_compiler
import wat

pub fn compile_body(
  body: List(Statement),
  state: LocalState,
) -> Result(#(LocalState, List(wat.WatExpression)), CompilerError) {
  let last = list.length(body) - 1
  list.index_map(body, fn(stmt, index) { #(stmt, index == last) })
  |> list.try_fold(#(state, []), fn(acc, item) {
    let #(stmt, tail) = item
    let #(state, compiled) = acc
    compile_statement(stmt, tail, state)
    |> result.map(fn(expr_state) {
      #(expr_state.local, [expr_state.compiled, ..compiled])
    })
  })
  |> result.map(fn(res) {
    let #(state, compiled) = res
    #(state, list.reverse(compiled))
  })
}

pub fn compile_statement(
  stmt: Statement,
  tail: Bool,
  state: LocalState,
) -> Result(ExpressionState, CompilerError) {
  case stmt {
    glance.Expression(expr) -> compile_expression(expr, state, tail)
    glance.Use(_, _) -> todo
    glance.Assignment(glance.Let, glance.PatternVariable(name), _, expr) -> {
      use state <- result.map(compile_expression(expr, state, tail))
      case tail {
        True -> state
        False ->
          compiler.ExpressionState(
            ..state,
            local: compiler.with_locals(state.local, [#(name, state.type_)]),
            compiled: wat.LocalSet(name, state.compiled),
          )
      }
    }
    glance.Assignment(_, _, _, _) -> todo
  }
}

fn generate_call_import(
  state: LocalState,
  function_import: compiler.FunctionImport,
  args: List(wat.WatExpression),
) -> #(LocalState, wat.WatExpression) {
  let function_call =
    wat.Call(function_import.module <> "/" <> function_import.name, args)
  #(compiler.import_functions(state, [function_import]), function_call)
}

fn call_import(
  state: ExpressionState,
  function_import: compiler.FunctionImport,
  args: List(wat.WatExpression),
) -> ExpressionState {
  let #(local, function_call) =
    generate_call_import(state.local, function_import, args)
  compiler.ExpressionState(..state, local: local, compiled: function_call)
}

fn box_primitive(state: ExpressionState) -> ExpressionState {
  case state.type_ {
    type_ if type_ == compiler.int_type ->
      call_import(state, compiler.import_int_box, [state.compiled])
    type_ if type_ == compiler.float_type ->
      call_import(state, compiler.import_float_box, [state.compiled])
    _ -> state
  }
}

fn unbox_primitive(
  state: ExpressionState,
  assigned_type: Type,
) -> ExpressionState {
  case assigned_type {
    type_ if type_ == compiler.int_type ->
      call_import(state, compiler.import_int_unbox, [state.compiled])
    type_ if type_ == compiler.float_type ->
      call_import(state, compiler.import_float_unbox, [state.compiled])
    _ -> state
  }
}

fn boxer(
  exp_type: Type,
  act_type: Type,
) -> Option(#(compiler.FunctionImport, compiler.FunctionImport)) {
  case exp_type, act_type {
    glance.VariableType(_), type_ if type_ == compiler.int_type ->
      Some(#(compiler.import_int_box, compiler.import_int_unbox))
    glance.VariableType(_), type_ if type_ == compiler.float_type ->
      Some(#(compiler.import_float_box, compiler.import_float_unbox))
    _, _ -> None
  }
}

fn call_closure(name: String, signature: String, args: List(wat.WatExpression)) {
  wat.CallRef(
    "function:" <> signature,
    [
      wat.StructGet("closure:" <> signature, "context", wat.LocalGet(name)),
      ..args
    ],
    wat.StructGet("closure:" <> signature, "function", wat.LocalGet(name)),
  )
}

// TODO: remove
pub fn type_compiler_hack(res) {
  case res {
    Ok(#(_, type_)) -> type_
    Error(e) -> {
      io.debug(e)
      wat.Ref(wat.Direct(wat.Any))
    }
  }
}

fn wrap_primitive_function(
  state: ExpressionState,
  exp_params: List(Type),
  exp_return: Type,
) -> ExpressionState {
  let assert glance.FunctionType(act_params, act_return) = state.type_
  let boxing = list.map2(exp_params, act_params, boxer)
  let box_return = boxer(exp_return, act_return)
  case list.find([box_return, ..boxing], option.is_some) {
    Error(_) -> state
    _ -> {
      let p = fn(index) { "p" <> int.to_string(index) }
      let wrapper_name =
        "@"
        <> state.local.anon_fn_prefix
        <> "_anon_"
        <> int.to_string(state.local.anon_fn_count)

      let assert Ok(signature) =
        signature_encoder.encode_signature(
          exp_params,
          exp_return,
          state.local.global.modules,
        )

      let #(fn_name, compiled) = case state.compiled {
        wat.RefFunc(fn_name) -> #(
          Some(fn_name),
          wat.StructNew("closure:" <> signature, [
            wat.RefNull(wat.Direct(wat.Any)),
            wat.RefFunc(wrapper_name),
          ]),
        )
        wat.LocalGet(_) as code -> #(
          None,
          wat.StructNew("closure:" <> signature, [
            code,
            wat.RefFunc(wrapper_name),
          ]),
        )
        code -> {
          io.debug(code)
          todo
        }
      }

      let optional_box_return = fn(state, code) {
        case box_return {
          Some(#(boxer, _)) -> generate_call_import(state, boxer, [code])
          None -> #(state, code)
        }
      }

      let assert Ok(act_signature) =
        signature_encoder.encode_signature(
          act_params,
          act_return,
          state.local.global.modules,
        )

      let #(local_state, wrapper_code) = {
        let #(local_state, args) =
          list.index_map(boxing, fn(boxer, index) {
            #(boxer, wat.LocalGet(p(index)))
          })
          |> list.fold_right(#(state.local, []), fn(acc, arg) {
            let #(state, args) = acc
            let #(boxer, getter) = arg
            let #(state, code) = case boxer {
              Some(#(_, unbox)) -> generate_call_import(state, unbox, [getter])
              None -> #(state, getter)
            }
            #(state, [code, ..args])
          })

        case fn_name {
          Some(fn_name) -> {
            let #(local_state, return) =
              wat.Call(fn_name, args)
              |> optional_box_return(local_state, _)
            #(local_state, [return])
          }
          None -> {
            let #(local_state, return) =
              call_closure("@ctx", act_signature, args)
              |> optional_box_return(local_state, _)
            #(local_state, [
              wat.LocalSet(
                "@ctx",
                wat.RefCast(
                  "closure:" <> act_signature,
                  wat.RefAsNonNull(wat.LocalGet("@ctx_any")),
                ),
              ),
              return,
            ])
          }
        }
      }

      let wrapper =
        wat.FunctionDefinition(
          wrapper_name,
          [
            wat.WatVariableDefinition(
              "@ctx_any",
              wat.NullRef(wat.Direct(wat.Any)),
            ),
            ..list.index_map(exp_params, fn(param, index) {
              wat.WatVariableDefinition(
                p(index),
                type_compiler.compile_type(state.local.global, param)
                  |> type_compiler_hack,
              )
            })
          ],
          type_compiler.compile_type(state.local.global, exp_return)
            |> type_compiler_hack,
          case fn_name {
            Some(_) -> []
            None -> [
              wat.WatVariableDefinition(
                "@ctx",
                wat.Ref(wat.Id("closure:" <> act_signature)),
              ),
            ]
          },
          wrapper_code,
        )

      let wrapper_types =
        dict.from_list([
          #(
            "function:" <> signature,
            wat.Function(wat.WatFunctionType(
              [
                wat.NullRef(wat.Direct(wat.Any)),
                ..list.map(exp_params, fn(param) {
                  type_compiler.compile_type(state.local.global, param)
                  |> type_compiler_hack
                })
              ],
              type_compiler.compile_type(state.local.global, exp_return)
                |> type_compiler_hack,
            )),
          ),
          #(
            "closure:" <> signature,
            wat.Struct([
              wat.WatVariableDefinition(
                "context",
                wat.NullRef(wat.Direct(wat.Any)),
              ),
              wat.WatVariableDefinition(
                "function",
                wat.Ref(wat.Id("function:" <> signature)),
              ),
            ]),
          ),
        ])

      compiler.ExpressionState(
        ..state,
        compiled: compiled,
        local: compiler.LocalState(
          ..local_state,
          anon_fn_count: state.local.anon_fn_count + 1,
          global: compiler.GlobalState(
            ..local_state.global,
            defined_types: dict.merge(
              state.local.global.defined_types,
              wrapper_types,
            ),
            compiled_functions: [
              wrapper,
              ..state.local.global.compiled_functions
            ],
          ),
        ),
        tail: False,
      )
    }
  }
}

fn match_type(
  expr: Expression,
  exp_type: Type,
  act_type: Type,
  mapping: Dict(String, Type),
) -> Result(Dict(String, Type), CompilerError) {
  let check = fn(expected_type: Type) {
    case act_type == expected_type {
      True -> Ok(mapping)
      False -> Error(compiler.TypeError(expr, expected_type, act_type))
    }
  }
  case exp_type {
    glance.VariableType(name) ->
      case dict.get(mapping, name) {
        Ok(assigned_type) -> check(assigned_type)
        _ -> Ok(dict.insert(mapping, name, act_type))
      }
    glance.NamedType(exp_name, exp_module, exp_params) -> {
      case act_type {
        glance.NamedType(act_name, act_module, act_params)
          if act_name == exp_name && act_module == exp_module
        -> {
          case list.length(act_params) == list.length(exp_params) {
            True -> Ok(list.map2(exp_params, act_params, fn(e, a) { #(e, a) }))
            False -> Error(compiler.TypeError(expr, exp_type, act_type))
          }
          |> result.try(list.try_fold(
            _,
            mapping,
            fn(mapping, item: #(Type, Type)) {
              match_type(expr, item.0, item.1, mapping)
            },
          ))
        }
        _ -> Error(compiler.TypeError(expr, exp_type, act_type))
      }
    }
    glance.FunctionType(exp_params, exp_return) -> {
      case act_type {
        glance.FunctionType(act_params, act_return) -> {
          case list.length(act_params) == list.length(exp_params) {
            True ->
              Ok(
                list.map2(
                  [exp_return, ..exp_params],
                  [act_return, ..act_params],
                  fn(e, a) { #(e, a) },
                ),
              )
            False -> Error(compiler.TypeError(expr, exp_type, act_type))
          }
          |> result.try(list.try_fold(
            _,
            mapping,
            fn(mapping, item: #(Type, Type)) {
              match_type(expr, item.0, item.1, mapping)
            },
          ))
        }
        _ -> Error(compiler.TypeError(expr, exp_type, act_type))
      }
    }
    _ -> Error(compiler.TypeError(expr, exp_type, act_type))
  }
}

fn check_type(
  expr: Expression,
  state: ExpressionState,
  expected_type: Type,
) -> Result(ExpressionState, CompilerError) {
  match_type(expr, expected_type, state.type_, state.local.variable_types)
  |> result.map(fn(mapping) {
    compiler.ExpressionState(
      ..state,
      local: compiler.LocalState(..state.local, variable_types: mapping),
    )
  })
  |> result.map(fn(state) {
    case expected_type {
      glance.VariableType(_) -> box_primitive(state)
      glance.FunctionType(params, return) ->
        wrap_primitive_function(state, params, return)
      _ -> state
    }
  })
}

fn check_typed_expressions(
  state: LocalState,
  exps: List(#(Expression, Type)),
) -> Result(#(LocalState, List(wat.WatExpression)), compiler.CompilerError) {
  list.try_fold(exps, #(state, []), fn(acc, item) {
    let #(state, compiled) = acc
    let #(expr, type_) = item
    compile_expression(expr, state, False)
    |> result.try(check_type(expr, _, type_))
    |> result.map(fn(expr_state) {
      #(expr_state.local, [expr_state.compiled, ..compiled])
    })
  })
  |> result.map(fn(r) {
    let #(state, compiled) = r
    #(state, list.reverse(compiled))
  })
}

fn prepend_boolean_condition(
  test_expr: wat.WatExpression,
  true_expr: wat.WatExpression,
) -> wat.WatExpression {
  case true_expr {
    wat.Int32Const(1) -> test_expr
    _ -> wat.If(test_expr, true_expr, Some(wat.Int32Const(0)))
  }
}

fn compile_pattern(
  subject: wat.WatExpression,
  subject_type: Type,
  pattern: glance.Pattern,
  acc_initializers: List(wat.WatExpression),
  true_expr: wat.WatExpression,
  state: LocalState,
) -> Result(
  #(LocalState, List(wat.WatExpression), wat.WatExpression),
  CompilerError,
) {
  case pattern {
    glance.PatternInt(value) ->
      parse_int(value)
      |> result.map(fn(expr) { #(state, [], wat.Int64Equal(subject, expr)) })
    glance.PatternDiscard(_) -> Ok(#(state, [], wat.Int32Const(1)))
    glance.PatternVariable(name) -> {
      Ok(#(
        compiler.with_locals(state, [#(name, subject_type)]),
        [wat.LocalSet(name, subject)],
        wat.Int32Const(1),
      ))
    }
    glance.PatternList([], None) -> {
      let #(state, call) =
        generate_call_import(state, compiler.import_list_is_empty, [subject])
      Ok(#(state, [], call))
    }
    glance.PatternList([head_pattern], Some(tail_pattern)) -> {
      let assert glance.NamedType("List", None, [item_type]) = subject_type

      let #(state, call) =
        generate_call_import(state, compiler.import_list_head, [subject])
      use #(state, initializers, compiled) <- result.try(compile_pattern(
        call,
        item_type,
        head_pattern,
        [],
        wat.Int32Const(1),
        state,
      ))
      let #(state, call) =
        generate_call_import(state, compiler.import_list_tail, [subject])
      use #(state, initializers, compiled) <- result.map(compile_pattern(
        call,
        subject_type,
        tail_pattern,
        initializers,
        compiled,
        state,
      ))
      let #(state, call) =
        generate_call_import(state, compiler.import_list_is_empty, [subject])
      #(
        state,
        initializers,
        prepend_boolean_condition(wat.Int32EqualZero(call), compiled),
      )
    }
    _ -> {
      io.debug(pattern)
      todo
    }
  }
  |> result.map(fn(res) {
    let #(state, initializers, expr) = res
    let initializers = list.append(acc_initializers, initializers)
    #(state, initializers, prepend_boolean_condition(expr, true_expr))
  })
}

fn resolve_types(a: Type, b: Type) -> Result(Type, Nil) {
  case a, b {
    a, b if a == b -> Ok(a)
    a, glance.HoleType(_) -> Ok(a)
    glance.HoleType(_), b -> Ok(b)
    glance.NamedType(na, ma, pa), glance.NamedType(nb, mb, pb)
      if na == nb && ma == mb
    ->
      list.map2(pa, pb, fn(a, b) { #(a, b) })
      |> list.try_map(fn(pair) { resolve_types(pair.0, pair.1) })
      |> result.try(fn(params) {
        case list.length(pa) == list.length(pb) {
          True -> Ok(glance.NamedType(na, ma, params))
          False -> Error(Nil)
        }
      })
    _, _ -> Error(Nil)
  }
}

fn compile_clause(
  previous_state: ExpressionState,
  clause: Clause,
) -> Result(ExpressionState, CompilerError) {
  case clause {
    glance.Clause([patterns], None, body) -> {
      let state = previous_state.local
      let condition =
        list.index_map(patterns, fn(pattern, index) {
          let name =
            "@case_"
            <> int.to_string(state.case_count)
            <> "_"
            <> int.to_string(index)
          let assert Ok(#(_, type_)) = compiler.get_variable(state, name)
          #(name, type_, pattern)
        })
        |> list.reverse
        |> list.try_fold(#(state, [], wat.Int32Const(1)), fn(acc, elem) {
          let #(subject_name, subject_type, pattern) = elem
          let #(state, initializers, compiled) = acc
          compile_pattern(
            wat.LocalGet(subject_name),
            subject_type,
            pattern,
            initializers,
            compiled,
            state,
          )
        })
      use #(state, initializers, compiled_condition) <- result.try(condition)
      use compiler.ExpressionState(state, compiled_body, body_type, tail) <- result.map(
        compile_expression(body, state, previous_state.tail)
        |> result.try(fn(expr) {
          resolve_types(previous_state.type_, expr.type_)
          |> result.map(fn(type_) {
            compiler.ExpressionState(
              expr.local,
              expr.compiled,
              type_,
              expr.tail,
            )
          })
          |> result.map_error(fn(_) {
            compiler.TypeError(body, previous_state.type_, expr.type_)
          })
        }),
      )
      compiler.ExpressionState(
        state,
        wat.If(
          compiled_condition,
          compiled_body
            |> fn(expr) {
              case initializers {
                [] -> expr
                _ -> wat.Block(list.append(initializers, [expr]))
              }
            },
          Some(previous_state.compiled),
        ),
        body_type,
        tail,
      )
    }
    _ -> todo
  }
}

pub fn parse_int(str: String) -> Result(wat.WatExpression, CompilerError) {
  int.parse(str)
  |> result.map(wat.Int64Const)
  |> result.map_error(fn(_) {
    compiler.SyntaxError("Invalid Int value: " <> str)
  })
}

pub fn parse_float(str: String) -> Result(wat.WatExpression, CompilerError) {
  float.parse(str)
  |> result.map(wat.Float64Const)
  |> result.map_error(fn(_) {
    compiler.SyntaxError("Invalid Float value: " <> str)
  })
}

pub fn compile_expression(
  expr: Expression,
  state: LocalState,
  tail: Bool,
) -> Result(ExpressionState, CompilerError) {
  case expr {
    glance.Int(val) ->
      parse_int(val)
      |> result.map(compiler.ExpressionState(state, _, compiler.int_type, tail))
    glance.Float(val) ->
      parse_float(val)
      |> result.map(compiler.ExpressionState(
        state,
        _,
        compiler.float_type,
        tail,
      ))
    glance.Variable(name) ->
      case
        compiler.get_variable(state, name),
        dict.get(state.global.defined_functions, name)
      {
        Ok(#(_, type_)), _ ->
          Ok(compiler.ExpressionState(state, wat.LocalGet(name), type_, tail))
        _, Ok(type_) ->
          Ok(compiler.ExpressionState(state, wat.RefFunc(name), type_, tail))
        _, _ -> Error(compiler.ReferenceError(name))
      }
    glance.Block(body) -> {
      let vars = state.in_scope_variables
      use #(state, exprs) <- result.map(compile_body(body, state))
      // TODO: proper typing
      compiler.ExpressionState(
        compiler.LocalState(..state, in_scope_variables: vars),
        wat.Block(exprs),
        glance.HoleType(""),
        tail,
      )
    }
    glance.List([], None) ->
      Ok(compiler.ExpressionState(
        state,
        wat.Call("gleam/list_empty", []),
        compiler.list_type(glance.HoleType("")),
        tail,
      ))
    glance.Call(glance.Variable(name), args) -> {
      case
        compiler.get_variable(state, name),
        dict.get(state.global.defined_functions, name)
      {
        Ok(#(_, type_)), _ ->
          case type_ {
            glance.FunctionType(params, return) -> {
              let assert Ok(signature) =
                signature_encoder.encode_signature(
                  params,
                  return,
                  state.global.modules,
                )
              Ok(#(call_closure(name, signature, _), type_))
            }
            _ -> Error(compiler.NotAFunctionError(glance.Variable(name)))
          }
        _, Ok(type_) -> Ok(#(fn(params) { wat.Call(name, params) }, type_))
        _, _ -> Error(compiler.ReferenceError(name))
      }
      |> result.try(fn(res) {
        let #(maker, fn_type) = res
        let assert glance.FunctionType(params, return) = fn_type
        compile_function_call(name, args, maker, params, return, state, tail)
      })
    }
    glance.Case(subjects, clauses) -> {
      list.try_fold(subjects, #(state, []), fn(acc, subject) {
        let #(state, initializers) = acc
        compile_expression(subject, state, False)
        |> result.map(fn(res) {
          #(res.local, [#(res.compiled, res.type_), ..initializers])
        })
      })
      |> result.try(fn(res) {
        let #(state, initializers) = res
        let initializers = list.reverse(initializers)
        let #(locals, compiled_initializers) =
          list.index_map(initializers, fn(elem, index) {
            let #(init, type_) = elem
            let name =
              "@case_"
              <> int.to_string(state.case_count)
              <> "_"
              <> int.to_string(index)
            let compiled = wat.LocalSet(name, init)
            #(#(name, type_), compiled)
          })
          |> list.unzip
        let state = compiler.with_locals(state, locals)
        list.reverse(clauses)
        |> list.try_fold(
          compiler.ExpressionState(
            state,
            wat.Unreachable,
            glance.HoleType(""),
            tail,
          ),
          compile_clause,
        )
        |> result.map(fn(state) {
          compiler.ExpressionState(
            compiler.LocalState(
              ..state.local,
              case_count: state.local.case_count + 1,
            ),
            wat.Block(list.append(compiled_initializers, [state.compiled])),
            state.type_,
            False,
          )
        })
      })
    }
    glance.BinaryOperator(glance.AddInt, left, right) ->
      check_typed_expressions(state, [
        #(left, compiler.int_type),
        #(right, compiler.int_type),
      ])
      |> result.map(fn(compiled) {
        case compiled {
          #(state, [left, right]) ->
            compiler.ExpressionState(
              state,
              wat.Int64Add(left, right),
              compiler.int_type,
              tail,
            )
          _ -> panic
        }
      })
    _ -> {
      io.debug(expr)
      todo
    }
  }
  |> result.map(fn(expr_state) {
    case expr_state {
      compiler.ExpressionState(_, compiled, _, True) ->
        compiler.ExpressionState(..expr_state, compiled: wat.Return(compiled))
      _ -> expr_state
    }
  })
}

fn compile_function_call(
  name: String,
  args,
  maker,
  params,
  return,
  state: LocalState,
  tail: Bool,
) -> Result(ExpressionState, compiler.CompilerError) {
  case list.length(params) == list.length(args) {
    True ->
      list.map2(args, params, fn(arg, param) { #(arg.item, param) })
      |> check_typed_expressions(state, _)
      |> result.try(fn(checked) {
        let #(state, args) = checked
        let outer_state =
          compiler.LocalState(..state, variable_types: dict.new())
        case return {
          glance.VariableType(name) ->
            dict.get(state.variable_types, name)
            |> result.map(fn(return) {
              compiler.ExpressionState(outer_state, maker(args), return, tail)
              |> unbox_primitive(return)
            })
            |> result.map_error(fn(_) { compiler.ReferenceError(name) })
          _ ->
            Ok(compiler.ExpressionState(outer_state, maker(args), return, tail))
        }
      })
    False ->
      Error(compiler.ArityError(
        glance.Variable(name),
        list.length(params),
        list.length(args),
      ))
  }
}
