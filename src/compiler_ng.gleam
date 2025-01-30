import analysis
import compiler
import filepath
import gl_wasm/wasm
import glance
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import graph
import pprint
import prelude
import project
import simplifile

pub fn main() {
  impl() |> pprint.debug
}

pub fn impl() {
  // TODO: maybe topologically sort the packages, and limit import graph to package boundary?
  use project <- result.try(project.scan_project("../gl_example", "wasm"))
  use _ <- result.try(output_module(
    project,
    project.ModuleId("gleam", "gleam"),
    prelude.build_prelude(),
  ))
  use modules <- result.try(
    analysis.analyze_project_imports(project)
    |> result.try(fn(graph) {
      let #(nodes, edges) = graph
      graph.topological_sort(nodes, edges)
      |> result.replace_error(compiler.CircularDependencyError)
    }),
  )
  list.try_fold(modules, dict.new(), fn(modules, module_id) {
    use module <- result.try(analysis.analyze_module(
      project,
      module_id,
      modules,
    ))
    use _ <- result.map(compile_module(project, module))
    let module = analysis.module_from_internals(module)
    dict.insert(modules, module_id, module)
  })
}

/// Type for mapping to WASM -- stripped generics
type ProjectedType {
  TypeId(analysis.TypeId)
  FunctionType(List(ProjectedType))
  HoleType
}

type TypeRegistry =
  Dict(ProjectedType, Int)

pub fn compile_module(
  project: project.Project,
  module: analysis.ModuleInternals,
) {
  let mb = wasm.create_module_builder(Some(module.location.module_path))

  use mb <- result.try(
    prelude.add_prelude_types(mb)
    |> result.replace_error(compiler.AnotherTypeError(
      "Failed to add prelude types",
    )),
  )

  let types =
    dict.from_list([
      #(TypeId(analysis.BuiltInType(analysis.IntType)), 0),
      #(TypeId(analysis.BuiltInType(analysis.FloatType)), 1),
      #(TypeId(analysis.BuiltInType(analysis.BoolType)), 2),
      #(TypeId(analysis.BuiltInType(analysis.NilType)), 3),
      #(TypeId(analysis.BuiltInType(analysis.ListType)), 4),
      #(TypeId(analysis.BuiltInType(analysis.StringType)), 6),
      #(TypeId(analysis.BuiltInType(analysis.BitArrayType)), 7),
      #(TypeId(analysis.TypeFromModule(analysis.gleam_module_id, "Result")), 8),
    ])

  // Generate types
  // TODO: types for anonymous functions
  use #(mb, types) <- result.try(
    list.try_fold(
      over: list.flatten(module.implemented_functions),
      from: #(mb, types),
      with: fn(acc, func) {
        let #(mb, types) = acc
        register_func_types(mb, types, func)
      },
    ),
  )

  pprint.debug(types)

  // TODO: generate imports

  // TODO: wrap functions passed as arguments in a closure.
  // this closure must also cast any generic arguments to the specific as relevant
  // e.g. fn(Int, Int) -> Int when passing into fold becomes struct(any, fn(any, any, any) -> any)

  let functions =
    list.flatten(module.implemented_functions)
    |> list.index_map(fn(func, index) { #(func.signature.name, index) })
    |> dict.from_list

  // generate functions
  // TODO: mutually recursive functions
  use mb <- result.try(
    list.try_fold(
      over: list.flatten(module.implemented_functions),
      from: mb,
      with: fn(mb, func) {
        compile_function(mb, types, functions, module, func)
      },
    ),
  )

  // TODO: generate exports

  output_module(project, module.location, mb)
}

fn register_func_types(
  mb: wasm.ModuleBuilder,
  registry: TypeRegistry,
  func: analysis.Function,
) -> Result(#(wasm.ModuleBuilder, TypeRegistry), compiler.CompilerError) {
  let projected_type = project_type(analysis.signature_type(func.signature))
  register_type(mb, registry, projected_type)
  |> result.map(fn(triple) {
    let #(mb, registry, _) = triple
    #(mb, registry)
  })
}

fn register_type(
  mb: wasm.ModuleBuilder,
  registry: TypeRegistry,
  t: ProjectedType,
) -> Result(
  #(wasm.ModuleBuilder, TypeRegistry, Option(Int)),
  compiler.CompilerError,
) {
  use _ <- result.try_recover(
    dict.get(registry, t)
    |> result.map(fn(i) { #(mb, registry, Some(i)) }),
  )
  case t {
    FunctionType(args) ->
      analysis.try_map_fold(args, #(mb, registry), fn(acc, arg) {
        let #(mb, registry) = acc
        register_type(mb, registry, arg)
        |> result.map(fn(triple) {
          let #(mb, registry, arg) = triple
          #(#(mb, registry), arg)
        })
      })
      |> result.try(fn(acc) {
        let #(#(mb, registry), types) = acc
        let types =
          list.map(types, fn(t) {
            case t {
              None -> wasm.AbstractAny
              Some(i) -> wasm.ConcreteType(i)
            }
            |> wasm.NonNull
            |> wasm.Ref
          })
        let func_type = case types {
          [] -> wasm.Func(None, [], [])
          [return, ..params] -> wasm.Func(None, params, [return])
        }
        wasm.add_type(mb, func_type)
        |> result.map(fn(res) {
          let #(mb, i) = res
          let registry = dict.insert(registry, t, i)
          #(mb, registry, Some(i))
        })
        |> result.map_error(compiler.AnotherTypeError)
      })
    HoleType -> Ok(#(mb, registry, None))
    TypeId(_) -> todo
  }
}

pub fn output_module(
  project: project.Project,
  module_id: project.ModuleId,
  mb: wasm.ModuleBuilder,
) -> Result(String, compiler.CompilerError) {
  let wasm_file_name =
    filepath.join(
      project.package_build_path(project, module_id.package_name),
      module_id.module_path,
    )
    <> ".wasm"
  let wasm_dir_name = filepath.directory_name(wasm_file_name)
  use _ <- result.try(
    simplifile.create_directory_all(wasm_dir_name)
    |> result.map_error(compiler.FileError(wasm_dir_name, _)),
  )

  // TODO: properly return errors
  wasm.emit_module(mb, file_output_stream(wasm_file_name))
  |> pprint.debug
  |> result.replace_error(compiler.AnotherTypeError("Hmm"))
}

fn project_type(t: analysis.Type) -> ProjectedType {
  case t {
    analysis.FunctionType(params, return) ->
      FunctionType(list.map([return, ..params], project_type))
    analysis.TypeConstructor(type_id, _) -> TypeId(type_id)
    analysis.TypeVariable(_) -> HoleType
  }
}

type LocalState {
  LocalState(
    builder: wasm.CodeBuilder,
    registry: TypeRegistry,
    functions: Dict(String, Int),
    locals: Dict(String, Int),
  )
}

fn compile_function(
  mb: wasm.ModuleBuilder,
  registry: TypeRegistry,
  functions: Dict(String, Int),
  _internals: analysis.ModuleInternals,
  function: analysis.Function,
) -> Result(wasm.ModuleBuilder, compiler.CompilerError) {
  let projected_type = project_type(analysis.signature_type(function.signature))
  use type_index <- result.try(
    dict.get(registry, projected_type)
    |> result.replace_error(compiler.AnotherTypeError("Function type lost?")),
  )
  io.println(
    "Create function "
    <> function.signature.name
    <> " of type "
    <> int.to_string(type_index),
  )
  use #(mb, fb) <- result.try(
    wasm.create_function_builder(
      mb,
      wasm.FunctionSignature(
        type_index,
        Some(function.signature.name),
        Some(
          list.map(function.argument_names, fn(ass_name) {
            case ass_name {
              glance.Named(name) -> name
              glance.Discarded(name) -> name
            }
          }),
        ),
      ),
    )
    |> result.map_error(compiler.AnotherTypeError),
  )
  io.debug("We're doing a function..." <> function.signature.name)
  let state =
    with_params(
      LocalState(fb, registry, functions, dict.new()),
      function.argument_names,
    )
  use state <- result.try(case function.body {
    analysis.External(_, _) -> todo
    analysis.GleamBody(statements) ->
      list.try_fold(over: statements, from: state, with: compile_statement)
  })
  io.debug("Going to end function " <> function.signature.name)
  use state <- result.try(add_instruction(state, wasm.End))
  let res =
    wasm.finalize_function(mb, state.builder)
    |> result.map_error(compiler.AnotherTypeError)
  io.debug("Ended function")
  res
}

fn with_params(
  state: LocalState,
  argument_names: List(glance.AssignmentName),
) -> LocalState {
  let params =
    list.index_map(argument_names, fn(ass_name, index) {
      case ass_name {
        glance.Named(name) -> Ok(#(name, index))
        glance.Discarded(_) -> Error(Nil)
      }
    })
    |> result.values
    |> dict.from_list
  LocalState(..state, locals: dict.merge(state.locals, params))
}

fn compile_statement(
  state: LocalState,
  stmt: analysis.Statement,
) -> Result(LocalState, compiler.CompilerError) {
  io.debug("-- Compile statement")
  let res = case stmt {
    analysis.Assignment(
      glance.Let,
      analysis.PatternWithVariables(pattern, variables),
      value,
    ) -> {
      use state <- result.try(compile_expression(state, value))
      use state <- result.try(create_variables(state, variables))
      compile_pattern(state, pattern)
    }
    analysis.Assignment(_, _, _) -> todo
    analysis.Expression(expr) -> compile_expression(state, expr)
  }
  io.debug("-- Compiled statement")
  res
}

fn add_local(
  state: LocalState,
  local_type: analysis.Type,
  local_name: Option(String),
) -> Result(#(LocalState, Int), compiler.CompilerError) {
  use local_type <- result.try(ref_type(state, local_type))
  wasm.add_local(state.builder, wasm.Ref(local_type), local_name)
  |> result.map_error(compiler.AnotherTypeError)
  |> result.map(fn(res) {
    let #(builder, index) = res
    #(
      LocalState(..state, builder:, locals: case local_name {
        None -> state.locals
        Some(name) -> dict.insert(state.locals, name, index)
      }),
      index,
    )
  })
}

fn ref_type(
  state: LocalState,
  t: analysis.Type,
) -> Result(wasm.RefType, compiler.CompilerError) {
  let projected_type = project_type(t)
  case projected_type {
    HoleType -> Ok(wasm.NonNull(wasm.AbstractAny))
    _ -> {
      dict.get(state.registry, projected_type)
      |> result.replace_error(compiler.AnotherTypeError("Type lost?"))
      |> result.map(fn(type_index) {
        wasm.NonNull(wasm.ConcreteType(type_index))
      })
    }
  }
}

fn get_type_index(
  state: LocalState,
  t: analysis.Type,
) -> Result(Int, compiler.CompilerError) {
  let projected_type = project_type(t)
  dict.get(state.registry, projected_type)
  |> result.replace_error(compiler.AnotherTypeError("Type lost?"))
}

fn add_instruction(
  state: LocalState,
  instr: wasm.Instruction,
) -> Result(LocalState, compiler.CompilerError) {
  use builder <- result.map(
    wasm.add_instruction(state.builder, instr)
    |> result.map_error(compiler.AnotherTypeError),
  )
  LocalState(..state, builder:)
}

fn compile_expression(
  state: LocalState,
  expr: analysis.Expression,
) -> Result(LocalState, compiler.CompilerError) {
  case expr {
    analysis.Call(analysis.Fn(_, [], block), []) -> {
      io.debug("Compiling block")
      let previous_locals = state.locals
      use state <- result.map(list.try_fold(
        over: block,
        from: state,
        with: compile_statement,
      ))
      io.debug("Compiled block")
      LocalState(..state, locals: previous_locals)
    }
    analysis.Call(fun, args) -> {
      io.debug("Compiling call")
      use state <- result.try(
        list.try_fold(over: args, from: state, with: case unbox_args(fun) {
          False -> compile_expression
          True -> compile_expression_unboxed
        }),
      )
      let res = case fun {
        analysis.FunctionReference(_, analysis.BuiltInFunction(builtin)) ->
          compile_builtin(state, builtin)
        analysis.FunctionReference(_, analysis.FunctionFromModule(_, name)) -> {
          // TODO: imported functions
          use index <- result.try(get_function_index(state, name))
          io.debug("Going to call a function")
          let res = add_instruction(state, wasm.Call(index))
          io.debug("Got here")
          res
        }
        analysis.Variable(fn_type, name) -> {
          use local_index <- result.try(get_local_index(state, name))
          use type_index <- result.try(get_type_index(state, fn_type))
          use state <- result.try(add_instruction(
            state,
            wasm.LocalGet(local_index),
          ))
          add_instruction(state, wasm.CallRef(type_index))
        }
        _ -> {
          use state <- result.try(compile_expression(state, fun))
          pprint.debug(fun)
          todo
        }
      }
      io.debug("Compiled call")
      res
    }
    analysis.Case(typ:, subjects:, clauses:) -> {
      // allocate locals for the subjects
      io.debug("Compiling case")
      use #(state, indices) <- result.try(
        analysis.try_map_fold(
          over: subjects,
          from: state,
          with: fn(state, subj) {
            add_local(state, type_of_expression(subj), None)
          },
        ),
      )
      use state <- result.try(
        list.try_fold(
          over: list.zip(subjects, indices),
          from: state,
          with: fn(state, local) {
            let #(subj, index) = local
            use state <- result.try(compile_expression(state, subj))
            add_instruction(state, wasm.LocalSet(index))
          },
        ),
      )
      use block_type <- result.try(ref_type(state, typ))
      use state <- result.try(add_instruction(
        state,
        wasm.Block(wasm.BlockValue(wasm.Ref(block_type))),
      ))
      use state <- result.try(
        list.try_fold(over: clauses, from: state, with: fn(state, clause) {
          compile_clause(state, indices, clause)
        }),
      )
      use state <- result.try(list.try_fold(
        over: [wasm.Unreachable, wasm.End],
        from: state,
        with: add_instruction,
      ))
      io.debug("Compiled case")
      Ok(state)
    }
    analysis.Float(_) -> todo
    analysis.Fn(_, _, _) -> {
      pprint.debug(expr)
      todo
    }
    analysis.FunctionReference(_, analysis.FunctionFromModule(_, name)) -> {
      // TODO: imported functions!
      use index <- result.try(get_function_index(state, name))
      add_instruction(state, wasm.RefFunc(index))
    }
    analysis.FunctionReference(_, analysis.BuiltInFunction(_)) -> todo
    analysis.Int(value) -> {
      use value <- result.try(
        int.parse(value)
        |> result.replace_error("Not a valid Int: " <> value)
        |> result.try(wasm.int64_signed)
        |> result.map_error(compiler.AnotherTypeError),
      )
      list.try_fold(
        over: [wasm.I64Const(value), wasm.StructNew(prelude.int_index)],
        from: state,
        with: add_instruction,
      )
    }
    analysis.String(_) -> todo
    analysis.Trap(_, _, _) -> todo
    analysis.Variable(_, name) -> {
      use index <- result.try(get_local_index(state, name))
      add_instruction(state, wasm.LocalGet(index))
    }
  }
}

fn get_local_index(
  state: LocalState,
  name: String,
) -> Result(Int, compiler.CompilerError) {
  dict.get(state.locals, name)
  |> result.replace_error(compiler.AnotherTypeError(
    "Could not find local " <> name,
  ))
}

fn get_function_index(
  state: LocalState,
  name: String,
) -> Result(Int, compiler.CompilerError) {
  dict.get(state.functions, name)
  |> result.replace_error(compiler.AnotherTypeError(
    "Could not find function " <> name,
  ))
}

fn compile_clause(
  state: LocalState,
  subjects: List(Int),
  clause: analysis.Clause,
) -> Result(LocalState, compiler.CompilerError) {
  let analysis.Clause(patterns:, variables:, guard:, body:) = clause
  // mint locals for the variables
  let previous_locals = state.locals
  use state <- result.try(create_variables(state, variables))
  // open clause block
  use state <- result.try(add_instruction(state, wasm.Block(wasm.BlockEmpty)))
  // open pattern matching block
  use state <- result.try(add_instruction(state, wasm.Block(wasm.BlockEmpty)))
  // compile each alternative pattern
  use state <- result.try(
    list.try_fold(over: patterns, from: state, with: fn(state, pattern) {
      // open pattern block 
      use state <- result.try(add_instruction(
        state,
        wasm.Block(wasm.BlockEmpty),
      ))
      // pattern breaks out of the block if it fails to match
      use state <- result.try(compile_case_patterns(state, subjects, pattern))
      // this pattern matches; break out of pattern matching
      use state <- result.try(add_instruction(state, wasm.Break(1)))
      // close pattern block
      add_instruction(state, wasm.End)
    }),
  )
  // none of the alternative patterns matched; break out of the clause
  use state <- result.try(add_instruction(state, wasm.Break(1)))
  // close pattern matching block
  use state <- result.try(add_instruction(state, wasm.End))

  // clause guard expression
  io.debug("guard")
  use state <- result.try(compile_expression(state, guard))
  // if guard == false, break out of the clause
  io.debug("it was the bool!")
  use state <- result.try(add_instruction(
    state,
    wasm.StructGet(prelude.bool_index, 0),
  ))
  io.debug("it was the bool!")
  use state <- result.try(add_instruction(state, wasm.I32EqZ))
  use state <- result.try(add_instruction(state, wasm.BreakIf(0)))

  // body expression
  io.debug("body")
  use state <- result.try(compile_expression(state, body))
  pprint.debug(state.builder)
  // break out of the clause returning the result
  io.debug("/body")
  use state <- result.try(add_instruction(state, wasm.Break(1)))

  // close clause block
  io.debug("end block")
  use state <- result.try(add_instruction(state, wasm.Unreachable))
  use state <- result.try(add_instruction(state, wasm.End))
  io.debug("/clause")
  Ok(LocalState(..state, locals: previous_locals))
}

fn create_variables(state: LocalState, variables: Dict(String, analysis.Type)) {
  list.try_fold(
    over: dict.to_list(variables),
    from: state,
    with: fn(state, var) {
      let #(name, typ) = var
      add_local(state, typ, Some(name))
      |> result.map(pair.first)
    },
  )
}

fn compile_case_patterns(
  state: LocalState,
  subjects: List(Int),
  patterns: List(analysis.Pattern),
) -> Result(LocalState, compiler.CompilerError) {
  list.zip(subjects, patterns)
  |> list.try_fold(from: state, with: fn(state, pair) {
    let #(subject, pattern) = pair
    use state <- result.try(add_instruction(state, wasm.LocalGet(subject)))
    use state <- result.try(compile_pattern(state, pattern))
    use state <- result.try(add_instruction(state, wasm.I32EqZ))
    use state <- result.try(add_instruction(state, wasm.BreakIf(0)))
    Ok(state)
  })
}

fn compile_pattern(
  state: LocalState,
  pattern: analysis.Pattern,
) -> Result(LocalState, compiler.CompilerError) {
  let assert Ok(zero) = wasm.int32_unsigned(0)
  let assert Ok(one) = wasm.int32_unsigned(1)
  case pattern {
    analysis.PatternAssignment(_, _) -> todo
    analysis.PatternBitString -> todo
    analysis.PatternConcatenate(_, _) -> todo
    analysis.PatternConstructor(_, _, _) -> todo
    analysis.PatternDiscard(_) -> todo
    analysis.PatternFloat(_) -> todo
    analysis.PatternInt(_) -> todo
    analysis.PatternList(analysis.PatternEmpty) -> {
      add_instruction(
        state,
        wasm.RefTest(wasm.NonNull(wasm.ConcreteType(prelude.list_index + 1))),
      )
    }
    analysis.PatternList(analysis.PatternTail(pattern)) -> {
      compile_pattern(state, pattern)
    }
    analysis.PatternList(analysis.PatternNonEmpty(head, tail)) -> {
      use #(state, index) <- result.try(add_local(
        state,
        analysis.list_type(analysis.TypeVariable("a")),
        None,
      ))
      // Check for non-empty list, then apply head pattern
      // TODO: use br_on_cast instead of ref.test?
      use state <- result.try(list.try_fold(
        [
          wasm.LocalSet(index),
          wasm.LocalGet(index),
          wasm.RefTest(wasm.NonNull(wasm.ConcreteType(prelude.list_index + 2))),
          wasm.If(wasm.BlockValue(wasm.I32)),
          wasm.LocalGet(index),
          wasm.RefCast(wasm.NonNull(wasm.ConcreteType(prelude.list_index + 2))),
          wasm.StructGet(prelude.list_index + 2, 1),
        ],
        state,
        add_instruction,
      ))
      use state <- result.try(compile_pattern(state, head))
      // Apply tail pattern if head pattern OK
      use state <- result.try(list.try_fold(
        [
          wasm.If(wasm.BlockValue(wasm.I32)),
          wasm.LocalGet(index),
          wasm.RefCast(wasm.NonNull(wasm.ConcreteType(prelude.list_index + 2))),
          wasm.StructGet(prelude.list_index + 2, 0),
        ],
        state,
        add_instruction,
      ))
      use state <- result.try(compile_pattern(state, analysis.PatternList(tail)))
      // Close out blocks, returning 0 in else branches
      let res =
        list.try_fold(
          [
            wasm.Else,
            wasm.I32Const(zero),
            wasm.End,
            wasm.Else,
            wasm.I32Const(zero),
            wasm.End,
          ],
          state,
          add_instruction,
        )
      res
    }
    analysis.PatternString(_) -> todo
    analysis.PatternTuple(_) -> todo
    analysis.PatternVariable(name) -> {
      use index <- result.try(
        dict.get(state.locals, name)
        |> result.replace_error(compiler.AnotherTypeError(
          "Local for variable " <> name <> " not defined",
        )),
      )
      list.try_fold(
        [wasm.LocalSet(index), wasm.I32Const(one)],
        state,
        add_instruction,
      )
    }
  }
}

// TODO: this sucks!
fn type_of_expression(expr: analysis.Expression) -> analysis.Type {
  case expr {
    analysis.Call(func, _) -> {
      let assert analysis.FunctionType(return:, ..) = type_of_expression(func)
      return
    }
    analysis.Case(typ:, ..)
    | analysis.Fn(typ:, ..)
    | analysis.FunctionReference(typ:, ..)
    | analysis.Trap(typ:, ..)
    | analysis.Variable(typ:, ..) -> typ
    analysis.Float(_) -> analysis.float_type
    analysis.Int(_) -> analysis.int_type
    analysis.String(_) -> analysis.string_type
  }
}

fn compile_expression_unboxed(
  state: LocalState,
  expr: analysis.Expression,
) -> Result(LocalState, compiler.CompilerError) {
  // TODO: need to check which type we have
  use state <- result.try(compile_expression(state, expr))
  add_instruction(state, wasm.StructGet(prelude.int_index, 0))
}

fn unbox_args(fun: analysis.Expression) -> Bool {
  case fun {
    analysis.FunctionReference(_, analysis.BuiltInFunction(builtin)) ->
      case builtin {
        analysis.BinaryOperator(op) ->
          case op {
            glance.AddFloat
            | glance.AddInt
            | glance.And
            | glance.DivFloat
            | glance.DivInt
            | glance.GtEqFloat
            | glance.GtEqInt
            | glance.GtFloat
            | glance.GtInt
            | glance.LtEqFloat
            | glance.LtEqInt
            | glance.LtFloat
            | glance.LtInt
            | glance.MultFloat
            | glance.MultInt
            | glance.Or
            | glance.RemainderInt
            | glance.SubFloat
            | glance.SubInt -> True
            _ -> False
          }
        analysis.NegateBool | analysis.NegateInt -> True
        _ -> False
      }
    _ -> False
  }
}

fn with_builder(state: LocalState, builder: wasm.CodeBuilder) -> LocalState {
  LocalState(..state, builder:)
}

fn compile_builtin(
  state: LocalState,
  builtin: analysis.BuiltInFunction,
) -> Result(LocalState, compiler.CompilerError) {
  let assert Ok(zero) = wasm.int32_signed(0)
  let assert Ok(one) = wasm.int32_signed(1)
  case builtin {
    analysis.AccessField(_, _) -> todo
    analysis.BinaryOperator(op) ->
      case op {
        glance.AddFloat -> todo
        glance.AddInt ->
          add_instruction(state, wasm.I64Add)
          |> result.try(add_instruction(_, wasm.StructNew(prelude.int_index)))
        glance.And -> todo
        glance.Concatenate -> todo
        glance.DivFloat -> todo
        glance.DivInt -> todo
        glance.Eq -> todo
        glance.GtEqFloat -> todo
        glance.GtEqInt -> todo
        glance.GtFloat -> todo
        glance.GtInt -> todo
        glance.LtEqFloat -> todo
        glance.LtEqInt -> todo
        glance.LtFloat -> todo
        glance.LtInt -> todo
        glance.MultFloat -> todo
        glance.MultInt -> todo
        glance.NotEq -> todo
        glance.Or -> todo
        glance.Pipe -> todo
        glance.RemainderInt -> todo
        glance.SubFloat -> todo
        glance.SubInt -> todo
      }
    analysis.BoolConstructor(val) -> {
      let val = case val {
        False -> zero
        True -> one
      }
      add_instruction(state, wasm.I32Const(val))
      |> result.try(add_instruction(_, wasm.StructNew(prelude.bool_index)))
    }
    analysis.EmptyListConstructor -> todo
    analysis.NegateBool -> todo
    analysis.NegateInt -> todo
    analysis.NonEmptyListConstructor -> todo
    analysis.TupleConstructor(_) -> todo
  }
}

fn file_output_stream(fname) {
  let output_stream =
    wasm.OutputStream(
      stream: fname,
      write_bytes: fn(fname, bytes) {
        simplifile.append_bits(fname, bytes)
        |> result.replace(fname)
      },
      close: fn(fname) { Ok(fname) },
    )
  let _ = simplifile.write_bits(fname, <<>>)
  output_stream
}
