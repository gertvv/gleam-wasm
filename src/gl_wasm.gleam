import compiler.{
  type CompilerError, type ExpressionState, type GlobalState, type LocalState,
}
import expression_compiler
import glance.{
  type AssignmentName, type Definition, type Expression, type Function,
  type FunctionParameter, type Module, type Statement, type Type,
}
import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set
import simplifile
import type_compiler
import wat

// Conventions:
//  - module-local types and functions are called $<name>
//  - gleam built-in types and functions are called $gleam/<name>
//  - imported types and functions are called $<module>/<name>
//  - any function assigned to a parameter or local variable is wrapped in a
//    closure - in this case the wasm types for the function/closure are defined
//    when the variable is
//  - for functions passed as arguments, the types corresponding to the function
//    signature are generated at the call site
//  - function/closure types are named based on their signature so duplicates
//    are prevented.

fn compile_functions(
  functions: List(Definition(Function)),
  state: GlobalState,
) -> Result(GlobalState, CompilerError) {
  // TODO: compilation of functions is currently order-dependent
  list.try_fold(list.reverse(functions), state, fn(state, func_src) {
    compile_function(func_src, state)
  })
}

// Compile a function; returns the function and any anonymous functions defined within
fn compile_function(
  def: Definition(Function),
  state: GlobalState,
) -> Result(GlobalState, CompilerError) {
  // TODO: also return any referenced types and functions
  case def {
    glance.Definition(_, glance.Function(name, public, params, return, body, _)) -> {
      let param_locals =
        list.map(params, fn(param) {
          case param {
            glance.FunctionParameter(_, name, opt_type) -> {
              let name = compile_assignment_name(name)
              case opt_type {
                Some(type_) -> #(name, type_)
                None -> todo
              }
            }
          }
        })
      let return_type = case return {
        Some(type_) -> type_
        None -> todo
      }
      let func_type =
        glance.FunctionType(list.map(param_locals, fn(p) { p.1 }), return_type)
      let params_wasm =
        list.map(param_locals, fn(param) {
          let #(name, type_) = param
          let typeref = compile_typeref(type_, state)
          wat.WatVariableDefinition(name, typeref)
        })
      let functions = dict.insert(state.defined_functions, name, func_type)
      use #(state, body) <- result.try(expression_compiler.compile_body(
        body,
        compiler.with_params(
          compiler.new_local_state(
            compiler.GlobalState(..state, defined_functions: functions),
            name,
          ),
          param_locals,
        ),
      ))
      let function =
        wat.FunctionDefinition(
          name,
          params_wasm,
          compile_typeref(return_type, state.global),
          dict.to_list(state.locals)
            |> list.map(fn(local) {
              let #(name, type_) = local
              wat.WatVariableDefinition(
                name,
                compile_typeref(type_, state.global),
              )
            }),
          body,
        )
      Ok(
        compiler.GlobalState(
          ..state.global,
          compiled_functions: [function, ..state.global.compiled_functions],
          exported_functions: case public {
            glance.Public -> set.insert(state.global.exported_functions, name)
            glance.Private -> state.global.exported_functions
          },
        ),
      )
    }
  }
}

fn compile_assignment_name(name: AssignmentName) -> String {
  case name {
    glance.Named(str) -> str
    glance.Discarded(str) -> str
  }
}

fn compile_typeref(type_: Type, state: GlobalState) -> wat.WatType {
  type_compiler.compile_type(state, type_)
  |> expression_compiler.type_compiler_hack
}

pub fn main() {
  let builtin_types =
    dict.from_list([
      #(
        "gleam/Int",
        wat.Struct([wat.WatVariableDefinition("value", wat.Int64)]),
      ),
      #(
        "gleam/Float",
        wat.Struct([wat.WatVariableDefinition("value", wat.Float64)]),
      ),
      #(
        "gleam/List",
        wat.Struct([
          wat.WatVariableDefinition("value", wat.NullRef(wat.Direct(wat.Any))),
          wat.WatVariableDefinition("tail", wat.NullRef(wat.Id("gleam/List"))),
        ]),
      ),
    ])
  simplifile.read("example.gleam")
  |> result.map_error(compiler.FileError)
  |> result.try(fn(str) {
    glance.module(str)
    |> result.map_error(compiler.ParseError)
  })
  |> result.try(fn(module) {
    case module {
      glance.Module(_, _, _, _, functions) -> {
        compile_functions(
          functions,
          compiler.GlobalState(
            ..compiler.new_global_state(),
            defined_types: builtin_types,
          ),
        )
        |> result.map(fn(state) {
          let imports =
            state.imported_functions
            |> dict.values
            |> list.map(fn(function_import) {
              wat.FunctionImport(
                function_import.module,
                function_import.name,
                function_import.module <> "/" <> function_import.name,
                function_import.function_type,
              )
            })
          // TODO: type definitions must be output in a certain order, so storing in a dict doesn't make sense
          // For now put in a list.reverse() as this seems to put the definitions in the right order for the example
          let types =
            state.defined_types
            |> dict.to_list
            |> list.reverse
            |> list.map(fn(typedef) {
              let #(id, type_) = typedef
              wat.Type(id, type_)
            })
          let exports =
            set.to_list(state.exported_functions)
            |> list.map(fn(fn_name) { wat.FunctionExport(fn_name, fn_name) })

          list.flatten([imports, types, state.compiled_functions, exports])
        })
      }
    }
  })
  |> result.map(wat.to_string)
  |> result.try(fn(code_str) {
    simplifile.write("generated.wat", code_str)
    |> result.map_error(compiler.FileError)
  })
  |> io.debug
}
