import gleeunit/should
import glance
import compiler
import type_compiler
import gleam/dict
import wat
import gleam/option.{None, Some}

pub fn compile_basic_types_test() {
  let state =
    compiler.GlobalState(
      ..compiler.new_global_state(),
      modules: dict.from_list([#("dict", "gleam/dict")]),
    )

  type_compiler.compile_type(state, compiler.int_type)
  |> should.equal(Ok(#(state, wat.Int64)))

  type_compiler.compile_type(state, compiler.float_type)
  |> should.equal(Ok(#(state, wat.Float64)))

  type_compiler.compile_type(state, compiler.list_type(compiler.float_type))
  |> should.equal(Ok(#(state, wat.Ref(wat.Id("gleam/List")))))

  type_compiler.compile_type(
    state,
    glance.NamedType("Dict", Some("dict"), [
      compiler.int_type,
      compiler.int_type,
    ]),
  )
  |> should.equal(Ok(#(state, wat.Ref(wat.Id("gleam/dict/Dict")))))

  type_compiler.compile_type(
    state,
    glance.FunctionType(
      [compiler.int_type, compiler.list_type(compiler.int_type)],
      compiler.float_type,
    ),
  )
  |> should.equal(
    Ok(#(
      compiler.GlobalState(
        ..state,
        defined_types: dict.from_list([
          #(
            "function:Int.List<Int>:Float",
            wat.Function(wat.WatFunctionType(
              [wat.Int64, wat.Ref(wat.Id("gleam/List"))],
              wat.Float64,
            )),
          ),
          compiler.closure_type_def("Int.List<Int>:Float"),
        ]),
      ),
      wat.Ref(wat.Id("closure:Int.List<Int>:Float")),
    )),
  )
}
