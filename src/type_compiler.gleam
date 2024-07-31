import glance.{type Type}
import compiler.{type GlobalState}
import wat
import gleam/io
import gleam/option.{None, Some}
import gleam/dict
import gleam/result
import gleam/list
import signature_encoder

pub fn compile_type(
  state: GlobalState,
  type_: Type,
) -> Result(#(GlobalState, wat.WatType), compiler.CompilerError) {
  case type_ {
    type_ if type_ == compiler.int_type -> Ok(#(state, wat.Int64))
    type_ if type_ == compiler.float_type -> Ok(#(state, wat.Float64))
    glance.NamedType("List", None, _) ->
      Ok(#(state, wat.Ref(wat.Id("gleam/List"))))
    glance.NamedType(name, None, _) -> Ok(#(state, wat.Ref(wat.Id(name))))
    glance.NamedType(name, Some(module_name), _) ->
      dict.get(state.modules, module_name)
      |> result.map(fn(module) {
        #(state, wat.Ref(wat.Id(module <> "/" <> name)))
      })
      |> result.map_error(fn(_) { compiler.ReferenceError(module_name) })
    glance.FunctionType(params, return) -> {
      use signature <- result.try(signature_encoder.encode_signature(
        params,
        return,
        state.modules,
      ))
      use #(state, params_compiled) <- result.try(
        list.try_fold(params, #(state, []), fn(acc, param) {
          let #(state, list) = acc
          compile_type(state, param)
          |> result.map(fn(res) { #(res.0, [res.1, ..list]) })
        }),
      )
      let params_compiled = list.reverse(params_compiled)
      use #(state, return_compiled) <- result.map(compile_type(state, return))
      #(
        compiler.GlobalState(
          ..state,
          defined_types: dict.merge(
            state.defined_types,
            dict.from_list([
              #(
                "function:" <> signature,
                wat.Function(wat.WatFunctionType(
                  params_compiled,
                  return_compiled,
                )),
              ),
              compiler.closure_type_def(signature),
            ]),
          ),
        ),
        wat.Ref(wat.Id("closure:" <> signature)),
      )
    }
    glance.VariableType(_) -> Ok(#(state, wat.Ref(wat.Direct(wat.Any))))
    _ -> {
      io.debug(type_)
      todo
    }
  }
}
