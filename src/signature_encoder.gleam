import glance
import gleam/int
import gleam/dict.{type Dict}
import gleam/list
import gleam/string
import gleam/option.{None, Some}
import gleam/result
import compiler

type EncoderState {
  EncoderState(
    imports: Dict(String, String),
    assignments: Dict(String, Int),
    assigned_count: Int,
  )
}

fn encode_types(
  types: List(glance.Type),
  state: EncoderState,
) -> Result(#(EncoderState, String), compiler.CompilerError) {
  list.try_fold(types, #(state, []), fn(acc, type_) {
    let #(state, encoded_types) = acc
    encode_type(type_, state)
    |> result.map(fn(res) {
      let #(state, encoded_type) = res
      #(state, [encoded_type, ..encoded_types])
    })
  })
  |> result.map(fn(res) {
    let #(state, encoded_types) = res
    #(state, string.join(list.reverse(encoded_types), "."))
  })
}

fn encode_type(
  type_: glance.Type,
  state: EncoderState,
) -> Result(#(EncoderState, String), compiler.CompilerError) {
  case type_ {
    glance.NamedType(name, module, params) ->
      {
        case module {
          None -> Ok("")
          Some(mod) ->
            dict.get(state.imports, mod)
            |> result.map_error(fn(_) { compiler.ReferenceError(mod) })
            |> result.map(fn(mod_str) { mod_str <> "/" })
        }
      }
      |> result.try(fn(mod) {
        case params {
          [] -> Ok(#(state, mod <> name))
          _ ->
            encode_types(params, state)
            |> result.map(fn(res) {
              let #(state, param_str) = res
              #(state, mod <> name <> "<" <> param_str <> ">")
            })
        }
      })
    glance.TupleType(elements) ->
      encode_types(elements, state)
      |> result.map(fn(res) {
        let #(state, elems_str) = res
        #(state, "#<" <> elems_str <> ">")
      })
    glance.FunctionType(params, return) ->
      encode_signature_internal(params, return, state)
      |> result.map(fn(res) {
        let #(state, sig) = res
        #(state, "fn<" <> sig <> ">")
      })
    glance.VariableType(name) ->
      case dict.get(state.assignments, name) {
        Ok(num) -> Ok(#(state, int.to_string(num)))
        _ ->
          Ok(#(
            EncoderState(
              ..state,
              assignments: dict.insert(
                state.assignments,
                name,
                state.assigned_count,
              ),
              assigned_count: state.assigned_count
              + 1,
            ),
            int.to_string(state.assigned_count),
          ))
      }
    glance.HoleType(_name) -> todo
  }
}

fn encode_signature_internal(
  parameters: List(glance.Type),
  return: glance.Type,
  state: EncoderState,
) -> Result(#(EncoderState, String), compiler.CompilerError) {
  encode_types(parameters, state)
  |> result.try(fn(res) {
    let #(state, param_str) = res
    encode_type(return, state)
    |> result.map(fn(res) {
      let #(state, return_str) = res
      #(state, param_str <> ":" <> return_str)
    })
  })
}

pub fn encode_signature(
  parameters: List(glance.Type),
  return: glance.Type,
  imports: Dict(String, String),
) -> Result(String, compiler.CompilerError) {
  encode_signature_internal(
    parameters,
    return,
    EncoderState(imports, dict.new(), 0),
  )
  |> result.map(fn(res) {
    let #(_state, str) = res
    str
  })
}
