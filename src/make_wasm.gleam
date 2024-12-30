import gleam/io
import gleam/list
import gleam/result
import wasm

pub fn main() {
  impl() |> io.debug
}

fn impl() {
  // TODO: implement add/fold/sum
  let mb = wasm.create_module_builder("out.wasm")
  use mb <- result.try(
    wasm.add_type_group(mb, [
      wasm.Struct([
        wasm.ValueType(wasm.Immutable, wasm.I64),
        wasm.ValueType(wasm.Immutable, wasm.I64),
      ]),
    ]),
  )
  use mb <- result.try(
    wasm.add_type_group(mb, [
      wasm.Func([wasm.Ref(wasm.NonNull(wasm.ConcreteType(0)))], [wasm.I64]),
    ]),
  )
  use #(mb, fb) <- result.try(wasm.create_function_builder(mb, 1))
  use fb <- result.try(list.try_fold(
    over: [wasm.LocalGet(0), wasm.StructGet(0, 0), wasm.End],
    from: fb,
    with: wasm.add_instruction,
  ))
  use mb <- result.try(wasm.finalize_function(mb, fb))
  wasm.emit_module(mb) |> result.replace_error("Error writing to file")
}
