import wat
import gleeunit/should

pub fn wat_to_string_test() {
  wat.to_string([])
  |> should.equal("(module)")

  wat.to_string([
    wat.FunctionImport(
      "gleam/int",
      "add",
      "gleam/int/add",
      wat.WatFunctionType([wat.Int64, wat.Int64], wat.Int64),
    ),
  ])
  |> should.equal(
    "(module
  (import \"gleam/int\" \"add\" (func
    $gleam/int/add
    (param i64)
    (param i64)
    (result i64)
  ))
)",
  )

  wat.to_string([
    wat.Type(
      "some_function",
      wat.Function(wat.WatFunctionType([wat.Int64], wat.Float64)),
    ),
  ])
  |> should.equal(
    "(module
  (type $some_function (func
    (param i64)
    (result f64)
  ))
)",
  )

  wat.to_string([
    wat.FunctionDefinition(
      "some_function",
      [wat.WatVariableDefinition("a", wat.Int64)],
      wat.Int64,
      [wat.WatVariableDefinition("b", wat.Int64)],
      [
        wat.LocalSet("b", wat.LocalGet("a")),
        wat.Return(wat.Int64Add(wat.LocalGet("a"), wat.LocalGet("b"))),
      ],
    ),
  ])
  |> should.equal(
    "(module
  (func
    $some_function
    (param $a i64)
    (result i64)
    (local $b i64)
    (local.set $b (local.get $a))
    (return (i64.add (local.get $a) (local.get $b)))
  )
)",
  )

  wat.to_string([
    wat.FunctionDefinition(
      "some_function",
      [wat.WatVariableDefinition("a", wat.Int64)],
      wat.Int64,
      [],
      [wat.Call("smth", [wat.LocalGet("a")])],
    ),
  ])
  |> should.equal(
    "(module
  (func
    $some_function
    (param $a i64)
    (result i64)
    (call
      $smth
      (local.get $a)
    )
  )
)",
  )
  wat.to_string([
    wat.FunctionDefinition(
      "some_function",
      [wat.WatVariableDefinition("a", wat.Int64)],
      wat.Int64,
      [],
      [wat.Return(wat.Call("smth", [wat.LocalGet("a")]))],
    ),
  ])
  |> should.equal(
    "(module
  (func
    $some_function
    (param $a i64)
    (result i64)
    (return_call
      $smth
      (local.get $a)
    )
  )
)",
  )
}
