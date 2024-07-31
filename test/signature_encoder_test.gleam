import gleeunit/should
import compiler
import signature_encoder.{encode_signature}
import gleam/dict
import gleam/option.{None, Some}
import glance

pub fn encode_signature_concrete_types_test() {
  encode_signature([compiler.int_type], compiler.int_type, dict.new())
  |> should.equal(Ok("Int:Int"))

  encode_signature(
    [compiler.float_type, compiler.float_type],
    compiler.float_type,
    dict.new(),
  )
  |> should.equal(Ok("Float.Float:Float"))

  encode_signature(
    [glance.NamedType("List", None, [compiler.int_type])],
    compiler.int_type,
    dict.new(),
  )
  |> should.equal(Ok("List<Int>:Int"))

  encode_signature(
    [
      glance.NamedType("Dict", Some("dict"), [
        glance.NamedType("String", None, []),
        compiler.int_type,
      ]),
    ],
    compiler.int_type,
    dict.from_list([#("dict", "gleam/dict")]),
  )
  |> should.equal(Ok("gleam/dict/Dict<String.Int>:Int"))

  encode_signature(
    [glance.TupleType([compiler.int_type, compiler.float_type])],
    glance.TupleType([compiler.int_type, compiler.int_type]),
    dict.new(),
  )
  |> should.equal(Ok("#<Int.Float>:#<Int.Int>"))
}

pub fn encode_signature_generic_types_test() {
  let tv_a = glance.VariableType("a")
  let tv_b = glance.VariableType("b")
  let tv_c = glance.VariableType("c")
  encode_signature([tv_a], tv_b, dict.new())
  |> should.equal(Ok("0:1"))

  encode_signature([tv_b, tv_a], tv_a, dict.new())
  |> should.equal(Ok("0.1:1"))

  encode_signature([glance.NamedType("List", None, [tv_a])], tv_a, dict.new())
  |> should.equal(Ok("List<0>:0"))

  encode_signature(
    [glance.NamedType("Dict", Some("dict"), [tv_a, tv_b])],
    tv_c,
    dict.from_list([#("dict", "gleam/dict")]),
  )
  |> should.equal(Ok("gleam/dict/Dict<0.1>:2"))

  encode_signature(
    [glance.TupleType([tv_c, tv_b])],
    glance.TupleType([tv_a, tv_c]),
    dict.new(),
  )
  |> should.equal(Ok("#<0.1>:#<2.0>"))
}

pub fn encode_signature_function_types_test() {
  let tv_a = glance.VariableType("a")
  let tv_b = glance.VariableType("b")
  let fn_x =
    glance.FunctionType(
      [compiler.int_type, compiler.int_type],
      compiler.int_type,
    )
  let fn_y = glance.FunctionType([tv_b, tv_a], tv_b)

  encode_signature(
    [glance.NamedType("List", None, [compiler.int_type]), fn_x],
    compiler.int_type,
    dict.new(),
  )
  |> should.equal(Ok("List<Int>.fn<Int.Int:Int>:Int"))

  encode_signature(
    [glance.NamedType("List", None, [tv_a]), tv_b, fn_y],
    tv_b,
    dict.new(),
  )
  |> should.equal(Ok("List<0>.1.fn<1.0:1>:1"))
}
