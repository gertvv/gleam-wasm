import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string_builder.{type StringBuilder, from_string as sb}
import gleam/io
import gleam/int

pub type WatModule =
  List(WatDefinition)

pub type WatDefinition {
  FunctionImport(
    module: String,
    function: String,
    id: String,
    definition: WatFunctionType,
  )
  Type(id: String, definition: WatType)
  FunctionDefinition(
    id: String,
    parameters: List(WatVariableDefinition),
    result: WatType,
    locals: List(WatVariableDefinition),
    body: List(WatExpression),
  )
  FunctionExport(name: String, id: String)
}

pub type WatVariableDefinition {
  WatVariableDefinition(id: String, type_: WatType)
}

pub type WatType {
  Int32
  Int64
  Float64
  Any
  Function(definition: WatFunctionType)
  Ref(to: WatTypeRef)
  NullRef(to: WatTypeRef)
  Struct(fields: List(WatVariableDefinition))
}

pub type WatTypeRef {
  Id(id: String)
  Direct(type_: WatType)
}

pub type WatFunctionType {
  WatFunctionType(parameters: List(WatType), result: WatType)
}

pub type WatExpression {
  If(
    condition: WatExpression,
    then: WatExpression,
    else_: Option(WatExpression),
  )
  Return(expr: WatExpression)
  Int64Const(value: Int)
  Float64Const(value: Float)
  Int64Add(left: WatExpression, right: WatExpression)
  Int64Equal(left: WatExpression, right: WatExpression)
  Int32Const(value: Int)
  Int32EqualZero(expr: WatExpression)
  Call(id: String, parameters: List(WatExpression))
  CallRef(type_id: String, parameters: List(WatExpression), ref: WatExpression)
  RefCast(type_id: String, ref: WatExpression)
  LocalGet(id: String)
  LocalSet(id: String, value: WatExpression)
  RefFunc(id: String)
  StructNew(type_id: String, fields: List(WatExpression))
  StructGet(type_id: String, field_id: String, ref: WatExpression)
  RefNull(WatTypeRef)
  RefAsNonNull(WatExpression)
  Block(exprs: List(WatExpression))
  Unreachable
}

fn indent_more(indent: StringBuilder) -> StringBuilder {
  string_builder.append(indent, "  ")
}

fn sexpr(
  name: String,
  args: List(StringBuilder),
  indent: StringBuilder,
  newline_separate: Bool,
) -> StringBuilder {
  case newline_separate, args {
    _, [] -> {
      [sb("("), sb(name), sb(")")]
      |> string_builder.concat
    }
    True, _ -> {
      let arg_list =
        list.map(args, string_builder.prepend_builder(_, indent_more(indent)))
      [sb("("), sb(name), ..arg_list]
      |> string_builder.concat
      |> string_builder.append_builder(indent)
      |> string_builder.append(")")
    }
    False, _ ->
      [sb("("), string_builder.join([sb(name), ..args], " "), sb(")")]
      |> string_builder.concat
  }
}

fn type_to_string(type_: WatType, indent: StringBuilder) -> StringBuilder {
  case type_ {
    Int32 -> sb("i32")
    Int64 -> sb("i64")
    Float64 -> sb("f64")
    Ref(Id(id)) -> sexpr("ref", [id_to_string(id)], indent, False)
    Ref(Direct(type_)) ->
      sexpr("ref", [type_to_string(type_, indent)], indent, False)
    NullRef(Id(id)) ->
      sexpr("ref", [sb("null"), id_to_string(id)], indent, False)
    NullRef(Direct(type_)) ->
      sexpr("ref", [sb("null"), type_to_string(type_, indent)], indent, False)
    Function(type_) -> function_type_to_string(None, type_, indent)
    Struct(fields) ->
      sexpr(
        "struct",
        list.map(fields, fn(field) {
          sexpr(
            "field",
            [
              id_to_string(field.id),
              type_to_string(field.type_, indent_more(indent)),
            ],
            indent_more(indent),
            False,
          )
        }),
        indent,
        True,
      )
    Any -> sb("any")
    _ -> {
      io.debug(type_)
      todo
    }
  }
}

fn string_literal(value: String) -> StringBuilder {
  sb("\"" <> value <> "\"")
}

fn id_to_string(id: String) -> StringBuilder {
  sb("$" <> id)
}

fn function_type_to_string(
  id: Option(String),
  definition: WatFunctionType,
  indent: StringBuilder,
) -> StringBuilder {
  let params =
    list.map(definition.parameters, fn(param_type) {
      sexpr("param", [type_to_string(param_type, indent)], indent, False)
    })
  let result =
    sexpr("result", [type_to_string(definition.result, indent)], indent, False)
  case id {
    Some(id_str) ->
      sexpr(
        "func",
        list.append([id_to_string(id_str), ..params], [result]),
        indent,
        True,
      )
    None -> sexpr("func", list.append(params, [result]), indent, True)
  }
}

fn sexpr_taking_id_and_args(
  name: String,
  id: String,
  args: List(WatExpression),
  indent: StringBuilder,
) {
  sexpr(
    name,
    [
      id_to_string(id),
      ..list.map(args, expression_to_string(_, indent_more(indent)))
    ],
    indent,
    True,
  )
}

fn expression_to_string(
  expr: WatExpression,
  indent: StringBuilder,
) -> StringBuilder {
  case expr {
    LocalGet(id) -> sexpr("local.get", [id_to_string(id)], indent, False)
    LocalSet(id, value) ->
      sexpr(
        "local.set",
        [id_to_string(id), expression_to_string(value, indent)],
        indent,
        False,
      )
    Int32EqualZero(expr) ->
      sexpr("i32.eqz", [expression_to_string(expr, indent)], indent, False)
    Int64Const(value) ->
      sexpr("i64.const", [sb(int.to_string(value))], indent, False)
    Int64Add(left, right) ->
      sexpr(
        "i64.add",
        [
          expression_to_string(left, indent),
          expression_to_string(right, indent),
        ],
        indent,
        False,
      )
    Call(id, args) -> sexpr_taking_id_and_args("call", id, args, indent)
    Return(Call(id, args)) ->
      sexpr_taking_id_and_args("return_call", id, args, indent)
    CallRef(type_id, args, ref) ->
      sexpr_taking_id_and_args(
        "call_ref",
        type_id,
        list.append(args, [ref]),
        indent,
      )
    Return(CallRef(type_id, args, ref)) ->
      sexpr_taking_id_and_args(
        "return_call_ref",
        type_id,
        list.append(args, [ref]),
        indent,
      )
    StructNew(type_id, fields) ->
      sexpr_taking_id_and_args("struct.new", type_id, fields, indent)
    StructGet(type_id, field_id, expr) ->
      sexpr(
        "struct.get",
        [
          id_to_string(type_id),
          id_to_string(field_id),
          expression_to_string(expr, indent),
        ],
        indent,
        False,
      )
    RefFunc(id) -> sexpr("ref.func", [id_to_string(id)], indent, False)
    RefNull(Direct(type_)) ->
      sexpr("ref.null", [type_to_string(type_, indent)], indent, False)
    Block(exprs) ->
      sexpr(
        "block",
        list.map(exprs, expression_to_string(_, indent_more(indent))),
        indent,
        True,
      )
    If(cond, then, else_) ->
      sexpr(
        "if",
        [
          expression_to_string(cond, indent_more(indent)),
          sexpr(
            "then",
            [expression_to_string(then, indent_more(indent))],
            indent,
            False,
          ),
          ..case else_ {
            Some(else_expr) -> [
              sexpr(
                "else",
                [expression_to_string(else_expr, indent_more(indent))],
                indent,
                False,
              ),
            ]
            _ -> []
          }
        ],
        indent,
        True,
      )
    Return(expr) ->
      sexpr("return", [expression_to_string(expr, indent)], indent, False)
    Unreachable -> sb("unreachable")
    _ -> {
      io.debug(expr)
      todo
    }
  }
}

fn definition_to_string(definition: WatDefinition) -> StringBuilder {
  let indent = indent_more(sb("\n"))
  case definition {
    FunctionImport(module, name, id, type_) ->
      sexpr(
        "import",
        [
          string_literal(module),
          string_literal(name),
          function_type_to_string(Some(id), type_, indent),
        ],
        indent,
        False,
      )
    Type(id, type_) ->
      sexpr(
        "type",
        [id_to_string(id), type_to_string(type_, indent)],
        indent,
        False,
      )
    FunctionDefinition(id, params, result, locals, body) -> {
      let id = [id_to_string(id)]
      let params =
        list.map(params, fn(param) {
          sexpr(
            "param",
            [
              id_to_string(param.id),
              type_to_string(param.type_, indent_more(indent)),
            ],
            indent,
            False,
          )
        })
      let result = [
        sexpr(
          "result",
          [type_to_string(result, indent_more(indent))],
          indent,
          False,
        ),
      ]
      let locals =
        list.map(locals, fn(local) {
          sexpr(
            "local",
            [
              id_to_string(local.id),
              type_to_string(local.type_, indent_more(indent)),
            ],
            indent,
            False,
          )
        })
      let body =
        list.map(body, fn(expr) {
          expression_to_string(expr, indent_more(indent))
        })
      sexpr(
        "func",
        list.flatten([id, params, result, locals, body]),
        indent,
        True,
      )
    }
    FunctionExport(name, id) -> {
      sexpr(
        "export",
        [string_literal(name), sexpr("func", [id_to_string(id)], indent, False)],
        indent,
        False,
      )
    }
  }
}

pub fn to_string(module: WatModule) -> String {
  sexpr("module", list.map(module, definition_to_string), sb("\n"), True)
  |> string_builder.to_string()
}
