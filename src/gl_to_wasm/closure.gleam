//// Similar in purpose to gig/closure, but based off gig/core instead of gig/mono.
//// 

import gig/core
import gig/gen_names
import gleam/int
import gleam/list

pub type Module {
  Module(
    types: List(core.CustomType),
    functions: List(Function),
    externals: List(core.External),
    closures: List(Closure),
  )
}

pub type Function {
  Function(
    typ: core.Poly,
    id: String,
    parameters: List(core.Parameter),
    body: Expr,
  )
}

/// A closure takes additional parameters via its environment.
/// In this representation, we pretend these are just automatically available,
/// no upacking is done.
pub type Closure {
  // TODO: add c_typ, e_typ, f_typ
  Closure(
    /// The closure's "virtual type" - i.e. the type of function it appears as
    typ: core.Poly,
    /// The type being passed around representing the closure
    c_typ: core.Type,
    /// The type representing the closure's environment
    e_typ: core.Type,
    /// The function implementing the closure, taking the environment as its first argument
    f_typ: core.Type,
    id: String,
    environment: List(core.Parameter),
    parameters: List(core.Parameter),
    body: Expr,
  )
}

pub type Expr {
  Literal(typ: core.Type, value: core.LiteralKind)
  Local(typ: core.Type, name: String)
  Global(typ: core.Type, id: String)
  /// Bind takes a true closure and binds its environment variables.
  BindClosure(typ: core.Type, id: String, environment: List(Expr))
  /// Calls a bound closure.
  CallClosure(typ: core.Type, closure: Expr, arguments: List(Expr))
  /// Calls a global.
  CallGlobal(typ: core.Type, id: String, arguments: List(Expr))
  Op(typ: core.Type, op: core.Op, arguments: List(Expr))
  Let(typ: core.Type, name: String, value: Expr, body: Expr)
  If(typ: core.Type, condition: Expr, then: Expr, els: Expr)
  Panic(typ: core.Type, value: Expr)
}

pub fn lower_module(module: core.Module) -> Module {
  let acc =
    Module(
      types: module.types,
      externals: module.externals,
      functions: [],
      closures: [],
    )
  list.fold(module.functions, acc, lower_function)
}

fn lower_function(module: Module, function: core.Function) -> Module {
  let #(module, expr) = lower_expr(module, function.body)
  let function = Function(function.typ, function.id, function.parameters, expr)
  Module(..module, functions: [function, ..module.functions])
}

fn lower_expr(module: Module, body: core.Exp) -> #(Module, Expr) {
  case body {
    core.Literal(typ:, value:) -> #(module, Literal(typ:, value:))
    core.Local(typ:, name:) -> #(module, Local(typ:, name:))
    core.Global(typ:, id:) -> {
      case typ {
        core.FunctionType(parameters:, return:) -> {
          let #(module, id) =
            generate_closure(module, typ, id, parameters, return)
          #(module, BindClosure(typ:, id:, environment: []))
        }
        _ -> #(module, Global(typ:, id:))
      }
    }
    core.Fn(typ:, parameters:, body:) -> lower_fn(module, typ, parameters, body)
    core.Call(typ:, function: core.Global(id:, ..), arguments:) -> {
      let #(module, arguments) = lower_exprs(module, arguments)
      #(module, CallGlobal(typ:, id:, arguments:))
    }
    core.Call(typ:, function:, arguments:) -> {
      let #(module, function) = lower_expr(module, function)
      let #(module, arguments) = lower_exprs(module, arguments)
      #(module, CallClosure(typ:, closure: function, arguments:))
    }
    core.Op(typ:, op:, arguments:) -> {
      let #(module, arguments) = lower_exprs(module, arguments)
      #(module, Op(typ:, op:, arguments:))
    }
    core.Let(typ:, name:, value:, body:) -> {
      let #(module, value) = lower_expr(module, value)
      let #(module, body) = lower_expr(module, body)
      #(module, Let(typ:, name:, value:, body:))
    }
    core.If(typ:, condition:, then:, els:) -> {
      let #(module, condition) = lower_expr(module, condition)
      let #(module, then) = lower_expr(module, then)
      let #(module, els) = lower_expr(module, els)
      #(module, If(typ:, condition:, then:, els:))
    }
    core.Panic(typ:, value:) -> {
      let #(module, value) = lower_expr(module, value)
      #(module, Panic(typ:, value:))
    }
  }
}

fn generate_closure(
  module: Module,
  typ: core.Type,
  id: String,
  params: List(core.Type),
  return: core.Type,
) -> #(Module, String) {
  let c_id = "$C_" <> id
  let e_typ = core.NamedType("Nil", [])
  let f_typ = core.FunctionType([e_typ, ..params], return)
  let c_typ = core.NamedType(gen_names.get_tuple_id(2), [e_typ, f_typ])
  let params =
    list.index_map(params, fn(typ, idx) {
      core.Parameter(typ:, name: "p" <> int.to_string(idx + 1))
    })
  let c =
    Closure(
      typ: core.Poly(find_type_vars(typ), typ),
      c_typ:,
      e_typ:,
      f_typ:,
      id: c_id,
      environment: [],
      parameters: [
        core.Parameter(typ: core.NamedType("Nil", []), name: "env"),
        ..params
      ],
      body: CallGlobal(
        return,
        id,
        list.map(params, fn(param) { Local(param.typ, param.name) }),
      ),
    )
  #(Module(..module, closures: [c, ..module.closures]), c_id)
}

fn lower_exprs(module: Module, exprs: List(core.Exp)) -> #(Module, List(Expr)) {
  list.fold_right(exprs, #(module, []), fn(acc, expr) {
    let #(module, exprs) = acc
    let #(module, expr) = lower_expr(module, expr)
    #(module, [expr, ..exprs])
  })
}

fn lower_fn(
  module: Module,
  typ: core.Type,
  parameters: List(core.Parameter),
  body: core.Exp,
) -> #(Module, Expr) {
  let environment =
    find_captures(list.map(parameters, fn(parameter) { parameter.name }), body)
  let e_typ =
    core.NamedType(
      gen_names.get_tuple_id(list.length(environment)),
      list.map(environment, fn(arg) { arg.typ }),
    )
  let f_typ =
    core.FunctionType(
      [e_typ, ..list.map(parameters, fn(param) { param.typ })],
      body.typ,
    )
  let c_typ = core.NamedType(gen_names.get_tuple_id(2), [e_typ, f_typ])

  let #(module, body) = lower_expr(module, body)

  // TODO: generate a real unique ID
  let id = "closure_" <> int.to_string(list.length(module.closures))

  let closure =
    Closure(
      typ: core.Poly(find_type_vars(typ), typ),
      c_typ:,
      e_typ:,
      f_typ:,
      id:,
      environment:,
      parameters:,
      body:,
    )

  let module = Module(..module, closures: [closure, ..module.closures])

  let expr =
    BindClosure(
      typ:,
      id:,
      environment: list.map(environment, fn(param) {
        Local(param.typ, param.name)
      }),
    )

  #(module, expr)
}

fn find_type_vars(typ: core.Type) -> List(core.TypeVarId) {
  case typ {
    core.NamedType(parameters:, ..) ->
      list.fold(parameters, [], fn(acc, param) {
        find_type_vars(param) |> list.append(acc) |> list.unique
      })
    core.FunctionType(parameters:, return:) ->
      list.fold([return, ..parameters], [], fn(acc, param) {
        find_type_vars(param) |> list.append(acc) |> list.unique
      })
    core.Unbound(id:) -> [id]
  }
}

fn find_captures(locals: List(String), expr: core.Exp) -> List(core.Parameter) {
  case expr {
    core.Literal(..) -> []
    core.Local(typ:, name:) ->
      case list.contains(locals, name) {
        True -> []
        False -> [core.Parameter(typ:, name:)]
      }

    core.Global(..) -> []
    core.Fn(parameters:, body:, ..) -> {
      list.map(parameters, fn(parameter) { parameter.name })
      |> list.append(locals)
      |> find_captures(body)
    }
    core.Call(function:, arguments:, ..) ->
      find_captures_list(locals, [function, ..arguments])
    core.Op(arguments:, ..) -> find_captures_list(locals, arguments)
    core.Let(name:, value:, body:, ..) -> {
      find_captures(locals, value)
      |> list.append(find_captures([name, ..locals], body))
    }
    core.If(condition:, then:, els:, ..) -> {
      find_captures(locals, condition)
      |> list.append(find_captures(locals, then))
      |> list.append(find_captures(locals, els))
    }
    core.Panic(value:, ..) -> find_captures(locals, value)
  }
}

fn find_captures_list(
  locals: List(String),
  exprs: List(core.Exp),
) -> List(core.Parameter) {
  list.fold(over: exprs, from: [], with: fn(acc, arg) {
    find_captures(locals, arg)
    |> list.append(acc)
  })
}
