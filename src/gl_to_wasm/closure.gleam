//// Similar in purpose to gig/closure, but based off gig/core instead of gig/mono.
//// 

import gig/core
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

pub type Closure {
  Closure(
    typ: core.Poly,
    id: String,
    env: List(core.Parameter),
    parameters: List(core.Parameter),
    body: Expr,
  )
}

// TODO: I wonder if the function/closure distinction should be maintained,
// or instead every global function should (also) be a trivial closure, taking
// Nil as its environment
pub type Expr {
  Literal(typ: core.Type, value: core.LiteralKind)
  Local(typ: core.Type, name: String)
  Global(typ: core.Type, id: String)
  BindClosure(typ: core.Type, id: String, environment: List(Expr))
  CallGlobal(typ: core.Type, id: String, arguments: List(Expr))
  CallClosure(typ: core.Type, closure: Expr, arguments: List(Expr))
  Op(typ: core.Type, op: core.Op, arguments: List(Expr))
  Let(typ: core.Type, name: String, value: Expr, body: Expr)
  If(typ: core.Type, condition: Expr, then: Expr, els: Expr)
  Panic(typ: core.Type, value: Expr)
}

pub fn lower_module(module: core.Context) -> Module {
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
        core.FunctionType(..) -> {
          #(module, BindClosure(typ:, id:, environment: []))
        }
        _ -> #(module, Global(typ:, id:))
      }
    }
    core.Fn(typ:, parameters:, body:) -> lower_fn(typ, parameters, body)
    core.Call(typ:, function: core.Global(id:, ..), arguments:) -> {
      let #(module, arguments) =
        list.fold_right(arguments, #(module, []), fn(acc, arg) {
          let #(module, args) = acc
          let #(module, arg) = lower_expr(module, arg)
          #(module, [arg, ..args])
        })
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

fn lower_exprs(module: Module, exprs: List(core.Exp)) -> #(Module, List(Expr)) {
  list.fold_right(exprs, #(module, []), fn(acc, expr) {
    let #(module, exprs) = acc
    let #(module, expr) = lower_expr(module, expr)
    #(module, [expr, ..exprs])
  })
}

fn lower_fn(
  typ: core.Type,
  parameters: List(core.Parameter),
  body: core.Exp,
) -> #(Module, Expr) {
  let env =
    find_captures(list.map(parameters, fn(parameter) { parameter.name }), body)

  // TODO: mint and bind a closure.

  todo
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
