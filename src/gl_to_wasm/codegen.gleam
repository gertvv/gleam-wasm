import gig/core
import gig/typed_ast.{type ModuleInterface}
import gl_to_wasm/closure
import gl_to_wasm/graph
import gl_to_wasm/prelude
import gl_wasm/wasm
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set
import gleam/string
import pprint

type Context {
  Context(
    mb: wasm.ModuleBuilder,
    fbs: List(wasm.CodeBuilder),
    types: Dict(TypeRef, Int),
    next_type_index: Int,
    interfaces: Dict(String, ModuleInterface),
    functions: Dict(String, Int),
  )
}

pub type Error {
  WasmError(String)
}

// For example how do we represent this type?
// 
// pub type Example {
//  Example(nested: Example, func: fn(Example) -> String)
//  TheEnd
// }
//
// We need a type_group because example has sub-types that are self-referential,
// and the function type references the struct type that references the function type.
// Let i be the next available type index, and h the type index of String.
//
// [
//   wasm.SubOpen([], wasm.Struct(Some("Example"), [])),
//   wasm.SubFinal([i], wasm.Struct(Some("Example"), [
//     wasm.ValueType(
//       Some("nested"), wasm.Immutable,
//       wasm.Ref(wasm.NonNull(wasm.ConcreteType(i))),
//     wasm.ValueType(
//       Some("func"), wasm.Immutable,
//       wasm.Ref(wasm.NonNull(wasm.ConcreteType(i + 3)))),
//   ])),
//   wasm.SubFinal([i], wasm.Struct(Some("TheEnd"), []))
//   wasm.SubFinal([], wasm.Func(None, [
//     wasm.Ref(wasm.NonNull(wasm.ConcreteType(i)))
//   ], [
//     wasm.Ref(wasm.NonNull(wasm.ConcreteType(h)))
//   ]))
// ]

pub fn codegen_module(
  interfaces: Dict(String, ModuleInterface),
  module_name: String,
  module: closure.Module,
) -> Result(wasm.ModuleBuilder, Error) {
  let mb = wasm.create_module_builder(Some(module_name))
  let ctx =
    Context(
      mb:,
      fbs: [],
      types: dict.new(),
      next_type_index: 0,
      interfaces:,
      functions: dict.new(),
    )
  use ctx <- result.try(register_types(ctx, module))
  use ctx <- result.try(register_functions(ctx, module))
  use ctx <- result.try(build_functions(ctx, module))
  echo ctx.mb
  todo
}

fn build_functions(
  ctx: Context,
  module: closure.Module,
) -> Result(Context, Error) {
  list.zip(module.functions, ctx.fbs)
  |> list.try_fold(ctx, fn(ctx, build) {
    let #(func, fb) = build
    let local_env =
      dict.from_list(
        list.index_map(func.parameters, fn(param, idx) { #(param.name, idx) }),
      )
    use fb <- result.try(codegen_expr(fb, func.body, local_env, ctx))
    use mb <- result.map(wasm.finalize_function(ctx.mb, fb))
    Context(..ctx, mb:)
  })
  |> result.map_error(WasmError)
}

fn codegen_expr(
  fb: wasm.CodeBuilder,
  expr: closure.Expr,
  local_env: Dict(String, Int),
  ctx: Context,
) -> Result(wasm.CodeBuilder, String) {
  case expr {
    closure.Literal(typ:, value:) -> {
      case value {
        // TODO: instead of creating a new struct, get the global
        core.NilVal ->
          wasm.add_instruction(fb, wasm.StructNew(prelude.nil_index))
        core.Bool(value:) -> todo
        core.Int(value:) -> {
          use value <- result.try(
            int.parse(value) |> result.replace_error("Invalid Int value"),
          )
          use value <- result.try(wasm.int64_signed(value))
          use fb <- result.try(wasm.add_instruction(fb, wasm.I64Const(value)))
          wasm.add_instruction(fb, wasm.StructNew(prelude.int_index))
        }
        core.Float(value:) -> todo
        core.String(value:) -> todo
        core.BitArray(size:) -> todo
      }
    }
    closure.Local(name:, ..) -> {
      let assert Ok(idx) = dict.get(local_env, name)
      wasm.add_instruction(fb, wasm.LocalGet(idx))
    }
    closure.Global(typ:, id:) -> todo
    closure.Bind(typ:, id:, environment:) -> {
      // generate the environment
      let assert Ok(e_type_idx) = todo
      use fb <- result.try(
        list.try_fold(environment, fb, fn(fb, arg) {
          codegen_expr(fb, arg, local_env, ctx)
        }),
      )
      use fb <- result.try(wasm.add_instruction(fb, wasm.StructNew(e_type_idx)))
      // create the function reference
      let assert Ok(f_idx) = dict.get(ctx.functions, id)
      use fb <- result.try(wasm.add_instruction(fb, wasm.RefFunc(f_idx)))
      // create the struct
      let assert Ok(c_type_idx) = todo
      wasm.add_instruction(fb, wasm.StructNew(c_type_idx))
    }
    closure.Call(typ:, closure:, arguments:) -> {
      // TODO: the closure needs separate struct and func types!
      // create a local
      let assert Ok(c_type_idx) = dict.get(ctx.types, type_to_typeref(typ))
      use #(fb, local_idx) <- result.try(wasm.add_local(
        fb,
        wasm.Ref(wasm.NonNull(wasm.ConcreteType(c_type_idx))),
        None,
      ))
      // generate the closure struct and store it
      use fb <- result.try(codegen_expr(fb, closure, local_env, ctx))
      use fb <- result.try(wasm.add_instruction(fb, wasm.LocalSet(local_idx)))
      // put the closure environment on the stack
      use fb <- result.try(wasm.add_instruction(fb, wasm.LocalGet(local_idx)))
      use fb <- result.try(wasm.add_instruction(
        fb,
        wasm.StructGet(c_type_idx, 0),
      ))
      // put the function parameters on the stack
      use fb <- result.try(
        list.try_fold(arguments, fb, fn(fb, arg) {
          codegen_expr(fb, arg, local_env, ctx)
        }),
      )
      // put the function itself on the stack
      use fb <- result.try(wasm.add_instruction(fb, wasm.LocalGet(local_idx)))
      use fb <- result.try(wasm.add_instruction(
        fb,
        wasm.StructGet(c_type_idx, 1),
      ))
      // call the function
      let assert Ok(f_type_idx) = dict.get(ctx.types, todo)
      wasm.add_instruction(fb, wasm.CallRef(f_type_idx))
    }
    closure.Op(typ:, op:, arguments:) -> {
      case op {
        core.FieldAccess(variant:, field:) -> todo
        core.VariantCheck(variant:) -> todo
      }
    }
    closure.Let(typ:, name:, value:, body:) -> {
      // create a local
      let assert Ok(type_idx) = dict.get(ctx.types, type_to_typeref(typ))
      use #(fb, local_idx) <- result.try(wasm.add_local(
        fb,
        wasm.Ref(wasm.NonNull(wasm.ConcreteType(type_idx))),
        Some(name),
      ))
      // generate the assigned value
      use fb <- result.try(codegen_expr(fb, value, local_env, ctx))
      // set the local
      use fb <- result.try(wasm.add_instruction(fb, wasm.LocalSet(local_idx)))
      let local_env = dict.insert(local_env, name, local_idx)
      // generate the body
      codegen_expr(fb, body, local_env, ctx)
    }
    closure.If(typ:, condition:, then:, els:) -> {
      let assert Ok(type_idx) = dict.get(ctx.types, type_to_typeref(typ))
      let bt =
        wasm.BlockValue(wasm.Ref(wasm.NonNull(wasm.ConcreteType(type_idx))))
      use fb <- result.try(codegen_expr(fb, condition, local_env, ctx))
      use fb <- result.try(wasm.add_instruction(fb, wasm.If(bt)))
      use fb <- result.try(codegen_expr(fb, then, local_env, ctx))
      use fb <- result.try(wasm.add_instruction(fb, wasm.Else))
      use fb <- result.try(codegen_expr(fb, els, local_env, ctx))
      wasm.add_instruction(fb, wasm.End)
    }
    closure.Panic(typ:, value:) -> todo
  }
}

fn register_functions(
  ctx: Context,
  module: closure.Module,
) -> Result(Context, Error) {
  // TODO: externals
  use #(mb, fbs) <- result.map(
    list.map(module.functions, fn(func) {
      // TODO: add an environment param?
      let closure.Function(typ:, id:, parameters:, ..) = func
      let assert Ok(type_idx) = dict.get(ctx.types, type_to_typeref(typ.typ))
      wasm.FunctionSignature(
        type_idx,
        Some(id),
        Some(list.map(parameters, fn(param) { param.name })),
      )
    })
    |> wasm.create_function_builders(ctx.mb, _)
    |> result.map_error(WasmError),
  )
  Context(..ctx, mb:, fbs:)
}

// TODO: what kind of types do I need to generate at the WebAssembly level?
//  - A rec-group with a struct for each custom type and a struct for each variant
//  - A struct for each closure environment
//  - A function type for each closure, function, and external defined
//  - All of the above for both defined in this module and imported/used
// By convention:
//  - the index for variants is the index for the custom type plus the one-based index of the variant.
//  - the index of a closure's environment is the index of the function plus one
fn register_types(
  ctx: Context,
  module: closure.Module,
) -> Result(Context, Error) {
  let types = list_module_direct_type_references(module)
  let types =
    list_indirect_type_references(types, ctx.interfaces) |> set.to_list
  let graph = construct_type_graph(types, ctx.interfaces) |> pprint.debug
  let assert Ok(sorted) = graph.squash_cycles(graph) |> graph.topological_sort
  pprint.debug(sorted)
  use ctx <- result.try(list.try_fold(sorted, ctx, register_type_group))
  Ok(ctx)
}

fn construct_type_graph(
  vertices: List(TypeRef),
  interfaces: Dict(String, ModuleInterface),
) -> graph.Graph(TypeRef) {
  let edges =
    list.flat_map(vertices, fn(typ) {
      case typ {
        CustomTypeRef(name:) ->
          list_custom_type_type_references(name, interfaces)
        FunctionTypeRef(params:, return:) -> [return, ..params]
        HoleTypeRef -> []
      }
      |> list.map(fn(other) { #(typ, other) })
    })
  #(vertices, edges)
}

fn list_indirect_type_references(
  types: List(TypeRef),
  interfaces: Dict(String, ModuleInterface),
) -> set.Set(TypeRef) {
  do_list_indirect_type_references(types, set.new(), interfaces)
}

fn do_list_indirect_type_references(
  types: List(TypeRef),
  seen: set.Set(TypeRef),
  interfaces: Dict(String, ModuleInterface),
) -> set.Set(TypeRef) {
  case types {
    [typ, ..rest] -> {
      let new = case set.contains(seen, typ) {
        True -> []
        False ->
          case typ {
            CustomTypeRef(name:) ->
              list_custom_type_type_references(name, interfaces)
            FunctionTypeRef(params:, return:) -> [return, ..params]
            HoleTypeRef -> []
          }
      }
      do_list_indirect_type_references(
        list.append(new, rest),
        set.insert(seen, typ),
        interfaces,
      )
    }
    [] -> seen
  }
}

fn list_custom_type_type_references(
  name: String,
  interfaces: Dict(String, ModuleInterface),
) -> List(TypeRef) {
  let assert Ok(t) = resolve_custom_type(interfaces, name)
  list.flat_map(t.variants, fn(variant) {
    list.map(variant.fields, fn(field) { typed_ast_type_to_ref(field.item.typ) })
  })
}

fn typed_ast_type_to_ref(typ: typed_ast.Type) {
  case typ {
    typed_ast.NamedType(module:, name:, parameters:) ->
      case module {
        "gleam" -> CustomTypeRef(name)
        _ -> todo
      }
    typed_ast.TupleType(elements:) -> todo
    typed_ast.FunctionType(parameters:, return:) ->
      FunctionTypeRef(
        list.map(parameters, typed_ast_type_to_ref),
        typed_ast_type_to_ref(return),
      )
    typed_ast.VariableType(..) -> HoleTypeRef
  }
}

fn resolve_custom_type(interfaces: Dict(String, ModuleInterface), name: String) {
  // TODO: make this more complete
  let assert Ok(prelude) = dict.get(interfaces, core.builtin)
  list.find(prelude.custom_types, fn(t) { t.name == name })
}

fn list_module_direct_type_references(module: closure.Module) -> List(TypeRef) {
  let from_types =
    list.flat_map(module.types, fn(ct) {
      [
        CustomTypeRef(ct.id),
        ..list.flat_map(ct.variants, fn(variant) {
          list.flat_map(variant.fields, fn(field) {
            [type_to_typeref(field.typ)]
          })
        })
      ]
    })
  let from_externals =
    list.flat_map(module.externals, fn(ext) { [type_to_typeref(ext.typ.typ)] })
  let from_functions =
    list.flat_map(module.functions, fn(func) {
      [type_to_typeref(func.typ.typ)]
      |> list.append(list_expr_direct_type_references(func.body))
    })
  list.append(from_types, from_externals)
  |> list.append(from_functions)
}

fn list_expr_direct_type_references(expr: closure.Expr) -> List(TypeRef) {
  case expr {
    closure.Literal(typ:, ..)
    | closure.Local(typ:, ..)
    | closure.Global(typ:, ..)
    | closure.Panic(typ:, ..) -> [type_to_typeref(typ)]
    closure.Bind(typ:, environment:, ..) ->
      [type_to_typeref(typ)]
      |> list.append(list.flat_map(
        environment,
        list_expr_direct_type_references,
      ))
    closure.Call(typ:, closure:, arguments:) ->
      [type_to_typeref(typ)]
      |> list.append(list.flat_map(
        [closure, ..arguments],
        list_expr_direct_type_references,
      ))
    closure.Op(typ:, arguments:, ..) ->
      [type_to_typeref(typ)]
      |> list.append(list.flat_map(arguments, list_expr_direct_type_references))
    closure.Let(typ:, value:, body:, ..) ->
      [type_to_typeref(typ)]
      |> list.append(list_expr_direct_type_references(value))
      |> list.append(list_expr_direct_type_references(body))
    closure.If(typ:, condition:, then:, els:) ->
      [type_to_typeref(typ)]
      |> list.append(list_expr_direct_type_references(condition))
      |> list.append(list_expr_direct_type_references(then))
      |> list.append(list_expr_direct_type_references(els))
  }
}

fn type_to_typeref(typ: core.Type) -> TypeRef {
  case typ {
    core.NamedType(id:, ..) -> CustomTypeRef(id)
    core.FunctionType(parameters:, return:) ->
      FunctionTypeRef(
        list.map(parameters, type_to_typeref),
        type_to_typeref(return),
      )
    core.Unbound(..) -> HoleTypeRef
  }
}

type TypeRef {
  // TODO: need module + name!
  CustomTypeRef(name: String)
  FunctionTypeRef(params: List(TypeRef), return: TypeRef)
  HoleTypeRef
}

fn register_type_group(
  ctx: Context,
  ts: List(TypeRef),
) -> Result(Context, Error) {
  // first we register the type indices for each element of the group
  let ctx =
    list.fold(ts, ctx, fn(ctx, t) {
      case t {
        CustomTypeRef(name:) -> {
          let assert Ok(c) = resolve_custom_type(ctx.interfaces, name)
          // TODO: also register variant indices?
          Context(
            ..ctx,
            types: dict.insert(ctx.types, t, ctx.next_type_index),
            next_type_index: ctx.next_type_index + 1 + list.length(c.variants),
          )
        }
        FunctionTypeRef(..) ->
          Context(
            ..ctx,
            types: dict.insert(ctx.types, t, ctx.next_type_index),
            next_type_index: ctx.next_type_index + 1,
          )
        HoleTypeRef -> ctx
      }
    })

  let wasm_types =
    list.flat_map(ts, fn(t) {
      case t {
        CustomTypeRef(name:) -> generate_custom_type(ctx, name)
        FunctionTypeRef(params:, return:) ->
          generate_function_type(ctx, params, return)
        HoleTypeRef -> []
      }
    })

  use #(mb, _ids) <- result.map(
    wasm.add_sub_type_group(ctx.mb, wasm_types)
    |> result.map_error(WasmError),
  )
  Context(..ctx, mb:)
}

fn generate_function_type(
  ctx: Context,
  params: List(TypeRef),
  return: TypeRef,
) -> List(wasm.SubType) {
  [
    wasm.SubFinal(
      super_types: [],
      definition: wasm.Func(
        name: None,
        params: list.map(params, ref_to_wasm_value_type(ctx, _)),
        result: [ref_to_wasm_value_type(ctx, return)],
      ),
    ),
  ]
}

fn generate_custom_type(ctx: Context, name: String) {
  let assert Ok(t) = resolve_custom_type(ctx.interfaces, name)
  let assert Ok(base_idx) = dict.get(ctx.types, CustomTypeRef(name))
  let base =
    wasm.SubOpen(
      super_types: [],
      definition: wasm.Struct(name: Some(t.name), field_types: []),
    )
  let variants =
    list.map(t.variants, fn(v) {
      wasm.SubFinal(
        super_types: [base_idx],
        definition: wasm.Struct(
          name: Some(v.name),
          field_types: list.map(v.fields, fn(field) {
            typed_ast_type_to_ref(field.item.typ)
            |> ref_to_wasm_value_type(ctx, _)
            |> wasm.ValueType(
              name: field.label,
              mutable: wasm.Immutable,
              value_type: _,
            )
          }),
        ),
      )
    })
  [base, ..variants]
}

fn ref_to_wasm_value_type(ctx: Context, r: TypeRef) -> wasm.ValueType {
  case r {
    HoleTypeRef -> wasm.Ref(wasm.NonNull(wasm.AbstractAny))
    t -> {
      let assert Ok(id) = dict.get(ctx.types, t)
      wasm.Ref(wasm.NonNull(wasm.ConcreteType(id)))
    }
  }
}
