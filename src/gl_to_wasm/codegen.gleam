import gig/core
import gig/gen_names
import gig/typed_ast.{type ModuleInterface}
import gl_to_wasm/closure
import gl_to_wasm/graph
import gl_to_wasm/prelude
import gl_wasm/wasm
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/result
import gleam/set
import gleam/string
import listx
import pprint

type Context {
  Context(
    mb: wasm.ModuleBuilder,
    fbs: List(wasm.CodeBuilder),
    types: Dict(TypeRef, Int),
    next_type_index: Int,
    interfaces: Dict(String, ModuleInterface),
    functions: Dict(String, Int),
    module: closure.Module,
  )
}

pub type Error {
  WasmError(String)
  OtherError(String)
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
// (TODO: the func is actually a closure, that hasn't been taken into account.)
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

fn get_closure_representation(parameters: List(core.Type), return: core.Type) {
  let parameters = list.map(parameters, type_to_typeref)
  let return = type_to_typeref(return)
  let f_type = FunctionTypeRef([HoleTypeRef, ..parameters], return)
  let c_type = tuple_ref(2)
  // let e_type = tuple_ref(list.length(closure.environment))
  #(c_type, f_type)
}

pub fn codegen_module(
  interfaces: Dict(String, ModuleInterface),
  module_name: String,
  module: closure.Module,
) -> Result(wasm.ModuleBuilder, Error) {
  let mb = wasm.create_module_builder(Some(module_name))
  let assert Ok(mb) = prelude.add_prelude_types(mb)
  let types =
    dict.from_list([
      #(CustomTypeRef("Int"), prelude.int_index),
      #(CustomTypeRef("Float"), prelude.float_index),
      #(CustomTypeRef("Bool"), prelude.bool_index),
      #(CustomTypeRef("Nil"), prelude.nil_index),
      #(CustomTypeRef("List"), prelude.list_index),
      #(CustomTypeRef("String"), prelude.string_index),
      #(CustomTypeRef("BitArray"), prelude.bit_array_index),
      #(CustomTypeRef("Result"), prelude.result_index),
    ])
  let ctx =
    Context(
      mb:,
      fbs: [],
      types:,
      next_type_index: prelude.next_index,
      interfaces:,
      functions: dict.new(),
      module:,
    )
  use ctx <- result.try(register_types(ctx, module))
  use ctx <- result.try(register_imports(ctx, module))
  use ctx <- result.try(register_functions(ctx, module))
  use ctx <- result.try(build_functions(ctx, module))
  echo ctx.mb
  todo
}

fn register_imports(
  ctx: Context,
  module: closure.Module,
) -> Result(Context, Error) {
  // imports are any references globals that aren't from the local module
  let imports =
    list_module_used_globals(module)
    |> list.filter(fn(id) {
      !{
        list.any(module.functions, fn(f) { f.id == id })
        || list.any(module.closures, fn(c) { c.id == id })
      }
    })
    |> list.unique
  pprint.debug(imports)

  // find the interface the import is from
  use imports <- result.try(
    list.try_map(imports, resolve_function(ctx.interfaces, _)),
  )

  use mb <- result.map(
    list.try_fold(imports, ctx.mb, fn(mb, import_) {
      // TODO: lower the interfaces!
      let assert Ok(f_type_idx) =
        dict.get(ctx.types, type_to_typeref(todo as "Lower module interface"))
      wasm.import_function(
        mb,
        todo,
        Some(import_.name),
        wasm.ImportSource(core.builtin, import_.name),
      )
    })
    |> result.map_error(WasmError),
  )

  Context(..ctx, mb: mb)
}

fn resolve_function(
  interfaces: Dict(String, ModuleInterface),
  id: String,
) -> Result(typed_ast.FunctionDeclaration, Error) {
  // TODO: also resolve from other modules
  let assert Ok(prelude) = dict.get(interfaces, core.builtin)
  list.find(prelude.functions, fn(f) { f.name == id })
  |> result.replace_error(OtherError(
    "Could not resolve imported function " <> id,
  ))
}

fn list_module_used_globals(module: closure.Module) -> List(String) {
  list.flat_map(module.functions, fn(f) { list_expr_used_globals(f.body) })
  |> list.append(
    list.flat_map(module.closures, fn(c) { list_expr_used_globals(c.body) }),
  )
}

fn list_expr_used_globals(expr: closure.Expr) -> List(String) {
  case expr {
    closure.Literal(..) -> []
    closure.Local(..) -> []
    closure.Global(typ: _, id:) -> [id]
    closure.BindClosure(typ: _, id:, environment:) -> [
      id,
      ..list.flat_map(environment, list_expr_used_globals)
    ]
    closure.CallClosure(typ: _, closure:, arguments:) ->
      list.flat_map([closure, ..arguments], list_expr_used_globals)
    closure.CallGlobal(typ: _, id:, arguments:) -> [
      id,
      ..list.flat_map(arguments, list_expr_used_globals)
    ]
    closure.Op(typ: _, op: _, arguments:) ->
      list.flat_map(arguments, list_expr_used_globals)
    closure.Let(typ: _, name: _, value:, body:) ->
      list.flat_map([value, body], list_expr_used_globals)
    closure.If(typ: _, condition:, then:, els:) ->
      list.flat_map([condition, then, els], list_expr_used_globals)
    closure.Panic(..) -> []
  }
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
    use fb <- result.try(
      wasm.add_instruction(fb, wasm.End)
      |> result.map_error(fn(err) {
        echo "Generating function: " <> func.id
        pprint.debug(fb)
        echo err
      }),
    )
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
    closure.BindClosure(typ:, id:, environment:) -> {
      // get the closure
      let assert Ok(c) =
        list.find(ctx.module.closures, fn(closure) { closure.id == id })
      // generate the environment
      let assert Ok(e_type_idx) =
        dict.get(ctx.types, tuple_ref(list.length(c.environment)))
      use fb <- result.try(
        list.try_fold(environment, fb, fn(fb, arg) {
          codegen_expr(fb, arg, local_env, ctx)
        }),
      )
      use fb <- result.try(wasm.add_instruction(
        fb,
        wasm.StructNew(e_type_idx + 1),
      ))
      // create the function reference
      let assert Ok(f_idx) = dict.get(ctx.functions, id)
        as { "Function not found: " <> id }
      use fb <- result.try(wasm.add_instruction(fb, wasm.RefFunc(f_idx)))
      // create the struct
      let assert Ok(c_type_idx) = dict.get(ctx.types, tuple_ref(2))
      wasm.add_instruction(fb, wasm.StructNew(c_type_idx + 1))
    }
    closure.CallGlobal(id:, arguments:, ..) -> {
      use fb <- result.try(
        list.try_fold(arguments, fb, fn(fb, arg) {
          codegen_expr(fb, arg, local_env, ctx)
        }),
      )
      let assert Ok(f_idx) = dict.get(ctx.functions, id)
      wasm.add_instruction(fb, wasm.Call(f_idx))
    }
    closure.CallClosure(typ: _, closure:, arguments:) -> {
      let assert core.FunctionType(parameters:, return:) = closure.typ
      let #(c_type, f_type) = get_closure_representation(parameters, return)
      // create a local
      let assert Ok(c_type_idx) = dict.get(ctx.types, c_type)
      // TODO: don't do this for types with one variant!
      let c_type_idx = c_type_idx + 1
      use #(fb, local_idx) <- result.try(wasm.add_local(
        fb,
        wasm.Ref(wasm.NonNull(wasm.ConcreteType(c_type_idx))),
        None,
      ))
      // generate the closure struct and store it
      use fb <- result.try(codegen_expr(fb, closure, local_env, ctx))
      use fb <- result.try(wasm.add_instruction(
        fb,
        wasm.RefCast(wasm.NonNull(wasm.ConcreteType(c_type_idx))),
      ))
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
      let assert Ok(f_type_idx) = dict.get(ctx.types, f_type)
        as string.inspect(f_type)
      use fb <- result.try(wasm.add_instruction(
        fb,
        wasm.RefCast(wasm.NonNull(wasm.ConcreteType(f_type_idx))),
      ))
      wasm.add_instruction(fb, wasm.CallRef(f_type_idx))
    }
    closure.Op(typ: _, op: core.VariantCheck(variant:) as op, arguments:)
    | closure.Op(typ: _, op: core.FieldAccess(variant:, ..) as op, arguments:) -> {
      let assert [arg] = arguments
      let assert core.NamedType(id:, ..) = arg.typ
      let assert Ok(custom_type) = resolve_custom_type(ctx.interfaces, id)
      let assert Ok(#(variant, variant_idx)) =
        list.index_map(custom_type.variants, fn(variant, idx) {
          #(variant.name, #(variant, idx))
        })
        |> list.key_find(variant)
      let assert Ok(idx) = dict.get(ctx.types, type_to_typeref(arg.typ))
      use fb <- result.try(codegen_expr(fb, arg, local_env, ctx))
      let variant_type_idx = idx + variant_idx + 1
      let variant_wasm_type = wasm.NonNull(wasm.ConcreteType(variant_type_idx))
      case op {
        core.VariantCheck(..) -> {
          wasm.add_instruction(fb, wasm.RefTest(variant_wasm_type))
        }
        core.FieldAccess(field:, ..) -> {
          // TODO: lower the module interface and make this less awkward
          let assert Ok(idx) =
            list.index_map(variant.fields, fn(field, idx) {
              #(
                option.unwrap(field.label, gen_names.get_field_name("", idx)),
                idx,
              )
            })
            |> list.key_find(field)
          use fb <- result.try(wasm.add_instruction(
            fb,
            wasm.RefCast(variant_wasm_type),
          ))
          wasm.add_instruction(fb, wasm.StructGet(variant_type_idx, idx))
        }
      }
    }
    closure.Let(typ:, name:, value:, body:) -> {
      // create a local
      let assert Ok(wasm_type) = get_wasm_value_type(typ, ctx)
      use #(fb, local_idx) <- result.try(wasm.add_local(
        fb,
        wasm_type,
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
      let assert Ok(wasm_type) = get_wasm_value_type(typ, ctx)
      let bt = wasm.BlockValue(wasm_type)
      use fb <- result.try(codegen_expr(fb, condition, local_env, ctx))
      use fb <- result.try(wasm.add_instruction(fb, wasm.If(bt)))
      use fb <- result.try(codegen_expr(fb, then, local_env, ctx))
      use fb <- result.try(wasm.add_instruction(fb, wasm.Else))
      use fb <- result.try(codegen_expr(fb, els, local_env, ctx))
      wasm.add_instruction(fb, wasm.End)
    }
    closure.Panic(typ:, value:) ->
      // TODO: probably should use throw for this
      wasm.add_instruction(fb, wasm.Unreachable)
  }
}

fn get_wasm_value_type(
  typ: core.Type,
  ctx: Context,
) -> Result(wasm.ValueType, Nil) {
  case type_to_typeref(typ) {
    HoleTypeRef -> Ok(wasm.Ref(wasm.NonNull(wasm.AbstractAny)))
    ref ->
      dict.get(ctx.types, ref)
      |> result.map(fn(type_idx) {
        wasm.Ref(wasm.NonNull(wasm.ConcreteType(type_idx)))
      })
  }
}

fn tuple_ref(size: Int) -> TypeRef {
  CustomTypeRef("Tuple" <> int.to_string(size))
}

fn register_functions(
  ctx: Context,
  module: closure.Module,
) -> Result(Context, Error) {
  // TODO: externals
  let functions =
    list.map(module.functions, fn(func) {
      let assert Ok(type_idx) =
        dict.get(ctx.types, type_to_typeref(func.typ.typ))
      #(
        func.id,
        wasm.FunctionSignature(
          type_idx,
          Some(func.id),
          Some(list.map(func.parameters, fn(param) { param.name })),
        ),
      )
    })
  let closures =
    list.map(module.closures, fn(closure) {
      let assert core.FunctionType(parameters:, return:) = closure.typ.typ
      let #(_c_type, f_type) = get_closure_representation(parameters, return)
      let assert Ok(type_idx) = dict.get(ctx.types, f_type)
      #(
        closure.id,
        wasm.FunctionSignature(
          type_idx,
          Some(closure.id),
          Some([
            "$env",
            ..list.map(closure.parameters, fn(param) { param.name })
          ]),
        ),
      )
    })
  let functions = list.append(functions, closures)
  use #(mb, fbs) <- result.map(
    wasm.create_function_builders(ctx.mb, list.map(functions, pair.second))
    |> result.map_error(WasmError),
  )
  let functions =
    list.index_map(functions, fn(func, idx) { #(func.0, idx) })
    |> dict.from_list
  Context(..ctx, mb:, fbs:, functions:)
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
  // TODO: because we want to erase types from function signatures, we can't just generate the
  // types as if they were one-to-one with the gleam types.
  let types = list_module_direct_type_references(module)
  let types =
    list_indirect_type_references(types, ctx.interfaces)
    |> set.to_list
    |> list.filter(fn(t) { !dict.has_key(ctx.types, t) })
  let graph = construct_type_graph(types, ctx.interfaces)
  let assert Ok(sorted) = graph.squash_cycles(graph) |> graph.topological_sort
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
  let assert Ok(t) = resolve_custom_type(interfaces, name) as name
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
    typed_ast.TupleType(elements:) -> tuple_ref(list.length(elements))
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
  |> result.try_recover(fn(_) {
    // deal with tuples since we currently don't in lowering
    case name {
      "Tuple" <> size -> {
        int.parse(size)
        |> result.map(fn(size) {
          let vars = list.map(list.range(1, size), typed_ast.TypeVarId)
          let el_types = list.map(vars, typed_ast.VariableType)
          let el_names =
            list.map(listx.sane_range(size), gen_names.get_field_name(
              "field",
              _,
            ))
          let typ =
            typed_ast.Poly(vars, typed_ast.NamedType("gleam", name, el_types))
          let variant =
            typed_ast.Variant(
              typ:,
              name:,
              fields: list.map(list.zip(el_names, el_types), fn(el) {
                let #(label, typ) = el
                typed_ast.Field(
                  label: Some(label),
                  item: typed_ast.HoleAnno(typ, ""),
                )
              }),
            )
          typed_ast.CustomType(
            typ:,
            name:,
            publicity: typed_ast.Private,
            opaque_: False,
            parameters: el_names,
            variants: [variant],
          )
        })
      }
      _ -> Error(Nil)
    }
  })
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
  let from_closures =
    list.flat_map(module.closures, fn(closure) {
      let assert core.FunctionType(parameters:, return:) = closure.typ.typ
      let #(c_type, f_type) = get_closure_representation(parameters, return)
      let e_type = tuple_ref(list.length(closure.environment))
      [f_type, c_type, e_type]
      |> list.append(list_expr_direct_type_references(closure.body))
    })
  list.append(from_types, from_externals)
  |> list.append(from_functions)
  |> list.append(from_closures)
}

fn list_expr_direct_type_references(expr: closure.Expr) -> List(TypeRef) {
  case expr {
    closure.Literal(typ:, ..)
    | closure.Local(typ:, ..)
    | closure.Global(typ:, ..)
    | closure.Panic(typ:, ..) -> [type_to_typeref(typ)]
    closure.BindClosure(..) ->
      // All closure-related types are already listed at the signature level
      []
    closure.CallClosure(typ:, closure:, arguments:) ->
      {
        let assert core.FunctionType(parameters:, return:) = closure.typ
        let #(c_type, f_type) = get_closure_representation(parameters, return)
        [c_type, f_type, type_to_typeref(typ)]
      }
      |> list.append(list.flat_map(
        [closure, ..arguments],
        list_expr_direct_type_references,
      ))
    closure.CallGlobal(typ:, arguments:, ..) ->
      [type_to_typeref(typ)]
      |> list.append(list.flat_map(arguments, list_expr_direct_type_references))
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
        params: list.map(params, fn(_) {
          wasm.Ref(wasm.NonNull(wasm.AbstractAny))
        }),
        result: [wasm.Ref(wasm.NonNull(wasm.AbstractAny))],
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
