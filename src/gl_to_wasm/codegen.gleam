import gig/core
import gig/mono
import gl_to_wasm/graph
import gl_wasm/wasm
import gleam/bit_array
import gleam/bool
import gleam/bytes_tree
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/result
import gleam/set
import glexer
import snag.{type Snag}

// Notes on mono:
//  - it would be nice to extract the types that aren't constructed within Gleam's type system
//    (String, BitArray, Int, Float, UtfCodePoint, perhaps Bool & Nil)
//  - the fact that builtin functions are omitted is awkward

pub type Type {
  // True closures take an environment struct as their first argument, which
  // until now has been implicit, but for codegen needs to be explicit.
  AbstractEnv
  Env(closure_name: String)
  NamedType(name: String)
  NamedTypeVariant(type_name: String, variant_name: String)
  FunctionType(parameters: List(Type), return: Type)
}

pub type FunctionSignature {
  FunctionSignature(parameters: List(mono.Type), return: mono.Type)
}

type Context {
  Context(
    mb: wasm.ModuleBuilder,
    fbs: List(wasm.CodeBuilder),
    types: Dict(Type, Int),
    next_type_index: Int,
    functions: Dict(String, Int),
    module: mono.Context,
    strings: Dict(String, #(Int, Int)),
  )
}

/// Generate a WASM module for a gig/mono module.
/// A mono "module" typically results from many Gleam modules.
pub fn codegen(
  module: mono.Context,
  module_name: String,
  main_name: String,
) -> Result(wasm.ModuleBuilder, Snag) {
  let mb = wasm.create_module_builder(Some(module_name))
  let ctx =
    Context(
      mb:,
      fbs: [],
      types: dict.new(),
      next_type_index: 0,
      functions: dict.new(),
      module:,
      strings: dict.new(),
    )
  use ctx <- result.try(register_types(ctx) |> snag.context("register_types"))
  use ctx <- result.try(
    register_imports(ctx) |> snag.context("register_imports"),
  )
  use ctx <- result.try(register_data(ctx))
  use ctx <- result.try(build_builtins(ctx) |> snag.context("build_builtins"))
  use ctx <- result.try(
    register_functions(ctx) |> snag.context("register_functions"),
  )
  use ctx <- result.try(
    build_functions(ctx, module) |> snag.context("build_functions"),
  )
  register_exports(ctx, module, main_name) |> snag.context("register_exports")
}

fn register_data(ctx: Context) -> Result(Context, Snag) {
  let string_literals = find_string_literals(ctx.module)
  let #(data, _, strings) =
    list.fold(string_literals, #(bytes_tree.new(), 0, dict.new()), fn(acc, str) {
      let assert Ok(str) = glexer.unescape_string(str)
      let #(data, offset, index) = acc
      use <- bool.guard(result.is_ok(dict.get(index, str)), acc)
      let bytes = bit_array.from_string(str)
      let size = bit_array.byte_size(bytes)
      let data = bytes_tree.append(data, bytes)
      #(data, offset + size, dict.insert(index, str, #(offset, size)))
    })
  use mb <- result.map(
    wasm.add_passive_data(ctx.mb, bytes_tree.to_bit_array(data))
    |> to_snag("wasm.add_passive_data"),
  )
  Context(..ctx, mb:, strings:)
}

fn find_string_literals(module: mono.Context) -> List(String) {
  list.flat_map(module.functions, fn(func) {
    find_string_literals_exp(func.body)
  })
}

fn find_string_literals_exp(exp: mono.Exp) -> List(String) {
  case exp {
    mono.Literal(val: core.String(value:), ..) -> [value]
    mono.Literal(..) -> []
    mono.Local(..) -> []
    mono.ConstantGlobal(..) -> []
    mono.CallGlobal(args:, ..) -> list.flat_map(args, find_string_literals_exp)
    mono.BindClosure(environment:, ..) ->
      list.flat_map(environment, find_string_literals_exp)
    mono.CallClosure(fun:, args:, ..) ->
      list.flat_map([fun, ..args], find_string_literals_exp)
    mono.Op(args:, ..) -> list.flat_map(args, find_string_literals_exp)
    mono.Let(val:, exp:, ..) ->
      list.flat_map([val, exp], find_string_literals_exp)
    mono.If(cond:, then_exp:, else_exp:, ..) ->
      list.flat_map([cond, then_exp, else_exp], find_string_literals_exp)
    mono.Panic(value:, ..) -> find_string_literals_exp(value)
  }
}

fn register_exports(
  ctx: Context,
  _module: mono.Context,
  main_name: String,
) -> Result(wasm.ModuleBuilder, Snag) {
  let assert Ok(idx) = dict.get(ctx.functions, main_name)
  use mb <- result.try(
    wasm.add_export(ctx.mb, wasm.ExportFunction(main_name, idx))
    |> to_snag("wasm.add_export(" <> main_name <> ")"),
  )
  wasm.add_export(mb, wasm.ExportMemory("memory", 0))
  |> to_snag("wasm.add_export(memory)")
}

// === Register types ===

fn register_types(ctx: Context) -> Result(Context, Snag) {
  let assert Ok(groups) =
    construct_type_graph(ctx.module)
    |> graph.squash_cycles
    |> graph.topological_sort
  let groups =
    list.filter(groups, fn(group) {
      group != [AbstractEnv]
      && group != [NamedType("Int")]
      && group != [NamedType("Float")]
      && group != [NamedType("UtfCodepoint")]
    })

  // TODO: perhaps we can make this immutable again when we initialize strings properly?
  let byte_array =
    wasm.Array(None, wasm.PackedType(None, wasm.Mutable, wasm.I8))
  use #(mb, id) <- result.try(
    wasm.add_type(ctx.mb, byte_array)
    |> to_snag("wasm.add_type(byte_array)"),
  )
  assert byte_array_index == id
  let ctx = Context(..ctx, mb:, next_type_index: id + 1)
  use ctx <- result.try(list.try_fold(groups, ctx, register_type_group))
  Ok(ctx)
}

fn register_type_group(ctx: Context, ts: List(Type)) -> Result(Context, Snag) {
  // first register the type indices for each element of the group
  let ctx =
    list.fold(ts, ctx, fn(ctx, t) {
      case t {
        AbstractEnv -> panic
        _ ->
          Context(
            ..ctx,
            types: dict.insert(ctx.types, t, ctx.next_type_index),
            next_type_index: ctx.next_type_index + 1,
          )
      }
    })

  // then generate the actual types, which may now reference eachother
  let assert Ok(wasm_types) =
    list.try_map(ts, fn(t) {
      case t {
        AbstractEnv -> panic
        Env(closure_name:) -> generate_closure_env_type(ctx, closure_name)
        NamedType(name: "String") -> generate_string_type()
        NamedType(name: "BitArray") -> generate_bit_array_type()
        NamedType(name:) -> generate_custom_type(ctx, name)
        NamedTypeVariant(type_name:, variant_name:) ->
          generate_variant_type(ctx, type_name, variant_name)
        FunctionType(parameters:, return:) ->
          generate_function_type(ctx, parameters, return)
      }
    })

  use #(mb, _ids) <- result.map(
    wasm.add_sub_type_group(ctx.mb, wasm_types)
    |> to_snag("wasm.add_sub_type_group"),
  )
  Context(..ctx, mb:)
}

const byte_array_index = 0

fn generate_string_type() -> Result(wasm.SubType, Snag) {
  // TODO: do we really still need this to be a struct?
  // I don't think we need to run-time distinguish this from anything anymore?
  wasm.SubFinal(
    [],
    wasm.Struct(Some("gleam/String"), [
      wasm.ValueType(Some("size"), wasm.Immutable, wasm.I32),
      wasm.ValueType(
        Some("bytes"),
        wasm.Immutable,
        wasm.Ref(wasm.NonNull(wasm.ConcreteType(byte_array_index))),
      ),
    ]),
  )
  |> Ok
}

fn generate_bit_array_type() -> Result(wasm.SubType, Snag) {
  wasm.SubFinal(
    [],
    wasm.Struct(Some("gleam/BitArray"), [
      wasm.ValueType(Some("byte_size"), wasm.Immutable, wasm.I32),
      wasm.ValueType(Some("bit_size"), wasm.Immutable, wasm.I64),
      wasm.ValueType(
        Some("bytes"),
        wasm.Immutable,
        wasm.Ref(wasm.NonNull(wasm.ConcreteType(byte_array_index))),
      ),
    ]),
  )
  |> Ok
}

fn generate_closure_env_type(
  ctx: Context,
  closure_name: String,
) -> Result(wasm.SubType, Snag) {
  use fun <- result.map(
    list.find(ctx.module.functions, fn(f) { f.name == closure_name })
    |> result.replace_error(snag.new("Closure " <> closure_name <> " not found")),
  )
  generate_struct(ctx, "Env_" <> closure_name, [], fun.environment)
}

fn generate_custom_type(
  ctx: Context,
  name: String,
) -> Result(wasm.SubType, Snag) {
  use def <- result.map(
    list.find(ctx.module.types, fn(t) { t.name == name })
    |> result.replace_error(snag.new("CustomType " <> name <> " not found")),
  )
  wasm.SubOpen(
    super_types: [],
    definition: wasm.Struct(name: Some(def.name), field_types: []),
  )
}

fn generate_variant_type(
  ctx: Context,
  type_name: String,
  variant_name: String,
) -> Result(wasm.SubType, Snag) {
  use def <- result.try(
    list.find(ctx.module.types, fn(t) { t.name == type_name })
    |> result.replace_error(snag.new("CustomType " <> type_name <> " not found")),
  )
  let assert Ok(idx) = dict.get(ctx.types, NamedType(def.name))
  use variant <- result.map(
    list.find(def.variants, fn(v) { v.name == variant_name })
    |> result.replace_error(snag.new("Variant " <> variant_name <> " not found")),
  )
  generate_struct(ctx, variant_name, [idx], variant.fields)
}

fn generate_function_type(
  ctx: Context,
  params: List(Type),
  return: Type,
) -> Result(wasm.SubType, Snag) {
  wasm.SubFinal(
    super_types: [],
    definition: wasm.Func(
      name: None,
      params: list.map(params, wasm_value_type(ctx, _)),
      result: [wasm_value_type(ctx, return)],
    ),
  )
  |> Ok
}

fn generate_struct(
  ctx: Context,
  name: String,
  super_types: List(Int),
  fields: List(mono.Field),
) -> wasm.SubType {
  wasm.SubFinal(
    super_types:,
    definition: wasm.Struct(
      name: Some(name),
      field_types: list.map(fields, fn(field) {
        map_type(field.typ)
        |> wasm_value_type(ctx, _)
        |> wasm.ValueType(
          name: Some(field.name),
          mutable: wasm.Immutable,
          value_type: _,
        )
      }),
    ),
  )
}

fn wasm_value_type(ctx: Context, t: Type) -> wasm.ValueType {
  case t {
    AbstractEnv -> wasm.Ref(wasm.NonNull(wasm.AbstractAny))
    NamedType(name: "Int") -> wasm.I64
    NamedType(name: "Float") -> wasm.F64
    NamedType(name: "UtfCodepoint") -> wasm.I32
    _ -> {
      let assert Ok(id) = dict.get(ctx.types, t)
      wasm.Ref(wasm.NonNull(wasm.ConcreteType(id)))
    }
  }
}

pub fn construct_type_graph(module: mono.Context) {
  let basic_types =
    ["String", "Int", "Float", "BitArray", "UtfCodepoint"]
    |> list.map(NamedType)
    |> set.from_list
  let graph =
    list.fold(module.types, #(basic_types, set.new()), fn(graph, ct) {
      let #(nodes, edges) = graph
      let ct_node = NamedType(name: ct.name)
      let nodes = set.insert(nodes, ct_node)
      list.fold(ct.variants, #(nodes, edges), fn(graph, variant) {
        let #(nodes, edges) = graph
        let vt_node =
          NamedTypeVariant(type_name: ct.name, variant_name: variant.name)
        let nodes = set.insert(nodes, vt_node)
        let edges = set.insert(edges, #(vt_node, ct_node))
        let edges =
          list.fold(variant.fields, edges, fn(edges, field) {
            set.insert(edges, #(vt_node, map_type(field.typ)))
          })
        #(nodes, edges)
      })
    })
  // TODO: should get builtins from mono!
  let builtin_types = [
    FunctionType([NamedType("String")], NamedType("String")),
    FunctionType([NamedType("String")], NamedType("Nil")),
  ]
  let func_types =
    list.flat_map(module.types, fn(ct) {
      list.map(ct.variants, map_constructor_type(ct, _))
    })
    |> list.append(list.map(module.externals, fn(ext) { map_type(ext.typ) }))
    |> list.append(list.map(module.functions, map_func_type))
    |> list.append(builtin_types)
  let graph =
    list.fold(func_types, graph, fn(graph, func_type) {
      let #(nodes, edges) = graph
      let nodes = set.insert(nodes, func_type)
      let types = case func_type {
        FunctionType(parameters:, return:) -> [return, ..parameters]
        _ -> []
      }
      let edges =
        list.fold(types, edges, fn(edges, t) {
          set.insert(edges, #(func_type, t))
        })
      #(nodes, edges)
    })
  let graph =
    list.fold(module.functions, graph, fn(graph, func) {
      use <- bool.guard(func.environment == [], graph)
      let #(nodes, edges) = graph
      let env_type = Env(func.name)
      let nodes = set.insert(nodes, env_type)
      let edges =
        list.fold(func.environment, edges, fn(edges, field) {
          set.insert(edges, #(env_type, map_type(field.typ)))
        })
      #(nodes, edges)
    })
  let #(nodes, edges) = graph
  #(set.to_list(nodes), set.to_list(edges))
}

fn map_constructor_type(
  custom_type: mono.CustomType,
  variant: mono.Variant,
) -> Type {
  FunctionType(
    parameters: list.map(variant.fields, fn(field) { map_type(field.typ) }),
    return: NamedTypeVariant(
      type_name: custom_type.name,
      variant_name: variant.name,
    ),
  )
}

fn map_func_type(func: mono.Closure) -> Type {
  let parameters = list.map(func.parameters, fn(field) { map_type(field.typ) })
  let parameters = case func.environment {
    [] -> parameters
    _ -> [AbstractEnv, ..parameters]
  }
  FunctionType(parameters: parameters, return: map_type(func.return))
}

fn map_type(typ: mono.Type) -> Type {
  case typ {
    mono.NamedType(..) -> NamedType(mono.type_name(typ))
    mono.FunctionType(parameters:, return:) ->
      FunctionType(list.map(parameters, map_type), map_type(return))
  }
}

// === Register imports ===

fn register_imports(ctx: Context) -> Result(Context, Snag) {
  use mb <- result.map(
    list.try_fold(ctx.module.externals, ctx.mb, fn(mb, external) {
      let assert Ok(f_type_idx) = dict.get(ctx.types, map_type(external.typ))
      wasm.import_function(
        mb,
        f_type_idx,
        Some(external.internal_name),
        wasm.ImportSource(external.module, external.external_name),
      )
      |> to_snag("wasm.import_function(" <> external.internal_name <> ")")
    }),
  )

  let functions =
    list.index_map(ctx.module.externals, fn(external, idx) {
      #(external.internal_name, idx)
    })
    |> dict.from_list
  Context(..ctx, mb:, functions:)
}

// === Register functions ===
fn register_functions(ctx: Context) -> Result(Context, Snag) {
  let functions =
    list.map(ctx.module.functions, fn(func) {
      let assert Ok(type_idx) = dict.get(ctx.types, map_func_type(func))
      #(
        func.name,
        wasm.FunctionSignature(
          type_idx,
          Some(func.name),
          Some(
            list.map(func.parameters, fn(param) { param.name })
            |> fn(params) {
              case func.environment {
                [] -> params
                _ -> ["ENV", ..params]
              }
            },
          ),
        ),
      )
    })
  use #(mb, fbs) <- result.try(
    wasm.create_function_builders(ctx.mb, list.map(functions, pair.second))
    |> to_snag("wasm.create_function_builders"),
  )
  let floor = dict.size(ctx.functions)
  let functions =
    list.index_map(functions, fn(func, idx) { #(func.0, floor + idx) })
    |> dict.from_list
    |> dict.merge(ctx.functions, _)
  let ctx = Context(..ctx, mb:, fbs:, functions:)
  Ok(ctx)
  // TODO: closures!
  // Closures will be called by reference. For that, they will need to be declared outside the code
  // section first. Ideally use an `(elem declare func $f)` but for now set a global. 
  // let floor = floor + list.length(module.functions)
  // use mb <- result.map(
  //   list.index_map(closures, fn(closure, index) { #(index + floor, closure) })
  //   |> list.try_fold(ctx.mb, fn(mb, closure) {
  //     let #(f_idx, #(id, wasm.FunctionSignature(type_idx, ..))) = closure
  //     use #(mb, gb) <- result.try(wasm.create_global_builder(
  //       mb,
  //       Some(id),
  //       wasm.Immutable,
  //       wasm.Ref(wasm.NonNull(wasm.ConcreteType(type_idx))),
  //     ))
  //     use gb <- result.try(wasm.add_instruction(gb, wasm.RefFunc(f_idx)))
  //     use gb <- result.try(wasm.add_instruction(gb, wasm.End))
  //     wasm.finalize_global(mb, gb)
  //   })
  //   |> result.map_error(WasmError),
  // )
  // Context(..ctx, mb:)
}

// === Build functions ===

fn build_functions(ctx: Context, module: mono.Context) -> Result(Context, Snag) {
  // A wasm.CodeBuilder has been pre-registered for each function
  list.zip(module.functions, ctx.fbs)
  |> list.try_fold(ctx, fn(ctx, build) {
    let #(func, fb) = build
    {
      let params = list.map(func.parameters, fn(param) { param.name })
      let params = case func.environment {
        [] -> params
        [_, ..] -> ["ENV", ..params]
      }
      let env_params = list.map(func.environment, fn(param) { param.name })
      let local_env =
        list.append(params, env_params)
        |> list.index_map(fn(param, idx) { #(param, idx) })
        |> dict.from_list

      use fb <- result.try(
        list.try_fold(func.environment, fb, fn(_fb, _param) {
          todo as "Implement environment unpacking"
        }),
      )

      // Generate the body of the function.
      use fb <- result.try(codegen_expr(fb, func.body, local_env, ctx))
      // Close and finalize the function.
      use fb <- result.try(
        wasm.add_instruction(fb, wasm.End)
        |> to_snag("wasm.add_instruction(End)"),
      )
      use mb <- result.map(
        wasm.finalize_function(ctx.mb, fb) |> to_snag("wasm.finalize_function"),
      )
      Context(..ctx, mb:)
    }
    |> snag.context("building " <> func.name)
  })
}

fn to_snag(result: Result(a, String), context: String) -> Result(a, Snag) {
  result |> result.map_error(snag.new) |> snag.context(context)
}

fn codegen_expr(
  fb: wasm.CodeBuilder,
  expr: mono.Exp,
  local_env: Dict(String, Int),
  ctx: Context,
) -> Result(wasm.CodeBuilder, Snag) {
  case expr {
    mono.Literal(typ: _, val:) ->
      {
        case val {
          core.NilVal -> todo
          core.Bool(value: _) -> todo
          core.Int(value: _) -> todo
          core.Float(value: _) -> todo
          core.String(value:) -> {
            // TODO: String literals should be initialized form the data or element section
            let assert Ok(type_index) = dict.get(ctx.types, NamedType("String"))
            let assert Ok(value) = glexer.unescape_string(value)
            let assert Ok(#(offset, size)) = dict.get(ctx.strings, value)
            let assert Ok(offset) = wasm.int32_unsigned(offset)
            let assert Ok(size) = wasm.int32_unsigned(size)
            use #(fb, bytes_idx) <- result.try(
              wasm.add_local(
                fb,
                wasm.Ref(wasm.NonNull(wasm.ConcreteType(byte_array_index))),
                Some("bytes"),
              )
              |> to_snag("wasm.add_local"),
            )
            let assert Ok(zero) = wasm.int32_unsigned(0)
            use fb <- result.try(
              wasm.add_instruction(fb, wasm.I32Const(zero))
              |> to_snag("wasm.add_instruction(I32Const)"),
            )
            use fb <- result.try(
              wasm.add_instruction(fb, wasm.I32Const(size))
              |> to_snag("wasm.add_instruction(I32Const)"),
            )
            // second field of the struct
            use fb <- result.try(
              wasm.add_instruction(fb, wasm.ArrayNew(byte_array_index))
              |> to_snag("wasm.add_instruction(ArrayNew)"),
            )
            use fb <- result.try(
              wasm.add_instruction(fb, wasm.LocalSet(bytes_idx))
              |> to_snag("wasm.add_instruction(LocalSet)"),
            )
            use fb <- result.try(
              wasm.add_instruction(fb, wasm.LocalGet(bytes_idx))
              |> to_snag("wasm.add_instruction(LocalGet)"),
            )
            use fb <- result.try(
              wasm.add_instruction(fb, wasm.I32Const(zero))
              |> to_snag("wasm.add_instruction(I32Const)"),
            )
            use fb <- result.try(
              wasm.add_instruction(fb, wasm.I32Const(offset))
              |> to_snag("wasm.add_instruction(I32Const)"),
            )
            use fb <- result.try(
              wasm.add_instruction(fb, wasm.I32Const(size))
              |> to_snag("wasm.add_instruction(I32Const)"),
            )
            use fb <- result.try(
              wasm.add_instruction(fb, wasm.ArrayInitData(byte_array_index, 0))
              |> to_snag("wasm.add_instruction(ArrayInitData)"),
            )
            // create the String struct
            use fb <- result.try(
              wasm.add_instruction(fb, wasm.I32Const(size))
              |> to_snag("wasm.add_instruction(I32Const)"),
            )
            use fb <- result.try(
              wasm.add_instruction(fb, wasm.LocalGet(bytes_idx))
              |> to_snag("wasm.add_instruction(LocalGet)"),
            )
            wasm.add_instruction(fb, wasm.StructNew(type_index))
            |> to_snag("wasm.add_instruction(StructNew)")
          }
          core.BitArray(size: _) -> todo
        }
      }
      |> snag.context("Literal(String)")
    mono.Local(typ: _, var:) ->
      {
        let assert Ok(idx) = dict.get(local_env, var)
        wasm.add_instruction(fb, wasm.LocalGet(idx))
        |> to_snag("wasm.add_instruction(LocalGet)")
      }
      |> snag.context("Local " <> var)
    mono.ConstantGlobal(typ: _, id: _) -> todo
    mono.CallGlobal(typ: _, id:, args:) ->
      {
        use fb <- result.try(
          list.try_fold(args, fb, fn(fb, arg) {
            codegen_expr(fb, arg, local_env, ctx)
          }),
        )
        let assert Ok(f_idx) = dict.get(ctx.functions, id)
          as { "Missing global function " <> id }
        wasm.add_instruction(fb, wasm.Call(f_idx))
        |> to_snag("wasm.add_instruction(Call)")
      }
      |> snag.context("CallGlobal " <> id)
    mono.BindClosure(typ: _, id: _, environment: _) -> todo
    mono.CallClosure(typ: _, fun:, args:) ->
      {
        use fb <- result.try(
          list.try_fold(args, fb, fn(fb, arg) {
            codegen_expr(fb, arg, local_env, ctx)
          }),
        )
        use fb <- result.try(codegen_expr(fb, fun, local_env, ctx))
        todo
      }
      |> snag.context("CallClosure")
    mono.Op(typ: _, op: _, args: _) -> todo
    mono.Let(typ: _, var: "_" <> _, val:, exp:) -> {
      // we don't create and set locals for anonymous variables
      use fb <- result.try(codegen_expr(fb, val, local_env, ctx))
      use fb <- result.try(
        wasm.add_instruction(fb, wasm.Drop)
        |> to_snag("wasm.add_instruction(Drop)"),
      )
      // generate the body
      codegen_expr(fb, exp, local_env, ctx)
    }
    mono.Let(typ: _, var:, val:, exp:) ->
      {
        // create a local
        let wasm_type = wasm_value_type(ctx, map_type(val.typ))
        use #(fb, local_idx) <- result.try(
          wasm.add_local(fb, wasm_type, Some(var))
          |> to_snag("wasm.add_local"),
        )
        // generate the assigned value
        use fb <- result.try(codegen_expr(fb, val, local_env, ctx))
        // set the local
        use fb <- result.try(
          wasm.add_instruction(fb, wasm.LocalSet(local_idx))
          |> to_snag("wasm.add_instruction(LocalSet)"),
        )
        let local_env = dict.insert(local_env, var, local_idx)
        // generate the body
        codegen_expr(fb, exp, local_env, ctx)
      }
      |> snag.context("Let " <> var)
    mono.If(typ: _, cond: _, then_exp: _, else_exp: _) -> todo
    mono.Panic(typ: _, value: _) -> todo
  }
}

fn bit_array_to_bytes(bit_array: BitArray) -> List(Int) {
  case bit_array {
    <<byte:size(8), rest:bits>> -> [byte, ..bit_array_to_bytes(rest)]
    _ -> []
  }
}

// === built-in functions ===

fn build_builtins(ctx: Context) -> Result(Context, Snag) {
  // add core wasm imports
  let print_type =
    wasm.Func(name: None, params: [wasm.I32, wasm.I32], result: [])
  use #(mb, print_type_idx) <- result.try(
    wasm.add_type(ctx.mb, print_type) |> to_snag("wasm.add_type(print_type)"),
  )
  use mb <- result.try(
    wasm.import_function(
      mb,
      print_type_idx,
      Some("__print"),
      wasm.ImportSource("host", "print"),
    )
    |> to_snag("wasm.import_function(print)"),
  )
  let functions =
    dict.insert(ctx.functions, "__print", dict.size(ctx.functions))
  // create memory
  use mb <- result.try(
    wasm.add_memory(mb, 1, None)
    |> to_snag("wasm.add_memory"),
  )
  // create built-ins
  let ctx = Context(..ctx, mb:, functions:)
  let builtins = [
    #(
      "inspect_String",
      FunctionType([NamedType("String")], NamedType("String")),
      build_inspect_string,
    ),
    #(
      "print_string",
      FunctionType([NamedType("String")], NamedType("Nil")),
      build_print_string,
    ),
  ]
  use ctx <- result.map(
    list.try_fold(builtins, ctx, fn(ctx, builtin) {
      let #(name, func_type, build) = builtin
      {
        let assert Ok(type_idx) = dict.get(ctx.types, func_type)
        let sig = wasm.FunctionSignature(type_idx, Some(name), None)
        use #(mb, fb) <- result.try(
          wasm.create_function_builder(ctx.mb, sig)
          |> to_snag("wasm.create_function_builder"),
        )
        let functions =
          dict.insert(ctx.functions, name, dict.size(ctx.functions))
        use fb <- result.try(build(fb, ctx))
        use mb <- result.map(
          wasm.finalize_function(mb, fb)
          |> to_snag("wasm.finalize_function"),
        )
        Context(..ctx, mb:, functions:)
      }
      |> snag.context("build " <> name)
    }),
  )
  ctx
}

fn build_print_string(
  fb: wasm.CodeBuilder,
  ctx: Context,
) -> Result(wasm.CodeBuilder, Snag) {
  // super ugly: copy string to a fixed location in linear memory and call an import
  let assert Ok(string_type_idx) = dict.get(ctx.types, NamedType("String"))
  let assert Ok(nil_type_idx) =
    dict.get(ctx.types, NamedTypeVariant("Nil", "Nil"))
  let assert Ok(zero) = wasm.int32_unsigned(0)
  let assert Ok(one) = wasm.int32_unsigned(1)
  let assert Ok(print_idx) = dict.get(ctx.functions, "__print")
  use #(fb, bytes_idx) <- result.try(
    wasm.add_local(
      fb,
      wasm.Ref(wasm.NonNull(wasm.ConcreteType(byte_array_index))),
      Some("bytes"),
    )
    |> to_snag("wasm.add_local(bytes)"),
  )
  use #(fb, len_idx) <- result.try(
    wasm.add_local(fb, wasm.I32, Some("len"))
    |> to_snag("wasm.add_local(len)"),
  )
  use #(fb, idx_idx) <- result.try(
    wasm.add_local(fb, wasm.I32, Some("idx"))
    |> to_snag("wasm.add_local(idx)"),
  )
  [
    // initialize locals
    wasm.LocalGet(0),
    wasm.StructGet(string_type_idx, 0),
    wasm.LocalSet(len_idx),
    wasm.LocalGet(0),
    wasm.StructGet(string_type_idx, 1),
    wasm.LocalSet(bytes_idx),
    // block for exiting loop
    wasm.Block(wasm.BlockEmpty),
    // loop for looping
    wasm.Loop(wasm.BlockEmpty),
    // get the current index
    wasm.LocalGet(idx_idx),
    // get the length
    wasm.LocalGet(len_idx),
    // if (idx >= len), break
    wasm.I32GeU,
    wasm.BranchIf(1),
    // address to write in linear memory
    wasm.LocalGet(idx_idx),
    // get bytes[idx]
    wasm.LocalGet(bytes_idx),
    wasm.LocalGet(idx_idx),
    wasm.ArrayGetS(byte_array_index),
    // copy to linear memory
    wasm.I32Store8(0, 0),
    // increment the current index
    wasm.LocalGet(idx_idx),
    wasm.I32Const(one),
    wasm.I32Add,
    wasm.LocalSet(idx_idx),
    // loop
    wasm.Branch(0),
    wasm.End,
    wasm.End,
    // now call the import
    wasm.I32Const(zero),
    wasm.LocalGet(len_idx),
    wasm.Call(print_idx),
    wasm.StructNew(nil_type_idx),
    wasm.End,
  ]
  |> list.try_fold(fb, wasm.add_instruction)
  |> to_snag("wasm.add_instruction")
}

fn build_inspect_string(
  fb: wasm.CodeBuilder,
  _ctx: Context,
) -> Result(wasm.CodeBuilder, Snag) {
  [
    wasm.LocalGet(0),
    wasm.End,
  ]
  |> list.try_fold(fb, wasm.add_instruction)
  |> to_snag("wasm.add_instruction(LocalGet)")
}
