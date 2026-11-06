import gig/core
import gig/mono
import gl_to_wasm/graph
import gl_wasm/wasm
import gleam/bit_array
import gleam/bool
import gleam/bytes_tree
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/result
import gleam/set
import gleam/string
import glexer
import snag.{type Snag}

// Notes on mono:
//  - it would be nice to extract the types that aren't constructed within Gleam's type system
//    (String, BitArray, Int, Float, UtfCodePoint, perhaps Bool & Nil)
//  - the fact that builtin functions are omitted is awkward (and some are externals)

pub type ValType {
  // True closures take an environment struct as their first argument, which
  // until now has been implicit, but for codegen needs to be explicit.
  AbstractEnv
  Env(closure_name: String)
  NamedType(name: String)
  NamedTypeVariant(type_name: String, variant_name: String)
  Closure
}

pub type FunType {
  FunType(parameters: List(ValType), return: ValType)
}

pub type FunctionSignature {
  FunctionSignature(parameters: List(mono.Type), return: mono.Type)
}

type Context {
  Context(
    mb: wasm.ModuleBuilder,
    fbs: List(wasm.CodeBuilder),
    val_types: Dict(ValType, Int),
    fun_types: Dict(FunType, Int),
    next_type_index: Int,
    functions: Dict(String, Int),
    need_wrapping: List(String),
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
      val_types: dict.new(),
      fun_types: dict.new(),
      next_type_index: 0,
      functions: dict.new(),
      need_wrapping: [],
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
  use ctx <- result.try(build_functions(ctx) |> snag.context("build_functions"))
  use ctx <- result.try(build_wrappers(ctx) |> snag.context("build_wrappers"))
  use ctx <- result.try(register_elements(ctx))
  register_exports(ctx, main_name) |> snag.context("register_exports")
}

fn add_code(
  fb: wasm.CodeBuilder,
  code: List(wasm.Instruction),
) -> Result(wasm.CodeBuilder, Snag) {
  list.try_fold(code, fb, fn(fb, instr) {
    wasm.add_instruction(fb, instr)
    |> snag.map_error(fn(x) { x })
    |> snag.context(
      "wasm.add_instruction(" <> wasm.instruction_to_string(instr) <> ")",
    )
  })
}

fn register_elements(ctx: Context) -> Result(Context, Snag) {
  // Closures will be called by reference. For that, they will need to be declared outside the code
  // section first. We use an `(elem declare func $f)` but for that. 
  // Currently all functions are added, whether they are called by reference or not.
  use mb <- result.map(
    wasm.add_declarative_element(
      ctx.mb,
      int.range(
        from: 0,
        to: dict.size(ctx.functions),
        with: [],
        run: list.prepend,
      ),
    )
    |> to_snag("wasm.add_declarative_element"),
  )
  Context(..ctx, mb:)
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
  // TODO: make string literals for all types
  list.flat_map(module.functions, fn(func) {
    find_string_literals_exp(func.body)
  })
  |> list.append(["True", "False"])
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
      && group != [NamedType("Bool")]
      && group != [NamedTypeVariant("Bool", "False")]
      && group != [NamedTypeVariant("Bool", "True")]
    })

  // byte array is an implementation detail
  let byte_array =
    wasm.Array(None, wasm.PackedType(None, wasm.Mutable, wasm.I8))
  use #(mb, id) <- result.try(
    wasm.add_type(ctx.mb, byte_array)
    |> to_snag("wasm.add_type(byte_array)"),
  )
  assert byte_array_index == id

  // closure object (encapsulating an environment and a function)
  let closure =
    wasm.Struct(Some("Closure"), [
      wasm.ValueType(
        Some("env"),
        wasm.Immutable,
        wasm.Ref(wasm.NonNull(wasm.AbstractStruct)),
      ),
      wasm.ValueType(
        Some("func"),
        wasm.Immutable,
        wasm.Ref(wasm.NonNull(wasm.AbstractFunc)),
      ),
    ])
  use #(mb, id) <- result.try(
    wasm.add_type(mb, closure)
    |> to_snag("wasm.add_type(Closure)"),
  )
  assert closure_type_index == id

  let ctx =
    Context(
      ..ctx,
      mb:,
      next_type_index: id + 1,
      val_types: dict.from_list([#(Closure, closure_type_index)]),
    )
  use ctx <- result.try(list.try_fold(groups, ctx, register_type_group))
  use ctx <- result.try(register_function_types(ctx))

  Ok(ctx)
}

const builtin_instr = [
  #("and_bool", wasm.I32And),
  #("or_bool", wasm.I32Or),
  #("negate_bool", wasm.I32EqZ),
  #("eq_Int", wasm.I64Eq),
  #("lt_Int", wasm.I64LtS),
  #("gt_Int", wasm.I64GtS),
  #("lte_Int", wasm.I64LeS),
  #("gte_Int", wasm.I64GeS),
  #("add_Int", wasm.I64Add),
  #("sub_Int", wasm.I64Sub),
  #("mul_Int", wasm.I64Mul),
  #("div_Int", wasm.I64DivS),
  #("rem_Int", wasm.I64RemS),
  #("negate_Int", wasm.I64EqZ),
  #("eq_Float", wasm.F64Eq),
  #("lt_Float", wasm.F64Lt),
  #("gt_Float", wasm.F64Gt),
  #("lte_Float", wasm.F64Le),
  #("gte_Float", wasm.F64Ge),
  #("add_Float", wasm.F64Add),
  #("sub_Float", wasm.F64Sub),
  #("mul_Float", wasm.F64Mul),
  #("div_Float", wasm.F64Div),
]

fn find_builtin_instr(builtin_name) {
  list.find_map(builtin_instr, fn(builtin) {
    let #(name, instr) = builtin
    case name == builtin_name {
      True -> Ok(instr)
      False -> Error(Nil)
    }
  })
}

fn not_builtin(e: mono.External) -> Bool {
  result.is_error(find_builtin_instr(e.internal_name))
}

fn register_function_types(ctx: Context) -> Result(Context, Snag) {
  let module = ctx.module

  let need_wrapping = find_need_wrapping(module)

  // TODO: should get builtins from mono!
  let builtin_types = [
    FunType([NamedType("String")], NamedType("String")),
    FunType([NamedType("Bool")], NamedType("String")),
    FunType([NamedType("String")], NamedType("Nil")),
  ]
  let func_types =
    list.flat_map(module.types, fn(ct) {
      list.map(ct.variants, map_constructor_type(ct, _))
    })
    |> list.append(
      list.filter(module.externals, not_builtin)
      |> list.map(fn(ext) { map_external_type(ext.typ) }),
    )
    |> list.append(list.map(module.functions, map_func_type))
    |> list.append(list.map(need_wrapping, map_wrap_type(module, _)))
    |> list.append(builtin_types)
    |> list.unique

  use ctx <- result.try(
    list.try_fold(func_types, ctx, fn(ctx, ft) {
      use ct <- result.try(generate_function_type(ctx, ft.parameters, ft.return))
      use #(mb, id) <- result.map(
        wasm.add_type(ctx.mb, ct)
        |> to_snag("wasm.add_type(FunType)"),
      )
      Context(..ctx, mb:, fun_types: dict.insert(ctx.fun_types, ft, id))
    }),
  )

  // register closure environment types
  let assert Ok(nil_type_idx) = dict.get(ctx.val_types, NamedType("Nil"))
  use #(ctx, _) <- result.map(
    list.try_fold(module.functions, #(ctx, dict.new()), fn(ctx, fun) {
      let #(ctx, envs) = ctx
      use <- bool.guard(
        fun.environment == [],
        Ok(#(
          Context(
            ..ctx,
            val_types: dict.insert(ctx.val_types, Env(fun.name), nil_type_idx),
          ),
          envs,
        )),
      )
      let fields = list.map(fun.environment, fn(field) { map_type(field.typ) })
      case dict.get(envs, fields) {
        Ok(type_idx) ->
          #(
            Context(
              ..ctx,
              val_types: dict.insert(ctx.val_types, Env(fun.name), type_idx),
            ),
            envs,
          )
          |> Ok
        Error(_) -> {
          let ct =
            wasm.Struct(
              None,
              list.map(fields, fn(t) {
                wasm_value_type(ctx, t)
                |> wasm.ValueType(None, wasm.Immutable, _)
              }),
            )
          use #(mb, idx) <- result.map(
            wasm.add_type(ctx.mb, ct)
            |> to_snag("wasm.add_type(EnvType)"),
          )
          let val_types = dict.insert(ctx.val_types, Env(fun.name), idx)
          let envs = dict.insert(envs, fields, idx)
          #(Context(..ctx, mb:, val_types:), envs)
        }
      }
    }),
  )

  Context(..ctx, need_wrapping:)
}

fn map_wrap_type(module: mono.Context, fun_name: String) -> FunType {
  // TODO: I guess also constructors!
  let fun_type = case
    list.find(module.functions, fn(f) { f.name == fun_name }),
    list.find(module.externals, fn(f) { f.internal_name == fun_name })
  {
    Ok(func), _ -> map_func_type(func)
    _, Ok(ext) -> map_external_type(ext.typ)
    _, _ -> panic
  }
  FunType([AbstractEnv, ..fun_type.parameters], fun_type.return)
}

/// Find functions in need of wrapping as Closures
/// TODO: ideally previous steps do this, and we wouldn't need to
/// generate the non-wrapped version for anonymous functions
fn find_need_wrapping(module: mono.Context) -> List(String) {
  list.flat_map(module.functions, fn(func) {
    find_need_wrapping_expr(func.body)
  })
  |> list.unique
}

fn find_need_wrapping_expr(expr: mono.Exp) -> List(String) {
  let recur = fn(lst) { list.flat_map(lst, find_need_wrapping_expr) }
  case expr {
    mono.Literal(..) -> []
    mono.Local(..) -> []
    mono.ConstantGlobal(..) -> []
    mono.CallGlobal(args:, ..) -> recur(args)
    mono.BindClosure(id:, environment:, ..) ->
      case environment {
        [] -> [id, ..recur(environment)]
        [_, ..] -> recur(environment)
      }
    mono.CallClosure(fun:, args:, ..) -> recur([fun, ..args])
    mono.Op(args:, ..) -> recur(args)
    mono.Let(val:, exp:, ..) -> recur([val, exp])
    mono.If(cond:, then_exp:, else_exp:, ..) ->
      recur([cond, then_exp, else_exp])
    mono.Panic(value:, ..) -> find_need_wrapping_expr(value)
  }
}

fn register_type_group(ctx: Context, ts: List(ValType)) -> Result(Context, Snag) {
  // first register the type indices for each element of the group
  let ctx =
    list.fold(ts, ctx, fn(ctx, t) {
      case t {
        AbstractEnv -> panic
        _ ->
          Context(
            ..ctx,
            val_types: dict.insert(ctx.val_types, t, ctx.next_type_index),
            next_type_index: ctx.next_type_index + 1,
          )
      }
    })

  // then generate the actual types, which may now reference eachother
  let assert Ok(wasm_types) =
    list.try_map(ts, fn(t) {
      case t {
        AbstractEnv -> panic
        Closure -> panic
        Env(closure_name:) -> generate_closure_env_type(ctx, closure_name)
        NamedType(name: "String") -> generate_string_type()
        NamedType(name: "BitArray") -> generate_bit_array_type()
        NamedType(name:) -> generate_custom_type(ctx, name)
        NamedTypeVariant(type_name:, variant_name:) ->
          generate_variant_type(ctx, type_name, variant_name)
      }
    })

  use #(mb, _ids) <- result.map(
    wasm.add_sub_type_group(ctx.mb, wasm_types)
    |> to_snag("wasm.add_sub_type_group"),
  )
  Context(..ctx, mb:)
}

const byte_array_index = 0

const closure_type_index = 1

const closure_env_index = 0

const closure_fun_index = 1

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
  let assert Ok(idx) = dict.get(ctx.val_types, NamedType(def.name)) as def.name
  use variant <- result.map(
    list.find(def.variants, fn(v) { v.name == variant_name })
    |> result.replace_error(snag.new("Variant " <> variant_name <> " not found")),
  )
  generate_struct(ctx, variant_name, [idx], variant.fields)
}

fn generate_function_type(
  ctx: Context,
  params: List(ValType),
  return: ValType,
) -> Result(wasm.CompositeType, Snag) {
  wasm.Func(
    name: None,
    params: list.map(params, wasm_value_type(ctx, _)),
    result: [wasm_value_type(ctx, return)],
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

pub fn val_type_to_string(t: ValType) {
  case t {
    Closure -> "Closure"
    AbstractEnv -> "AbstractEnv"
    Env(closure_name:) -> "Env(" <> closure_name <> ")"
    NamedType(name:) -> name
    NamedTypeVariant(type_name:, variant_name:) ->
      type_name <> "/" <> variant_name
  }
}

fn wasm_value_type(ctx: Context, t: ValType) -> wasm.ValueType {
  case t {
    AbstractEnv -> wasm.Ref(wasm.NonNull(wasm.AbstractAny))
    NamedType(name: "Bool") | NamedTypeVariant(type_name: "Bool", ..) ->
      wasm.I32
    NamedType(name: "Int") -> wasm.I64
    NamedType(name: "Float") -> wasm.F64
    NamedType(name: "UtfCodepoint") -> wasm.I32
    _ -> {
      let assert Ok(id) = dict.get(ctx.val_types, t)
        as { "Unknown val_type " <> val_type_to_string(t) }
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
  let #(nodes, edges) = graph
  #(set.to_list(nodes), set.to_list(edges))
}

fn map_external_type(typ: mono.Type) -> FunType {
  case typ {
    mono.NamedType(..) ->
      panic as "Encountered NamedType where FunctionType was expected"
    mono.FunctionType(parameters:, return:) ->
      FunType(
        parameters: list.map(parameters, map_type),
        return: map_type(return),
      )
  }
}

fn map_constructor_type(
  custom_type: mono.CustomType,
  variant: mono.Variant,
) -> FunType {
  FunType(
    parameters: list.map(variant.fields, fn(field) { map_type(field.typ) }),
    return: NamedTypeVariant(
      type_name: custom_type.name,
      variant_name: variant.name,
    ),
  )
}

fn map_func_type(func: mono.Closure) -> FunType {
  let parameters = list.map(func.parameters, fn(field) { map_type(field.typ) })
  let parameters = case func.environment {
    [] -> parameters
    _ -> [AbstractEnv, ..parameters]
  }
  FunType(parameters: parameters, return: map_type(func.return))
}

fn map_type(typ: mono.Type) -> ValType {
  case typ {
    mono.NamedType(..) -> NamedType(mono.type_name(typ))
    mono.FunctionType(..) -> Closure
  }
}

// === Register imports ===

fn register_imports(ctx: Context) -> Result(Context, Snag) {
  let externals = list.filter(ctx.module.externals, not_builtin)
  use mb <- result.map(
    list.try_fold(externals, ctx.mb, fn(mb, external) {
      let assert Ok(f_type_idx) =
        dict.get(ctx.fun_types, map_external_type(external.typ))
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
    list.index_map(externals, fn(external, idx) {
      #(external.internal_name, idx)
    })
    |> dict.from_list
  Context(..ctx, mb:, functions:)
}

// === Register functions ===
fn register_functions(ctx: Context) -> Result(Context, Snag) {
  let functions =
    list.map(ctx.module.functions, fn(func) {
      let assert Ok(type_idx) = dict.get(ctx.fun_types, map_func_type(func))
      let param_names =
        list.map(func.parameters, fn(param) { param.name })
        |> fn(params) {
          case func.environment {
            [] -> params
            _ -> ["ENV", ..params]
          }
        }
      #(
        func.name,
        wasm.FunctionSignature(type_idx, Some(func.name), Some(param_names)),
      )
    })
  let wrappers =
    list.map(ctx.need_wrapping, fn(func_name) {
      let fun_type = map_wrap_type(ctx.module, func_name)
      let assert Ok(type_idx) = dict.get(ctx.fun_types, fun_type)
      #(
        "Wrap$" <> func_name,
        wasm.FunctionSignature(type_idx, Some("Wrap$" <> func_name), None),
      )
    })
  use #(mb, fbs) <- result.try(
    wasm.create_function_builders(
      ctx.mb,
      list.map(list.append(functions, wrappers), pair.second),
    )
    |> to_snag("wasm.create_function_builders"),
  )
  let floor = dict.size(ctx.functions)
  let functions =
    list.index_map(list.append(functions, wrappers), fn(func, idx) {
      #(func.0, floor + idx)
    })
    |> dict.from_list
    |> dict.merge(ctx.functions, _)
  let ctx = Context(..ctx, mb:, fbs:, functions:)
  Ok(ctx)
}

// === Build functions ===

fn build_wrappers(ctx: Context) -> Result(Context, Snag) {
  // A wasm.CodeBuilder has been pre-registered for each function
  list.zip(
    ctx.need_wrapping,
    list.drop(ctx.fbs, list.length(ctx.module.functions)),
  )
  |> list.try_fold(ctx, fn(ctx, build) {
    let #(func_name, fb) = build
    let assert Ok(func_idx) = dict.get(ctx.functions, func_name)
    let func_type = map_wrap_type(ctx.module, func_name)
    {
      let code =
        list.flatten([
          list.index_map(list.drop(func_type.parameters, 1), fn(_p, idx) {
            wasm.LocalGet(idx + 1)
          }),
          [wasm.Call(func_idx), wasm.End],
        ])
      use fb <- result.try(add_code(fb, code))
      use mb <- result.map(
        wasm.finalize_function(ctx.mb, fb) |> to_snag("wasm.finalize_function"),
      )
      Context(..ctx, mb:)
    }
    |> snag.context("building " <> func_name)
  })
}

fn build_functions(ctx: Context) -> Result(Context, Snag) {
  let module = ctx.module
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

      // add locals for the environment params
      use fb <- result.try(
        list.try_fold(func.environment, fb, fn(fb, param) {
          use #(fb, _) <- result.map(
            wasm.add_local(
              fb,
              wasm_value_type(ctx, map_type(param.typ)),
              Some(param.name),
            )
            |> to_snag("wasm.add_local"),
          )
          fb
        }),
      )

      use fb <- result.try(
        case func.environment {
          [] -> {
            Ok(fb)
          }
          [_, ..] -> {
            let assert Ok(env_type_idx) =
              dict.get(ctx.val_types, Env(func.name))
            let env_ref_type = wasm.NonNull(wasm.ConcreteType(env_type_idx))
            use #(fb, env_idx) <- result.try(
              wasm.add_local(fb, wasm.Ref(env_ref_type), None)
              |> to_snag("wasm.add_local"),
            )

            use fb <- result.try(
              add_code(fb, [
                wasm.LocalGet(0),
                wasm.RefCast(env_ref_type),
                wasm.LocalSet(env_idx),
              ])
              |> snag.context("casting ENV"),
            )

            let floor = list.length(params)
            list.index_map(func.environment, fn(_param, idx) { idx })
            |> list.try_fold(fb, fn(fb, idx) {
              add_code(fb, [
                wasm.LocalGet(env_idx),
                wasm.StructGet(env_type_idx, idx),
                wasm.LocalSet(floor + idx),
              ])
              |> snag.context("setting ENV[" <> int.to_string(idx) <> "]")
            })
          }
        }
        |> snag.context("unwrapping ENV"),
      )

      // Generate the body of the function.
      use fb <- result.try(codegen_expr(fb, func.body, local_env, ctx))
      // Close and finalize the function.
      use fb <- result.try(add_code(fb, [wasm.End]))
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
          core.Bool(value:) -> {
            use int_val <- result.try(case value {
              "False" -> Ok(0)
              "True" -> Ok(1)
              _ -> snag.error("Invalid bool literal: " <> value)
            })
            use int32 <- result.try(
              wasm.int32_unsigned(int_val) |> to_snag("wasm.int32_unsigned"),
            )
            add_code(fb, [wasm.I32Const(int32)])
          }
          core.Int(value:) -> {
            use int_val <- result.try(
              int.parse(value)
              |> snag.replace_error("Invalid int literal: " <> value),
            )
            use int64 <- result.try(
              wasm.int64_signed(int_val) |> to_snag("wasm.int64_signed"),
            )
            add_code(fb, [wasm.I64Const(int64)])
          }
          core.Float(value: _) -> todo
          core.String(value:) -> make_string_literal(ctx, fb, value)
          core.BitArray(size: _) -> todo
        }
      }
      |> snag.context("Literal(String)")
    mono.Local(typ: _, var:) ->
      {
        let assert Ok(idx) = dict.get(local_env, var)
        add_code(fb, [wasm.LocalGet(idx)])
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
        case find_builtin_instr(id) {
          Ok(instr) -> add_code(fb, [instr])
          _ -> {
            let assert Ok(f_idx) = dict.get(ctx.functions, id)
              as { "Missing global function " <> id }
            add_code(fb, [wasm.Call(f_idx)])
          }
        }
      }
      |> snag.context("CallGlobal " <> id)
    mono.BindClosure(typ: _, id:, environment:) ->
      {
        let assert Ok(fun_idx) =
          dict.get(
            ctx.functions,
            case environment {
              [] -> "Wrap$"
              [_, ..] -> ""
            }
              <> id,
          )
        let assert Ok(env_type_idx) = dict.get(ctx.val_types, Env(id))
        use fb <- result.try(
          list.try_fold(environment, fb, fn(fb, arg) {
            codegen_expr(fb, arg, local_env, ctx)
          }),
        )
        add_code(fb, [
          wasm.StructNew(env_type_idx),
          wasm.RefFunc(fun_idx),
          wasm.StructNew(closure_type_index),
        ])
      }
      |> snag.context("BindClosure " <> id)
    mono.CallClosure(typ: _, fun:, args:) ->
      {
        let fun_type = map_external_type(fun.typ)
        let fun_type =
          FunType([AbstractEnv, ..fun_type.parameters], fun_type.return)
        let assert Ok(fun_closure_type_idx) = dict.get(ctx.fun_types, fun_type)
        use #(fb, closure_idx) <- result.try(
          wasm.add_local(
            fb,
            wasm.Ref(wasm.NonNull(wasm.ConcreteType(closure_type_index))),
            None,
          )
          |> to_snag("wasm.add_local"),
        )
        use fb <- result.try(codegen_expr(fb, fun, local_env, ctx))
        use fb <- result.try(
          add_code(fb, [
            wasm.LocalSet(closure_idx),
            wasm.LocalGet(closure_idx),
            wasm.StructGet(closure_type_index, closure_env_index),
          ]),
        )
        // now the closure struct is a local, and the environment is on the stack
        // now put each argument on the stack
        use fb <- result.try(
          list.try_fold(args, fb, fn(fb, arg) {
            codegen_expr(fb, arg, local_env, ctx)
          }),
        )
        // finally, put the function on the stack and call it
        add_code(fb, [
          wasm.LocalGet(closure_idx),
          wasm.StructGet(closure_type_index, closure_fun_index),
          // wasm.LocalGet(closure_idx),
          // wasm.StructGet(closure_type_index, closure_env_index),
          // wasm.RefIsNull,
          // first branch: if the env is null, call global
          // wasm.If(wasm.BlockValue(ret_type)),
          // wasm.RefCast(wasm.NonNull(wasm.ConcreteType(fun_global_type_idx))),
          // wasm.CallRef(fun_global_type_idx),
          // wasm.Else,
          wasm.RefCast(wasm.NonNull(wasm.ConcreteType(fun_closure_type_idx))),
          wasm.CallRef(fun_closure_type_idx),
          // wasm.End,
        ])
      }
      |> snag.context("CallClosure")
    mono.Op(typ: _, op: _, args: _) -> todo
    mono.Let(typ: _, var: "_" <> _, val:, exp:) -> {
      // we don't create and set locals for anonymous variables
      use fb <- result.try(codegen_expr(fb, val, local_env, ctx))
      use fb <- result.try(add_code(fb, [wasm.Drop]))
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
        use fb <- result.try(add_code(fb, [wasm.LocalSet(local_idx)]))
        let local_env = dict.insert(local_env, var, local_idx)
        // generate the body
        codegen_expr(fb, exp, local_env, ctx)
      }
      |> snag.context("Let " <> var)
    mono.If(typ:, cond:, then_exp:, else_exp:) ->
      {
        let block_type = wasm_value_type(ctx, map_type(typ))
        use fb <- result.try(codegen_expr(fb, cond, local_env, ctx))
        use fb <- result.try(
          add_code(fb, [
            wasm.If(wasm.BlockValue(block_type)),
          ]),
        )
        use fb <- result.try(codegen_expr(fb, then_exp, local_env, ctx))
        use fb <- result.try(add_code(fb, [wasm.Else]))
        use fb <- result.try(codegen_expr(fb, else_exp, local_env, ctx))
        add_code(fb, [wasm.End])
      }
      |> snag.context("If")
    mono.Panic(typ: _, value:) -> {
      let assert Ok(echo_idx) = dict.get(ctx.functions, "print_string")
      use fb <- result.try(codegen_expr(fb, value, local_env, ctx))
      add_code(fb, [wasm.Call(echo_idx), wasm.Unreachable])
    }
  }
}

fn make_string_literal(
  ctx: Context,
  fb: wasm.CodeBuilder,
  value: String,
) -> Result(wasm.CodeBuilder, Snag) {
  // TODO: Ideally store string literals in a table
  let assert Ok(type_index) = dict.get(ctx.val_types, NamedType("String"))
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
  add_code(fb, [
    wasm.I32Const(zero),
    // initialize the bytes
    wasm.I32Const(size),
    wasm.ArrayNew(byte_array_index),
    wasm.LocalSet(bytes_idx),
    wasm.LocalGet(bytes_idx),
    wasm.I32Const(zero),
    wasm.I32Const(offset),
    wasm.I32Const(size),
    wasm.ArrayInitData(byte_array_index, 0),
    // create the String struct
    wasm.I32Const(size),
    wasm.LocalGet(bytes_idx),
    wasm.StructNew(type_index),
  ])
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
      FunType([NamedType("String")], NamedType("String")),
      build_inspect_string,
    ),
    #(
      "print_string",
      FunType([NamedType("String")], NamedType("Nil")),
      build_print_string,
    ),
    #(
      "inspect_Bool",
      FunType([NamedType("Bool")], NamedType("String")),
      build_inspect_bool,
    ),
  ]
  use ctx <- result.try(
    list.try_fold(builtins, ctx, fn(ctx, builtin) {
      let #(name, func_type, build) = builtin
      {
        let assert Ok(type_idx) = dict.get(ctx.fun_types, func_type)
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

  let assert Ok(nil_type_idx) = dict.get(ctx.val_types, NamedType("Nil"))
  use #(mb, cb) <- result.try(
    wasm.create_global_builder(
      ctx.mb,
      Some("Nil"),
      wasm.Immutable,
      wasm.Ref(wasm.NonNull(wasm.ConcreteType(nil_type_idx))),
    )
    |> to_snag("wasm.create_global_builder"),
  )
  use cb <- result.try(add_code(cb, [wasm.StructNew(nil_type_idx), wasm.End]))
  use mb <- result.map(
    wasm.finalize_global(mb, cb) |> to_snag("wasm.finalize_global"),
  )

  Context(..ctx, mb:)
}

const nil_global_idx = 0

fn build_print_string(
  fb: wasm.CodeBuilder,
  ctx: Context,
) -> Result(wasm.CodeBuilder, Snag) {
  // super ugly: copy string to a fixed location in linear memory and call an import
  let assert Ok(string_type_idx) = dict.get(ctx.val_types, NamedType("String"))
  let assert Ok(nil_type_idx) =
    dict.get(ctx.val_types, NamedTypeVariant("Nil", "Nil"))
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
  add_code(fb, [
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
  ])
}

fn build_inspect_string(
  fb: wasm.CodeBuilder,
  _ctx: Context,
) -> Result(wasm.CodeBuilder, Snag) {
  add_code(fb, [
    wasm.LocalGet(0),
    wasm.End,
  ])
}

fn build_inspect_bool(
  fb: wasm.CodeBuilder,
  ctx: Context,
) -> Result(wasm.CodeBuilder, Snag) {
  let string_type = wasm_value_type(ctx, NamedType("String"))
  use fb <- result.try(
    add_code(fb, [wasm.LocalGet(0), wasm.If(wasm.BlockValue(string_type))]),
  )
  use fb <- result.try(make_string_literal(ctx, fb, "True"))
  use fb <- result.try(add_code(fb, [wasm.Else]))
  use fb <- result.try(make_string_literal(ctx, fb, "False"))
  add_code(fb, [wasm.End, wasm.End])
}
