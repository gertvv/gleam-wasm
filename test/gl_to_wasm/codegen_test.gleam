import birdie
import cymbal
import gig/core
import gig/core_closure
import gig/mono
import gl_to_wasm/codegen
import gl_to_wasm/graph
import gl_wasm/wasm
import glance
import glance_typed
import gleam/dict.{type Dict}
import gleam/list
import gleam/option
import gleam/order
import gleam/result
import gleam/string
import simplifile

/// Parse, type check, lower, and monomorphize the given modules.
/// modules: to be given in dependency order
fn mono_modules(modules: List(#(String, String)), main_name) {
  list.fold(modules, #(dict.new(), dict.new(), []), fn(acc, val) {
    let #(typed_ifaces, lowered_ifaces, lowered_modules) = acc
    let #(name, source) = val
    let assert Ok(parsed) = glance.module(source)
    let assert Ok(checked) =
      glance_typed.infer_module(typed_ifaces, parsed, name)
    let lowered = core.lower_module(lowered_ifaces, checked)
    let lowered_module = core_closure.lower_module(lowered)
    let interface = glance_typed.interface(checked)
    #(
      dict.insert(typed_ifaces, name, interface),
      dict.insert(lowered_ifaces, name, core.lower_module_interface(interface)),
      [lowered_module, ..lowered_modules],
    )
  }).2
  |> mono.run(main_name)
}

fn mono_module_with_prelude(
  module_name: String,
  source: String,
  main_name: String,
) {
  let assert Ok(prelude) =
    simplifile.read("./build/packages/gig/patch/gleam.gleam")
  mono_modules([#("gleam", prelude), #(module_name, source)], main_name)
}

fn file_output_stream(fname) {
  let output_stream =
    wasm.OutputStream(
      stream: fname,
      write_bytes: fn(fname, bytes) {
        simplifile.append_bits(fname, bytes)
        |> result.replace(fname)
      },
      close: fn(fname) { Ok(fname) },
    )
  let _ = simplifile.write_bits(fname, <<>>)
  output_stream
}

// Exercises function and external types depending on built-in types
pub fn codegen_hello_world_test() {
  let #(module, main) =
    mono_module_with_prelude(
      "hello",
      "pub fn main() { echo \"Hello, World!\"}",
      "hello_main",
    )
  mono_to_yaml(#(module, main))
  |> cymbal.encode
  |> birdie.snap("hello_world mono")

  codegen.construct_type_graph(module)
  |> graph_to_yaml(codegen.val_type_to_string)
  |> cymbal.encode
  |> birdie.snap("hello_world type graph")

  let assert Ok(wasm) = codegen.codegen(module, "hello", "hello_main")
  let funcs = list_functions(wasm)

  let assert Ok(print) = dict.get(funcs, "print_string")
  wasm.function_to_string(print)
  |> birdie.snap("print_string code")

  let assert Ok(echo_) = dict.get(funcs, "echo__String")
  wasm.function_to_string(echo_)
  |> birdie.snap("hello_world echo__String code")

  let assert Ok(main) = dict.get(funcs, "hello_main")
  wasm.function_to_string(main)
  |> birdie.snap("hello_world main code")

  wasm.emit_module(wasm, file_output_stream("hello.wasm"))
}

fn list_functions(
  wasm: wasm.ModuleBuilder,
) -> Dict(String, wasm.FunctionDefinition) {
  do_list_functions(dict.new(), wasm, 0)
}

fn do_list_functions(
  acc: Dict(String, wasm.FunctionDefinition),
  wasm: wasm.ModuleBuilder,
  idx: Int,
) -> Dict(String, wasm.FunctionDefinition) {
  case wasm.get_function_by_index(wasm, idx) {
    Ok(def) -> {
      case def.name {
        option.Some(name) ->
          do_list_functions(dict.insert(acc, name, def), wasm, idx + 1)
        option.None -> do_list_functions(acc, wasm, idx + 1)
      }
    }
    Error(_) -> acc
  }
}

pub fn type_graph_type_recursion_test() {
  let #(module, main) =
    mono_module_with_prelude(
      "rec",
      "pub type Tree(a) { Branch(Tree(a), Tree(a)) Leaf(a) } pub fn main() { Branch(Branch(Leaf(\"a\"), Leaf(\"b\")), Leaf(\"c\")) }",
      "rec_main",
    )

  mono_to_yaml(#(module, main))
  |> cymbal.encode
  |> birdie.snap("rectype mono")

  codegen.construct_type_graph(module)
  |> graph_to_yaml(codegen.val_type_to_string)
  |> cymbal.encode
  |> birdie.snap("rectype type graph")
}

pub fn type_graph_closure_test() {
  let #(module, main) =
    mono_module_with_prelude(
      "rec",
      "fn sayer(str) { fn() { str } } pub fn main() { let f = sayer(\"Hello\") echo f() }",
      "rec_main",
    )

  mono_to_yaml(#(module, main))
  |> cymbal.encode
  |> birdie.snap("closure mono")

  codegen.construct_type_graph(module)
  |> graph_to_yaml(codegen.val_type_to_string)
  |> cymbal.encode
  |> birdie.snap("closure type graph")
  let assert Ok(wasm) = codegen.codegen(module, "rec", "rec_main")
  wasm.emit_module(wasm, file_output_stream("sayer.wasm"))
}

fn graph_to_yaml(
  graph: graph.Graph(a),
  node_to_string: fn(a) -> String,
) -> cymbal.Yaml {
  let #(nodes, edges) = graph
  let nodes = list.map(nodes, node_to_string)
  let edges =
    list.map(edges, fn(edge) {
      let #(a, b) = edge
      #(node_to_string(a), node_to_string(b))
    })
  let nodes = list.sort(nodes, string.compare)
  let edges =
    list.sort(edges, fn(a, b) {
      string.compare(a.0, b.0) |> order.break_tie(string.compare(a.1, b.1))
    })
  cymbal.block([
    #("nodes", cymbal.array(list.map(nodes, cymbal.string))),
    #(
      "edges",
      cymbal.array(
        list.map(edges, fn(edge) {
          let #(a, b) = edge
          cymbal.block([
            #("a", cymbal.string(a)),
            #("b", cymbal.string(b)),
          ])
        }),
      ),
    ),
  ])
}

fn mono_to_yaml(mono: #(mono.Context, String)) -> cymbal.Yaml {
  let #(module, main) = mono
  cymbal.block([
    #("main", cymbal.string(main)),
    #("types", cymbal.array(list.map(module.types, mono_custom_type_to_yaml))),
    #(
      "functions",
      cymbal.array(list.map(module.functions, mono_closure_to_yaml)),
    ),
    #(
      "externals",
      cymbal.array(list.map(module.externals, mono_external_to_yaml)),
    ),
  ])
}

fn mono_custom_type_to_yaml(custom_type: mono.CustomType) -> cymbal.Yaml {
  let mono.CustomType(name:, untyped_name:, display_name:, variants:) =
    custom_type
  cymbal.block([
    #("name", cymbal.string(name)),
    #("untyped_name", cymbal.string(untyped_name)),
    #("display_name", cymbal.string(display_name)),
    #("variants", cymbal.array(list.map(variants, mono_variant_to_yaml))),
  ])
}

fn mono_variant_to_yaml(variant: mono.Variant) -> cymbal.Yaml {
  let mono.Variant(name:, untyped_name:, display_name:, fields:) = variant
  cymbal.block([
    #("name", cymbal.string(name)),
    #("untyped_name", cymbal.string(untyped_name)),
    #("display_name", cymbal.string(display_name)),
    #("fields", cymbal.array(list.map(fields, mono_field_to_yaml))),
  ])
}

fn mono_closure_to_yaml(closure: mono.Closure) -> cymbal.Yaml {
  let mono.Closure(name:, environment:, parameters:, return:, body:) = closure
  cymbal.block([
    #("name", cymbal.string(name)),
    #("environment", cymbal.array(list.map(environment, mono_field_to_yaml))),
    #("parameters", cymbal.array(list.map(parameters, mono_field_to_yaml))),
    #("return", mono_type_to_yaml(return)),
    #("body", mono_exp_to_yaml(body)),
  ])
}

fn mono_exp_to_yaml(exp: mono.Exp) -> cymbal.Yaml {
  case exp {
    mono.Literal(typ:, val:) ->
      typed_node("Literal", typ, [#("value", mono_literal_to_yaml(val))])
    mono.Local(typ:, var:) ->
      typed_node("Local", typ, [#("var", cymbal.string(var))])
    mono.ConstantGlobal(typ:, id:) ->
      typed_node("ConstantGlobal", typ, [#("id", cymbal.string(id))])
    mono.CallGlobal(typ:, id:, args:) ->
      typed_node("CallGlobal", typ, [
        #("id", cymbal.string(id)),
        #("args", cymbal.array(list.map(args, mono_exp_to_yaml))),
      ])
    mono.BindClosure(typ:, id:, environment:) ->
      typed_node("BindClosure", typ, [
        #("id", cymbal.string(id)),
        #("environment", cymbal.array(list.map(environment, mono_exp_to_yaml))),
      ])
    mono.CallClosure(typ:, fun:, args:) ->
      typed_node("CallClosure", typ, [
        #("fun", mono_exp_to_yaml(fun)),
        #("args", cymbal.array(list.map(args, mono_exp_to_yaml))),
      ])
    mono.Op(typ:, op:, args:) ->
      typed_node("Op", typ, [
        #("op", mono_op_to_yaml(op)),
        #("args", cymbal.array(list.map(args, mono_exp_to_yaml))),
      ])
    mono.Let(typ:, var:, val:, exp:) ->
      typed_node("Let", typ, [
        #("var", cymbal.string(var)),
        #("val", mono_exp_to_yaml(val)),
        #("exp", mono_exp_to_yaml(exp)),
      ])
    mono.If(typ:, cond:, then_exp:, else_exp:) ->
      typed_node("If", typ, [
        #("cond", mono_exp_to_yaml(cond)),
        #("then_exp", mono_exp_to_yaml(then_exp)),
        #("else_exp", mono_exp_to_yaml(else_exp)),
      ])
    mono.Panic(typ:, value:) ->
      typed_node("Panic", typ, [#("value", mono_exp_to_yaml(value))])
  }
}

fn mono_op_to_yaml(op: mono.Op) -> cymbal.Yaml {
  case op {
    mono.FieldAccess(kind:, variant:, field:) ->
      cymbal.block([
        #("kind", cymbal.string(mono_field_access_kind_to_string(kind))),
        #("variant", cymbal.string(variant)),
        #("field", cymbal.string(field)),
      ])
    mono.VariantCheck(variant:) ->
      cymbal.block([
        #("kind", cymbal.string("VariantCheck")),
        #("variant", cymbal.string(variant)),
      ])
  }
}

fn mono_field_access_kind_to_string(kind: mono.FieldAccessKind) -> String {
  case kind {
    mono.StructAccess -> "StructAccess"
    mono.StructPointerAccess -> "StructPointerAccess"
    mono.TaggedUnionAccess -> "TaggedUnionAccess"
  }
}

fn mono_literal_to_yaml(val: core.LiteralKind) -> cymbal.Yaml {
  let render = fn(kind, value) { cymbal.string(kind <> "(" <> value <> ")") }
  case val {
    core.NilVal -> cymbal.string("Nil")
    core.Bool(value:) -> render("Bool", value)
    core.Int(value:) -> render("Int", value)
    core.Float(value:) -> render("Float", value)
    core.String(value:) -> render("String", value)
    core.BitArray(size:) -> render("BitArray", size)
  }
}

fn typed_node(
  kind: String,
  typ: mono.Type,
  properties: List(#(String, cymbal.Yaml)),
) -> cymbal.Yaml {
  cymbal.block([
    #("kind", cymbal.string(kind)),
    #("type", mono_type_to_yaml(typ)),
    ..properties
  ])
}

fn mono_external_to_yaml(external: mono.External) -> cymbal.Yaml {
  let mono.External(
    typ:,
    module:,
    internal_name:,
    external_name:,
    parameters:,
    builtin:,
  ) = external
  typed_node("External", typ, [
    #("module", cymbal.string(module)),
    #("internal_name", cymbal.string(internal_name)),
    #("external_name", cymbal.string(external_name)),
    #("parameters", cymbal.array(list.map(parameters, mono_field_to_yaml))),
    #("builtin", cymbal.bool(builtin)),
  ])
}

fn mono_field_to_yaml(field: mono.Field) -> cymbal.Yaml {
  let mono.Field(name:, typ:) = field
  cymbal.block([
    #("name", cymbal.string(name)),
    #("type", mono_type_to_yaml(typ)),
  ])
}

fn mono_type_to_yaml(typ: mono.Type) -> cymbal.Yaml {
  case typ {
    mono.NamedType(id:, parameters:) ->
      cymbal.block([
        #("kind", cymbal.string("NamedType")),
        #("id", cymbal.string(id)),
        #("parameters", cymbal.array(list.map(parameters, mono_type_to_yaml))),
      ])
    mono.FunctionType(parameters:, return:) ->
      cymbal.block([
        #("kind", cymbal.string("FunctionType")),
        #("parameters", cymbal.array(list.map(parameters, mono_type_to_yaml))),
        #("return", mono_type_to_yaml(return)),
      ])
  }
}
