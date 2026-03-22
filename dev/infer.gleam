import gig/core
import gig/typed_ast
import gl_to_wasm/closure
import gl_to_wasm/codegen
import gl_to_wasm/graph
import gl_to_wasm/io_context
import gl_to_wasm/module
import gl_to_wasm/project
import gl_wasm/wasm
import glance
import gleam/dict
import gleam/io
import gleam/list
import gleam/result
import pprint
import simplifile

// TODO: I'd prefer "NonEmpty" to "Cons" but gig/core.lower_context expects "Cons"
const prelude = "
  pub type Int
  pub type Float
  pub type Bool {
    False
    True
  }
  pub type String
  pub type BitArray
  pub type UtfCodepoint
  pub type List(e) {
    Empty
    Cons(e, List(e))
  }
  pub type Result(a, b) {
    Ok(a)
    Error(b)
  }
  pub type Nil {
    Nil
  }

  @external(erlang, \"gleam\", \"add_int\")
  pub fn add_int(a: Int, b: Int) -> Int
"

const target = "erlang"

pub type Context {
  Context(
    interfaces: dict.Dict(String, typed_ast.ModuleInterface),
    lowered_interfaces: dict.Dict(String, core.ModuleInterface),
    implementations: dict.Dict(String, core.Module),
  )
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

pub fn main() {
  let io_ctx =
    io_context.IOContext(
      list: simplifile.read_directory,
      list_recursive: simplifile.get_files,
      read: simplifile.read,
    )

  let assert Ok(project) = project.scan_project("../gl_example", target, io_ctx)

  let assert Ok(pkgs) =
    project.dependency_graph(project) |> graph.topological_sort

  let assert Ok(prelude_parsed) = glance.module(prelude)
  let assert Ok(prelude_checked) =
    typed_ast.infer_module(dict.new(), prelude_parsed, "gleam")
  let prelude_impl = core.lower_module(dict.new(), prelude_checked)

  let context =
    Context(dict.new(), dict.new(), dict.new())
    |> register_interface(prelude_checked)
    |> register_implementation("gleam", prelude_impl)

  let context =
    list.fold(pkgs, context, fn(context, pkg) {
      io.println("=== type checking package: " <> pkg <> " ===")
      let assert Ok(pkg) = module.parse_package(project, pkg, io_ctx)
      let g = module.import_graph(pkg)
      let assert Ok(sorted) = graph.topological_sort(g)
      list.fold(sorted, context, fn(context, module_id) {
        io.println("checking module: " <> module_id.module_path)
        let assert Ok(module.Module(ast:, ..)) =
          list.find(pkg, fn(module) { module.module_id == module_id })
        let ast = filter_target(ast, target)
        let assert Ok(checked) =
          typed_ast.infer_module(context.interfaces, ast, module_id.module_path)
        let context = register_interface(context, checked)
        // TODO: in fact need to lower interface before lowering implementation
        let impl = core.lower_module(context.lowered_interfaces, checked)
        register_implementation(context, checked.name, impl)
      })
    })

  let assert Ok(module) = dict.get(context.implementations, "gl_example")

  let closure = closure.lower_module(module)

  let assert Ok(mb) =
    codegen.codegen_module(context.lowered_interfaces, "example", closure)

  pprint.debug(mb)

  let output_stream = file_output_stream("gl_example.wasm")
  let assert Ok(_) = wasm.emit_module(mb, output_stream)

  Nil
}

fn register_interface(c: Context, m: typed_ast.Module) {
  Context(
    ..c,
    interfaces: dict.insert(c.interfaces, m.name, typed_ast.interface(m)),
  )
}

fn register_implementation(c: Context, name: String, i: core.Module) -> Context {
  Context(
    ..c,
    lowered_interfaces: dict.insert(
      c.lowered_interfaces,
      name,
      core.interface(i),
    ),
    implementations: dict.insert(c.implementations, name, i),
  )
}

fn filter_target(ast: glance.Module, target: String) -> glance.Module {
  let on_target = fn(definition: glance.Definition(a)) {
    let targets = get_targets(definition)
    targets == [] || list.contains(targets, target)
  }
  let custom_types = list.filter(ast.custom_types, on_target)
  let type_aliases = list.filter(ast.type_aliases, on_target)
  let constants = list.filter(ast.constants, on_target)
  let functions =
    list.filter_map(ast.functions, fn(function) {
      let targets = get_targets(function)
      case targets {
        [] -> Ok(function)
        _ -> {
          case list.contains(targets, target) {
            True ->
              Ok(
                glance.Definition(
                  ..function,
                  definition: glance.Function(..function.definition, body: []),
                ),
              )
            False -> Error(Nil)
          }
        }
      }
    })
  glance.Module(..ast, custom_types:, type_aliases:, constants:, functions:)
}

fn get_targets(def: glance.Definition(a)) -> List(String) {
  list.filter_map(def.attributes, fn(attr) {
    case attr {
      glance.Attribute(
        name: "target",
        arguments: [glance.Variable(_, target), ..],
      ) -> Ok(target)
      _ -> Error(Nil)
    }
  })
}
