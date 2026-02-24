import gig/core
import gig/typed_ast
import gl_to_wasm/closure
import gl_to_wasm/graph
import gl_to_wasm/io_context
import gl_to_wasm/module
import gl_to_wasm/project
import glance
import gleam/dict
import gleam/io
import gleam/list
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
"

const target = "erlang"

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

  let context = typed_ast.new_context()
  let assert Ok(prelude_parsed) = glance.module(prelude)
  let context =
    typed_ast.infer_module(context, prelude_parsed, "gleam", prelude)

  let context =
    list.fold(pkgs, context, fn(context, pkg) {
      io.println("=== type checking package: " <> pkg <> " ===")
      let assert Ok(pkg) = module.parse_package(project, pkg, io_ctx)
      let g = module.import_graph(pkg)
      let assert Ok(sorted) = graph.topological_sort(g)
      list.fold(sorted, context, fn(context, module_id) {
        let assert Ok(module.Module(ast:, source:, ..)) =
          list.find(pkg, fn(module) { module.module_id == module_id })
        let ast = filter_target(ast, target)
        typed_ast.infer_module(context, ast, module_id.module_path, source)
      })
    })

  let assert Ok(gleam) = dict.get(context.modules, "gleam")
  let assert Ok(module) = dict.get(context.modules, "example") |> pprint.debug
  let core =
    core.lower_module(
      typed_ast.Context(
        ..context,
        modules: dict.from_list([#("example", module), #("gleam", gleam)]),
      ),
      module,
    )
    |> pprint.debug

  let closure =
    closure.lower_module(core)
    |> pprint.debug

  Nil
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
