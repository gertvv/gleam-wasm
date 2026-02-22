import gl_to_wasm/graph
import gl_to_wasm/io_context
import gl_to_wasm/module
import gl_to_wasm/project
import gleam/io
import gleam/list
import pprint
import simplifile

pub fn main() {
  let io_ctx =
    io_context.IOContext(
      list: simplifile.read_directory,
      list_recursive: simplifile.get_files,
      read: simplifile.read,
    )

  io.println("=== project structure ===")
  let assert Ok(project) =
    project.scan_project("../gl_example", "erlang", io_ctx)
    |> pprint.debug

  let assert Ok(pkgs) =
    project.dependency_graph(project) |> graph.topological_sort

  io.println("")
  list.each(pkgs, fn(pkg) {
    io.println("=== import graph: " <> pkg <> " ===")
    let assert Ok(pkg) = module.parse_package(project, pkg, io_ctx)
    let g = module.import_graph(pkg)
    let assert Ok(sorted) = graph.topological_sort(g)
    pprint.debug(sorted)
  })
}
