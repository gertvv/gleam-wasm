import analysis
import compiler
import filepath
import gl_wasm/wasm
import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import graph
import pprint
import prelude
import project
import simplifile

pub fn main() {
  impl() |> pprint.debug
}

pub fn impl() {
  // TODO: maybe topologically sort the packages, and limit import graph to package boundary?
  use project <- result.try(project.scan_project("../gl_example", "wasm"))
  use _ <- result.try(output_module(
    project,
    project.ModuleId("gleam", "gleam"),
    prelude.build_prelude(),
  ))
  use modules <- result.try(
    analysis.analyze_project_imports(project)
    |> result.try(fn(graph) {
      let #(nodes, edges) = graph
      graph.topological_sort(nodes, edges)
      |> result.replace_error(compiler.CircularDependencyError)
    }),
  )
  list.try_fold(modules, dict.new(), fn(modules, module_id) {
    use module <- result.try(analysis.analyze_module(
      project,
      module_id,
      modules,
    ))
    use _ <- result.map(compile_module(project, module))
    let module = analysis.module_from_internals(module)
    dict.insert(modules, module_id, module)
  })
}

pub fn compile_module(
  project: project.Project,
  module: analysis.ModuleInternals,
) {
  let mb = wasm.create_module_builder(Some(module.location.module_path))

  use mb <- result.try(
    prelude.add_prelude_types(mb)
    |> result.replace_error(compiler.AnotherTypeError(
      "Failed to add prelude types",
    )),
  )

  // TODO: generate types

  // TODO: generate imports

  // TODO: generate functions
  list.each(module.implemented_functions, compile_functions(module, _))

  // TODO: generate exports

  output_module(project, module.location, mb)
}

pub fn output_module(
  project: project.Project,
  module_id: project.ModuleId,
  mb: wasm.ModuleBuilder,
) -> Result(String, compiler.CompilerError) {
  let wasm_file_name =
    filepath.join(
      project.package_build_path(project, module_id.package_name),
      module_id.module_path,
    )
    <> ".wasm"
  let wasm_dir_name = filepath.directory_name(wasm_file_name)
  use _ <- result.try(
    simplifile.create_directory_all(wasm_dir_name)
    |> result.map_error(compiler.FileError(wasm_dir_name, _)),
  )

  // TODO: properly return errors
  wasm.emit_module(mb, file_output_stream(wasm_file_name))
  |> pprint.debug
  |> result.replace_error(compiler.AnotherTypeError("Hmm"))
}

pub fn compile_functions(
  _internals: analysis.ModuleInternals,
  functions: List(analysis.Function),
) {
  pprint.debug(list.map(functions, fn(func) { func.signature.name }))
  list.flat_map(functions, analysis.referenced_types)
  |> list.unique
  |> pprint.debug
  Ok(Nil)
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
