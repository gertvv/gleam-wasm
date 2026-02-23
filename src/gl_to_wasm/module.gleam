import gl_to_wasm/graph
import gl_to_wasm/io_context.{type IOContext}
import gl_to_wasm/project.{type ModuleId, type Project}
import glance
import gleam/dict
import gleam/list
import gleam/result

pub type Module {
  Module(
    module_id: ModuleId,
    imports: List(ModuleId),
    ast: glance.Module,
    source: String,
  )
}

pub type Error(e) {
  PackageNotFound(name: String)
  FileError(path: String, error: e)
  ParseError(path: String, error: glance.Error)
  ReferenceError(module_id: ModuleId, name: String)
}

pub fn parse_package(
  project: Project,
  package_name: String,
  io: IOContext(e),
) -> Result(List(Module), Error(e)) {
  use package <- result.try(
    dict.get(project.packages, package_name)
    |> result.replace_error(PackageNotFound(package_name)),
  )
  let modules = case package {
    project.GleamPackage(modules:, ..) -> modules
    project.OtherPackage(..) -> []
  }
  list.try_map(modules, fn(module_name) {
    let module_id = project.ModuleId(package_name, module_name)
    use #(ast, source) <- result.try(parse_module(project, module_id, io))
    use imports <- result.map(
      list.try_map(ast.imports, fn(import_) {
        let import_name = import_.definition.module
        resolve_module(project, package, module_id, import_name)
      }),
    )
    Module(module_id:, imports:, ast:, source:)
  })
}

pub fn parse_module(
  project: Project,
  module_id: ModuleId,
  io: IOContext(e),
) -> Result(#(glance.Module, String), Error(e)) {
  let path = project.module_source_path(project, module_id)
  use source <- result.try(
    io.read(path)
    |> result.map_error(FileError(path, _)),
  )
  glance.module(source)
  |> result.map(fn(ast) { #(ast, source) })
  |> result.map_error(ParseError(path, _))
}

fn resolve_module(
  project: Project,
  importing_package: project.Package,
  importing_module: ModuleId,
  imported_module: String,
) -> Result(ModuleId, Error(e)) {
  use _ <- result.try_recover(project.module_id(
    project,
    importing_package.name,
    imported_module,
  ))
  let deps = case importing_package {
    project.GleamPackage(dependencies:, ..) -> dependencies
    project.OtherPackage(..) -> []
  }
  list.find_map(deps, project.module_id(project, _, imported_module))
  |> result.replace_error(ReferenceError(importing_module, imported_module))
}

pub fn import_graph(modules: List(Module)) -> graph.Graph(ModuleId) {
  #(
    list.map(modules, fn(module) { module.module_id }),
    list.flat_map(modules, fn(module) {
      list.map(module.imports, fn(import_) { #(module.module_id, import_) })
    }),
  )
}
