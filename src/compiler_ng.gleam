import analysis
import compiler
import gleam/dict
import gleam/list
import gleam/result
import graph
import pprint
import project

pub fn compile_functions(
  _internals: analysis.ModuleInternals,
  functions: List(analysis.Function),
) {
  pprint.debug(functions)
  Ok(Nil)
}

pub fn impl() {
  // TODO: maybe topologically sort the packages, and limit import graph to package boundary?
  use project <- result.try(project.scan_project("../gl_example", "javascript"))
  use modules <- result.try(
    analysis.analyze_project_imports(project)
    |> result.try(fn(graph) {
      let #(nodes, edges) = graph
      graph.topological_sort(nodes, edges)
      |> result.replace_error(compiler.CircularDependencyError)
    }),
  )
  list.try_fold(modules, dict.new(), fn(modules, module_id) {
    use module <- result.map(analysis.analyze_module(
      project,
      module_id,
      modules,
    ))

    list.each(module.sorted_functions, fn(funcs) {
      list.try_map(funcs, dict.get(module.functions, _))
      |> result.map(compile_functions(module, _))
    })

    let module = analysis.module_from_internals(module)
    dict.insert(modules, module_id, module)
  })
}

pub fn main() {
  impl() |> pprint.debug
}
