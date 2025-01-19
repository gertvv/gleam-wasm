import analysis
import compiler
import filepath
import gl_wasm/wasm
import glance
import gleam/dict.{type Dict}
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

/// Type for mapping to WASM -- stripped generics
type ProjectedType {
  TypeId(analysis.TypeId)
  FunctionType(List(ProjectedType))
  HoleType
}

type TypeRegistry =
  Dict(ProjectedType, Int)

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

  let types =
    dict.from_list([
      #(TypeId(analysis.BuiltInType(analysis.IntType)), 0),
      #(TypeId(analysis.BuiltInType(analysis.FloatType)), 1),
      #(TypeId(analysis.BuiltInType(analysis.BoolType)), 2),
      #(TypeId(analysis.BuiltInType(analysis.NilType)), 3),
      #(TypeId(analysis.BuiltInType(analysis.ListType)), 4),
      #(TypeId(analysis.BuiltInType(analysis.StringType)), 6),
      #(TypeId(analysis.BuiltInType(analysis.BitArrayType)), 7),
      #(TypeId(analysis.TypeFromModule(analysis.gleam_module_id, "Result")), 8),
    ])

  // Generate types
  // TODO: mutually recursive functions
  use #(mb, types) <- result.try(
    list.try_fold(
      over: list.flatten(module.implemented_functions),
      from: #(mb, types),
      with: fn(acc, func) {
        let #(mb, types) = acc
        register_func_types(mb, types, func)
      },
    ),
  )

  // TODO: generate imports

  // generate functions
  // TODO: mutually recursive functions
  list.try_fold(
    over: list.flatten(module.implemented_functions),
    from: mb,
    with: fn(mb, func) { compile_function(mb, types, module, func) },
  )

  // TODO: generate exports

  output_module(project, module.location, mb)
}

fn register_func_types(
  mb: wasm.ModuleBuilder,
  registry: TypeRegistry,
  func: analysis.Function,
) -> Result(#(wasm.ModuleBuilder, TypeRegistry), compiler.CompilerError) {
  let projected_type = project_type(analysis.signature_type(func.signature))
  register_type(mb, registry, projected_type)
  |> result.map(fn(triple) {
    let #(mb, registry, _) = triple
    #(mb, registry)
  })
}

fn register_type(
  mb: wasm.ModuleBuilder,
  registry: TypeRegistry,
  t: ProjectedType,
) -> Result(
  #(wasm.ModuleBuilder, TypeRegistry, Option(Int)),
  compiler.CompilerError,
) {
  use _ <- result.try_recover(
    dict.get(registry, t)
    |> result.map(fn(i) { #(mb, registry, Some(i)) }),
  )
  case t {
    FunctionType(args) ->
      analysis.try_map_fold(args, #(mb, registry), fn(acc, arg) {
        let #(mb, registry) = acc
        register_type(mb, registry, arg)
        |> result.map(fn(triple) {
          let #(mb, registry, arg) = triple
          #(#(mb, registry), arg)
        })
      })
      |> result.try(fn(acc) {
        let #(#(mb, registry), types) = acc
        let types =
          list.map(types, fn(t) {
            case t {
              None -> wasm.AbstractAny
              Some(i) -> wasm.ConcreteType(i)
            }
            |> wasm.NonNull
            |> wasm.Ref
          })
        let func_type = case types {
          [] -> wasm.Func(None, [], [])
          [return, ..params] -> wasm.Func(None, params, [return])
        }
        wasm.add_type(mb, func_type)
        |> result.map(fn(res) {
          let #(mb, i) = res
          let registry = dict.insert(registry, t, i)
          #(mb, registry, Some(i))
        })
        |> result.map_error(compiler.AnotherTypeError)
      })
    HoleType -> Ok(#(mb, registry, None))
    TypeId(_) -> todo
  }
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

fn project_type(t: analysis.Type) -> ProjectedType {
  case t {
    analysis.FunctionType(params, return) ->
      FunctionType(list.map([return, ..params], project_type))
    analysis.TypeConstructor(type_id, _) -> TypeId(type_id)
    analysis.TypeVariable(_) -> HoleType
  }
}

fn compile_function(
  mb: wasm.ModuleBuilder,
  registry: TypeRegistry,
  _internals: analysis.ModuleInternals,
  function: analysis.Function,
) {
  let projected_type = project_type(analysis.signature_type(function.signature))
  use type_index <- result.try(
    dict.get(registry, projected_type)
    |> result.replace_error(compiler.AnotherTypeError("Function type lost?")),
  )
  use #(mb, fb) <- result.try(
    wasm.create_function_builder(
      mb,
      wasm.FunctionSignature(
        type_index,
        Some(function.signature.name),
        Some(
          list.map(function.argument_names, fn(ass_name) {
            case ass_name {
              glance.Named(name) -> name
              glance.Discarded(name) -> name
            }
          }),
        ),
      ),
    )
    |> result.map_error(compiler.AnotherTypeError),
  )
  pprint.debug(fb)
  todo
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
