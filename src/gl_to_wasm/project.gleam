import filepath
import gl_to_wasm/graph.{type Graph}
import gl_to_wasm/io_context.{type IOContext}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import gleam/string
import tom

pub type Project {
  Project(
    name: String,
    source_path: String,
    target: String,
    packages: Dict(String, Package),
  )
}

pub type Package {
  GleamPackage(
    name: String,
    source_path: String,
    dependencies: List(String),
    modules: Set(String),
  )
  OtherPackage(name: String, source_path: String)
}

pub type ModuleId {
  ModuleId(package_name: String, module_path: String)
}

pub type Error(e) {
  FileError(path: String, error: e)
  PackageTomlParseError(path: String, error: tom.ParseError)
  PackageTomlInvalidError(path: String, error: String)
  NotGleamError
}

pub fn module_shorthand(loc: ModuleId) -> String {
  filepath.base_name(loc.module_path)
}

fn parse_gleam_toml(source_path: String, io: IOContext(e)) {
  let path = filepath.join(source_path, "gleam.toml")
  io.read(path)
  |> result.map_error(FileError(path, _))
  |> result.try(fn(str) {
    use toml <- result.try(
      tom.parse(str)
      |> result.map_error(PackageTomlParseError(path, _)),
    )
    use name <- result.map(
      tom.get_string(toml, ["name"])
      |> result.replace_error(PackageTomlInvalidError(path, "Name is missing")),
    )
    let deps =
      tom.get_table(toml, ["dependencies"])
      |> result.unwrap(dict.new())
      |> dict.keys
    #(name, deps)
  })
}

fn list_modules(package_path: String, io: IOContext(e)) {
  let prefix = filepath.join(package_path, "src")
  let suffix = ".gleam"
  use files <- result.map(
    io.list_recursive(prefix)
    |> result.map_error(FileError(prefix, _)),
  )
  list.map(files, string.drop_start(_, string.length(prefix) + 1))
  |> list.filter(string.ends_with(_, suffix))
  |> list.map(string.drop_end(_, string.length(suffix)))
}

fn scan_package(source_path: String, io: IOContext(e)) {
  use files <- result.try(
    io.list(source_path)
    |> result.map_error(FileError(source_path, _)),
  )
  case list.contains(files, "gleam.toml") {
    True -> {
      use #(name, dependencies) <- result.try(parse_gleam_toml(source_path, io))
      use modules <- result.map(list_modules(source_path, io))
      GleamPackage(name, source_path, dependencies, set.from_list(modules))
    }
    False -> {
      Ok(OtherPackage(filepath.base_name(source_path), source_path))
    }
  }
}

fn scan_packages(
  project_name: String,
  source_path: String,
  package_name: String,
  packages: Dict(String, Package),
  io: IOContext(e),
) -> Result(#(String, Dict(String, Package)), Error(e)) {
  use package <- result.try(scan_package(
    package_source_path(project_name, source_path, package_name),
    io,
  ))
  let packages = dict.insert(packages, package.name, package)
  let deps = case package {
    GleamPackage(dependencies: dependencies, ..) -> dependencies
    OtherPackage(..) -> []
  }
  use deps <- result.map(
    list.try_fold(deps, packages, fn(packages, dep) {
      use <- bool.guard(dict.has_key(packages, dep), Ok(packages))
      result.map(
        scan_packages(project_name, source_path, dep, packages, io),
        pair.second,
      )
    }),
  )
  #(package.name, deps)
}

pub fn scan_project(source_path: String, target: String, io: IOContext(e)) {
  use project_name <- result.try(
    scan_package(source_path, io)
    |> result.try(fn(package) {
      case package {
        GleamPackage(name:, ..) -> Ok(name)
        _ -> Error(NotGleamError)
      }
    }),
  )
  use #(name, packages) <- result.map(scan_packages(
    project_name,
    source_path,
    project_name,
    dict.new(),
    io,
  ))
  Project(name:, source_path:, target:, packages:)
}

pub fn package_build_path(project: Project, package_name: String) -> String {
  [project.source_path, "build", "dev", project.target, package_name]
  |> list.fold("", filepath.join)
}

fn package_source_path(project_name, source_path, package_name) {
  case package_name == project_name {
    True -> source_path
    False ->
      [source_path, "build", "packages", package_name]
      |> list.fold("", filepath.join)
  }
}

pub fn module_source_path(project: Project, location: ModuleId) -> String {
  [
    package_source_path(
      project.name,
      project.source_path,
      location.package_name,
    ),
    "src",
    location.module_path <> ".gleam",
  ]
  |> list.fold("", filepath.join)
}

pub fn module_id(
  project: Project,
  package_name: String,
  path: String,
) -> Result(ModuleId, Nil) {
  case dict.get(project.packages, package_name) {
    Ok(GleamPackage(modules: modules, ..)) ->
      case set.contains(modules, path) {
        True -> Ok(ModuleId(package_name, path))
        False -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

pub fn dependency_graph(project: Project) -> Graph(String) {
  #(
    dict.keys(project.packages),
    list.flat_map(dict.values(project.packages), fn(package) {
      case package {
        GleamPackage(name:, dependencies:, ..) ->
          list.map(dependencies, fn(dep) { #(name, dep) })
        OtherPackage(..) -> []
      }
    }),
  )
}
