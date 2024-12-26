import compiler.{type CompilerError}
import filepath
import glance
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import gloml
import simplifile

pub type Project {
  Project(
    name: String,
    target: String,
    packages: Dict(String, Package),
    parse_module: fn(Project, ModuleId) -> Result(glance.Module, CompilerError),
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

pub fn shorthand(loc: ModuleId) -> String {
  filepath.base_name(loc.module_path)
}

fn parse_gleam_toml(source_path: String) {
  simplifile.read(filepath.join(source_path, "gleam.toml"))
  |> result.map_error(compiler.FileError)
  |> result.try(fn(str) {
    gloml.decode(
      str,
      dynamic.decode2(
        fn(name, deps) {
          #(name, case deps {
            Some(deps) -> dict.keys(deps)
            None -> []
          })
        },
        dynamic.field("name", dynamic.string),
        dynamic.optional_field(
          "dependencies",
          dynamic.dict(dynamic.string, dynamic.string),
        ),
      ),
    )
    |> result.map_error(compiler.PackageTomlError)
  })
}

fn list_modules(package_path: String) {
  let prefix = filepath.join(package_path, "src")
  let suffix = ".gleam"
  use files <- result.map(
    simplifile.get_files(prefix)
    |> result.map_error(compiler.FileError),
  )
  list.map(files, string.drop_start(_, string.length(prefix) + 1))
  |> list.filter(string.ends_with(_, suffix))
  |> list.map(string.drop_end(_, string.length(suffix)))
}

fn scan_package(source_path: String) {
  case
    simplifile.is_directory(source_path),
    simplifile.is_file(filepath.join(source_path, "gleam.toml"))
  {
    Ok(True), Ok(True) -> {
      use #(name, dependencies) <- result.try(parse_gleam_toml(source_path))
      use modules <- result.map(list_modules(source_path))
      GleamPackage(name, source_path, dependencies, set.from_list(modules))
    }
    Ok(True), _ -> {
      Ok(OtherPackage(filepath.base_name(source_path), source_path))
    }
    Ok(False), _ -> Error(compiler.FileError(simplifile.Enotdir))
    Error(e), _ -> Error(compiler.FileError(e))
  }
}

fn scan_packages(
  source_path: String,
  target: String,
  packages: Dict(String, Package),
  chain: Set(String),
) -> Result(Project, CompilerError) {
  use package <- result.try(scan_package(source_path))
  let packages = dict.insert(packages, package.name, package)
  case package {
    GleamPackage(dependencies: dependencies, ..) -> {
      list.try_fold(
        dependencies,
        Project(package.name, target, packages, parse_module),
        fn(project, dependency) {
          case
            set.contains(chain, dependency),
            dict.get(project.packages, dependency)
          {
            True, _ -> Error(compiler.CircularDependencyError)
            False, Ok(_) -> Ok(project)
            False, Error(_) -> {
              result.map(
                scan_packages(
                  filepath.join("./build/packages", dependency),
                  target,
                  project.packages,
                  set.insert(chain, package.name),
                ),
                fn(nested) {
                  Project(project.name, target, nested.packages, parse_module)
                },
              )
            }
          }
        },
      )
    }
    OtherPackage(name, _) -> Ok(Project(name, target, packages, parse_module))
  }
}

pub fn scan_project(source_path: String, target: String) {
  scan_packages(source_path, target, dict.new(), set.new())
}

pub fn main() {
  scan_project(".", "javascript")
  |> io.debug
}

fn as_path(project: Project, location: ModuleId) -> String {
  case location.package_name == project.name {
    True -> ["src", location.module_path <> ".gleam"]
    False -> [
      "build",
      "packages",
      location.package_name,
      "src",
      location.module_path <> ".gleam",
    ]
  }
  |> list.fold(".", filepath.join)
}

fn parse_module(
  project: Project,
  location: ModuleId,
) -> Result(glance.Module, CompilerError) {
  simplifile.read(as_path(project, location))
  |> result.map_error(compiler.FileError)
  |> result.try(fn(str) {
    glance.module(str)
    |> result.map_error(compiler.ParseError)
  })
}

pub fn get_module_location(
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
