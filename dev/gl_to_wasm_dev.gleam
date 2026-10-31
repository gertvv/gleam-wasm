import argv
import gleam/io
import gleam/list
import project

pub fn main() {
  let args = argv.load().arguments
  case list.first(args) {
    Ok("project") -> project.main()
    Ok(module) -> io.println("Unknown module: " <> module)
    Error(_) -> io.println("Specify the module to run")
  }
}
