import argv
import gleam/io
import gleam/list
import infer
import project

pub fn main() {
  let args = argv.load().arguments
  case list.first(args) {
    Ok("project") -> project.main()
    Ok("infer") -> infer.main()
    Ok(module) -> io.println("Unknown module: " <> module)
    Error(_) -> io.println("Specify the module to run")
  }
}
