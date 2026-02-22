pub type IOContext(e) {
  IOContext(
    list: fn(String) -> Result(List(String), e),
    list_recursive: fn(String) -> Result(List(String), e),
    read: fn(String) -> Result(String, e),
  )
}
