import gleeunit/should
import graph

pub fn squash_cycles_test() {
  graph.squash_cycles(["a", "b", "c"], [#("a", "b"), #("a", "c"), #("b", "c")])
  |> should.equal(
    #([["c"], ["b"], ["a"]], [#(["a"], ["b"]), #(["a"], ["c"]), #(["b"], ["c"])]),
  )

  graph.squash_cycles(["a", "b", "c"], [#("a", "b"), #("b", "c"), #("c", "a")])
  |> should.equal(#([["c", "b", "a"]], []))

  graph.squash_cycles(["a", "b", "c"], [#("a", "b"), #("a", "c"), #("c", "a")])
  |> should.equal(#([["b"], ["c", "a"]], [#(["c", "a"], ["b"])]))

  graph.squash_cycles(["a", "b", "c", "d"], [
    #("a", "b"),
    #("b", "c"),
    #("c", "a"),
    #("b", "d"),
    #("d", "c"),
  ])
  |> should.equal(#([["c", "b", "a", "d"]], []))

  graph.squash_cycles(["a", "b", "c", "d"], [
    #("a", "b"),
    #("b", "a"),
    #("c", "d"),
    #("d", "c"),
    #("a", "c"),
  ])
  |> should.equal(#([["d", "c"], ["b", "a"]], [#(["b", "a"], ["d", "c"])]))
}

pub fn topological_sort_test() {
  // lowest in-degree last
  graph.topological_sort(["sum", "fold", "add"], [
    #("sum", "fold"),
    #("sum", "add"),
  ])
  |> should.equal(Ok(["fold", "add", "sum"]))

  // nodes without edges should be preserved
  graph.topological_sort(["a", "b", "c"], [])
  |> should.equal(Ok(["a", "b", "c"]))

  // cycles are an error
  graph.topological_sort(["a", "b", "c"], [
    #("a", "b"),
    #("b", "c"),
    #("c", "a"),
  ])
  |> should.equal(Error(Nil))
}
