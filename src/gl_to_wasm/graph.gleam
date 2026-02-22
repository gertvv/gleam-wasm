import gleam/list
import gleam/pair
import gleam/set.{type Set}

pub type Graph(node) =
  #(List(node), List(#(node, node)))

pub fn squash_cycles(
  nodes: List(node),
  edges: List(#(node, node)),
) -> Graph(List(node)) {
  let nodes = list.map(nodes, fn(node) { [node] })
  let edges =
    edges
    |> list.map(fn(edge) {
      let #(a, b) = edge
      #([a], [b])
    })
  squash_cycles_internal(nodes, edges, [])
}

fn squash_cycles_internal(
  remaining_nodes: List(List(node)),
  edges: List(#(List(node), List(node))),
  squashed_nodes: List(List(node)),
) -> Graph(List(node)) {
  case remaining_nodes {
    [] -> #(squashed_nodes, edges)
    [node, ..rest] -> {
      case detect_cycle(edges, [], node) {
        Error(Nil) ->
          squash_cycles_internal(rest, edges, [node, ..squashed_nodes])
        Ok(cycle) -> {
          let squashed = list.flatten(cycle)
          let remove_if_squashed = fn(lst) {
            list.filter(lst, fn(node) { !list.contains(cycle, node) })
          }
          let remaining_nodes = remove_if_squashed(remaining_nodes)
          let squashed_nodes = [
            list.flatten(cycle),
            ..remove_if_squashed(squashed_nodes)
          ]
          let edges =
            list.map(edges, fn(edge) {
              let replace_if_squashed = fn(node) {
                case list.contains(cycle, node) {
                  True -> squashed
                  False -> node
                }
              }
              edge
              |> pair.map_first(replace_if_squashed)
              |> pair.map_second(replace_if_squashed)
            })
            |> list.filter(fn(edge) { edge.0 != edge.1 })
            |> list.unique
          squash_cycles_internal(remaining_nodes, edges, squashed_nodes)
        }
      }
    }
  }
}

pub fn detect_cycle(
  edges: List(#(node, node)),
  sofar: List(node),
  next: node,
) -> Result(List(node), Nil) {
  case list.contains(sofar, next) {
    True -> Ok(sofar)
    False -> {
      list.filter(edges, fn(edge) { edge.0 == next })
      |> list.find_map(fn(edge) {
        let #(_from, to) = edge
        detect_cycle(edges, [next, ..sofar], to)
      })
    }
  }
}

fn topological_sort_internal(remaining: List(#(node, Set(node))), sorted) {
  let #(removed, remaining) =
    list.partition(remaining, fn(pair) { set.is_empty(pair.1) })
  let removed = list.map(removed, pair.first)
  let remaining =
    list.map(remaining, fn(pair) {
      let #(node, from_nodes) = pair
      #(node, set.drop(from_nodes, removed))
    })
  case list.is_empty(removed), list.is_empty(remaining) {
    _, True -> Ok(list.append(removed, sorted))
    True, _ -> Error(Nil)
    _, _ -> topological_sort_internal(remaining, list.append(removed, sorted))
  }
}

pub fn topological_sort(
  nodes: List(node),
  edges: List(#(node, node)),
) -> Result(List(node), Nil) {
  let in_edges =
    list.map(nodes, fn(node) {
      let from_nodes =
        list.filter_map(edges, fn(edge) {
          case edge {
            #(from, to) if to == node -> Ok(from)
            _ -> Error(Nil)
          }
        })
        |> set.from_list
      #(node, from_nodes)
    })
  topological_sort_internal(in_edges, [])
}
