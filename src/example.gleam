import gleam/int

import gleam/list

fn add(a, b) {
  a + b
}

fn fold(list: List(a), initial: acc, fun: fn(acc, a) -> acc) -> acc {
  case list {
    [] -> initial
    [x, ..rest] -> {
      let intermediate = fun(initial, x)
      fold(rest, intermediate, fun)
    }
  }
}

pub fn sum(numbers) {
  list.fold(numbers, 0, int.add)
}
