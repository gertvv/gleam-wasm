import gleam/int

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
  fold(numbers, 0, int.add)
}
