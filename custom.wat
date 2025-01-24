(module
  (type $gleam/Int (struct (field i64)))
  (type $gleam/Nil (struct))
  (rec
    (type
      $gleam/List
      (sub (struct)))
    (type
      $gleam/List/Empty
      (sub $gleam/List (struct)))
    (type
      $gleam/List/NonEmpty
      (sub $gleam/List
        (struct
          (field $next (ref $gleam/List))
          (field $elem (ref any))))))
  (rec
    (type $gleam/Result (sub (struct)))
    (type $gleam/Result/Ok (sub $gleam/Result (struct (field $item (ref any)))))
    (type $gleam/Result/Error (sub $gleam/Result (struct (field $item (ref any))))))
  (func
    $gleam/Int (param $value i64) (result (ref $gleam/Int))
    local.get $value
    struct.new $gleam/Int)
  (func
    $gleam/Empty (result (ref $gleam/List))
    struct.new $gleam/List/Empty)
  (func
    $gleam/NonEmpty (param $list (ref $gleam/List)) (param $elem (ref any)) (result (ref $gleam/List))
    local.get $list
    local.get $elem
    struct.new $gleam/List/NonEmpty)
  (func
    $gleam/list/first (param $list (ref $gleam/List)) (result (ref $gleam/Result))
    ;; TODO: could probably implement using br_on_cast
    local.get $list
    ref.test (ref $gleam/List/Empty)
    (if
      (result (ref $gleam/Result))
      (then
        struct.new $gleam/Nil
        struct.new $gleam/Result/Error
        )
      (else
        local.get $list
        ref.cast (ref $gleam/List/NonEmpty)
        struct.get $gleam/List/NonEmpty $elem
        struct.new $gleam/Result/Ok
        )))
  (func
    $gleam/Ok (param $item (ref any)) (result (ref $gleam/Result))
    local.get $item
    struct.new $gleam/Result/Ok)
  (func
    $gleam/Error (param $item (ref any)) (result (ref $gleam/Result))
    local.get $item
    struct.new $gleam/Result/Error)
  (func
    $gleam/result/is_ok (param $result (ref $gleam/Result)) (result i32)
    local.get $result
    ref.test (ref $gleam/Result/Ok))

  (export "gleam/Int" (func $gleam/Int))
  (export "gleam/Ok" (func $gleam/Ok))
  (export "gleam/Error" (func $gleam/Error))
  (export "gleam/Empty" (func $gleam/Empty))
  (export "gleam/NonEmpty" (func $gleam/NonEmpty))
  (export "gleam/result/is_ok" (func $gleam/result/is_ok))
  (export "gleam/list/first" (func $gleam/list/first))
)
