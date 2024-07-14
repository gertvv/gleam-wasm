(module
  ;; Compile using binaryen:
  ;;   wasm-as example.wat --enable-gc --enable-reference-types --enable-tail-call

  ;; Possibly this can be used for codegen in the gleam compiler:
  ;; https://docs.rs/wasm-encoder/latest/wasm_encoder/

  ;; Import gleam built-ins

  (type $gleam/List
    (struct
      (field $value (ref any))
      (field $tail (ref null $gleam/List))
    )
  )

  (type $gleam/Int (struct i64))

  (import "gleam" "list_head" (func $gleam/list_head (param $list (ref $gleam/List)) (result (ref any))))
  (import "gleam" "list_tail" (func $gleam/list_tail (param $list (ref $gleam/List)) (result (ref null $gleam/List))))
  (import "gleam" "list_is_empty" (func $gleam/list_is_empty (param $list (ref null $gleam/List)) (result i32)))
  (import "gleam" "int_box" (func $gleam/int_box (param $value i64) (result (ref $gleam/Int))))
  (import "gleam" "int_unbox" (func $gleam/int_unbox (param $boxed (ref $gleam/Int)) (result i64)))

  ;; -- "compiled" gleam --
 
  ;; typed implementation of add
  (func $add (param $a (ref $gleam/Int)) (param $b (ref $gleam/Int)) (result (ref $gleam/Int))
    (call $gleam/int_box
      (i64.add
        (call $gleam/int_unbox (local.get $a))
        (call $gleam/int_unbox (local.get $b))
      )
    )
  )

  ;; wrapper for "generic" calls
  (func $add_anyref (param $a (ref any)) (param $b (ref any)) (result (ref any))
    (call $add
      (ref.cast (ref $gleam/Int) (local.get $a))
      (ref.cast (ref $gleam/Int) (local.get $b))
    )
  )

  (type $folder (func (param (ref any)) (param (ref any)) (result (ref any))))
  (func $fold
    (param $l (ref null $gleam/List))
    (param $i (ref any))
    (param $f (ref $folder))
    (result (ref any))
    (local $nel (ref $gleam/List))
    (if
      (call $gleam/list_is_empty (local.get $l))
      (then (return (local.get $i)))
      (else
        (local.set $nel (ref.as_non_null (local.get $l)))
        (return_call $fold
          (call $gleam/list_tail (local.get $nel))
          (call_ref $folder
            (local.get $i)
            (call $gleam/list_head (local.get $nel))
            (local.get $f)
          )
          (local.get $f)
        )
      )
    )
  )

  (func $sum
    (param $l (ref $gleam/List))
    (result (ref $gleam/Int))
    (ref.cast (ref $gleam/Int)
      (call $fold (local.get $l) (struct.new $gleam/Int (i64.const 0)) (ref.func $add_anyref))
    )
  )

  (export "sum" (func $sum))
)
