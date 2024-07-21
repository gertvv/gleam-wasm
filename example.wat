(module
  ;; Compile using binaryen (>= 118):
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

  (type $gleam/Closure
    (struct
      (field $context (ref null any))
      (field $function (ref func))
    )
  )

  (type $gleam/Int (struct i64))

  (import "gleam" "list_head" (func $gleam/list_head (param $list (ref $gleam/List)) (result (ref any))))
  (import "gleam" "list_tail" (func $gleam/list_tail (param $list (ref $gleam/List)) (result (ref null $gleam/List))))
  (import "gleam" "list_is_empty" (func $gleam/list_is_empty (param $list (ref null $gleam/List)) (result i32)))
  (import "gleam" "int_box" (func $gleam/int_box (param $value i64) (result (ref $gleam/Int))))
  (import "gleam" "int_unbox" (func $gleam/int_unbox (param $boxed (ref any)) (result i64)))

  ;; -- "compiled" gleam --
 
  ;; typed implementation of add
  (func $add (param $a i64) (param $b i64) (result i64)
    (i64.add (local.get $a) (local.get $b))
  )

  ;; wrapper for "generic" calls
  (func $add_anyref (param $@context (ref null any)) (param $a (ref any)) (param $b (ref any)) (result (ref any))
    (call $gleam/int_box
      (call $add
        (call $gleam/int_unbox (local.get $a))
        (call $gleam/int_unbox (local.get $b))
      )
    )
  )

  (type $folder (func (param (ref null any)) (param (ref any)) (param (ref any)) (result (ref any))))
  (func $fold
    (param $l (ref null $gleam/List))
    (param $i (ref any))
    (param $f (ref $gleam/Closure))
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
            (struct.get $gleam/Closure 0 (local.get $f))
            (local.get $i)
            (call $gleam/list_head (local.get $nel))
            (ref.cast (ref $folder) (struct.get $gleam/Closure 1 (local.get $f)))
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
      (call
        $fold
        (local.get $l)
        (struct.new $gleam/Int (i64.const 0))
        (struct.new $gleam/Closure (ref.null any) (ref.func $add_anyref))
      )
    )
  )

  (export "sum" (func $sum))
)
