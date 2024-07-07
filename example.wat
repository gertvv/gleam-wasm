(module
  ;; Compile using binaryen:
  ;;   wasm-as example.wat --enable-gc --enable-reference-types --enable-tail-call

  ;; Possibly this can be used for codegen in the gleam compiler:
  ;; https://docs.rs/wasm-encoder/latest/wasm_encoder/

  ;; -- List implementation --

  (type $list
    (struct
      (field $value (ref any))
      (field $tail (ref null $list))
    )
  )

  (func $list_empty (result (ref null $list))
    (ref.null $list)
  )

  (func $list_non_empty (param $value (ref any)) (param $tail (ref null $list)) (result (ref $list))
    (struct.new $list
      (local.get $value)
      (local.get $tail)
    )
  )

  (func $list_head (param $list (ref $list)) (result (ref any))
    (struct.get $list $value (local.get $list))
  )

  (func $list_tail (param $list (ref $list)) (result (ref null $list))
    (struct.get $list $tail (local.get $list))
  )

  (func $list_is_empty (param $list (ref null $list)) (result i32)
    (ref.is_null (local.get $list))
  )

  ;; -- Int implementation: boxed i64 --

  (type $int (struct i64))

  (func $int_unbox (param $boxed (ref $int)) (result i64)
    (struct.get $int 0 (local.get $boxed))
  )

  (func $int_box (param $value i64) (result (ref $int))
    (struct.new $int (local.get $value))
  )

  ;; -- "compiled" gleam --
 
  ;; typed implementation of add
  (func $add (param $a (ref $int)) (param $b (ref $int)) (result (ref $int))
    (call $int_box
      (i64.add
        (call $int_unbox (local.get $a))
        (call $int_unbox (local.get $b))
      )
    )
  )

  ;; wrapper for "generic" calls
  (func $add_anyref (param $a (ref any)) (param $b (ref any)) (result (ref any))
    (call $add
      (ref.cast (ref $int) (local.get $a))
      (ref.cast (ref $int) (local.get $b))
    )
  )

  (type $folder (func (param (ref any)) (param (ref any)) (result (ref any))))
  (func $fold
    (param $l (ref null $list))
    (param $i (ref any))
    (param $f (ref $folder))
    (result (ref any))
    (local $nel (ref $list))
    (if (call $list_is_empty (local.get $l)) (then (return (local.get $i))))
    (local.set $nel (ref.as_non_null (local.get $l)))
    (return_call $fold
      (call $list_tail (local.get $nel))
      (call_ref $folder
        (local.get $i)
        (call $list_head (local.get $nel))
        (local.get $f)
      )
      (local.get $f)
    )
  )

  (func $sum
    (param $l (ref $list))
    (result (ref $int))
    (ref.cast (ref $int)
      (call $fold (local.get $l) (struct.new $int (i64.const 0)) (ref.func $add_anyref))
    )
  )

  (export "list_empty" (func $list_empty))
  (export "list_non_empty" (func $list_non_empty))
  (export "list_head" (func $list_head))
  (export "list_tail" (func $list_tail))
  (export "list_is_empty" (func $list_is_empty))
  (export "int_box" (func $int_box))
  (export "int_unbox" (func $int_unbox))
  (export "sum" (func $sum))
)
