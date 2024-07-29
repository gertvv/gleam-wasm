(module
  ;; -- List implementation --

  (type $list
    (struct
      (field $value (ref null any))
      (field $tail (ref null $list))
    )
  )

  (func $list_empty (result (ref $list))
    (struct.new $list
      (ref.null any)
      (ref.null $list)
    )
  )

  (func $list_non_empty (param $value (ref any)) (param $tail (ref $list)) (result (ref $list))
    (struct.new $list
      (local.get $value)
      (local.get $tail)
    )
  )

  (func $list_head (param $list (ref $list)) (result (ref any))
    (ref.as_non_null (struct.get $list $value (local.get $list)))
  )

  (func $list_tail (param $list (ref $list)) (result (ref $list))
    (ref.as_non_null (struct.get $list $tail (local.get $list)))
  )

  (func $list_is_empty (param $list (ref $list)) (result i32)
    (ref.is_null (struct.get $list $value (local.get $list)))
  )

  ;; -- Int implementation: boxed i64 --

  (type $int (struct i64))

  (func $int_unbox (param $boxed (ref any)) (result i64)
    (struct.get $int 0 (ref.cast (ref $int) (local.get $boxed)))
  )

  (func $int_box (param $value i64) (result (ref $int))
    (struct.new $int (local.get $value))
  )

  (export "list_empty" (func $list_empty))
  (export "list_non_empty" (func $list_non_empty))
  (export "list_head" (func $list_head))
  (export "list_tail" (func $list_tail))
  (export "list_is_empty" (func $list_is_empty))
  (export "int_box" (func $int_box))
  (export "int_unbox" (func $int_unbox))
)
