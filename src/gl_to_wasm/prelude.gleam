import gl_wasm/wasm
import gleam/list
import gleam/option.{None, Some}
import gleam/result

/// Prelude type: Int - boxed so it can be referenced
pub const int_type = wasm.Struct(
  Some("gleam/Int"),
  [wasm.ValueType(None, wasm.Immutable, wasm.I64)],
)

pub const int_index = 0

/// Prelude type: Float - boxed so it can be referenced
pub const float_type = wasm.Struct(
  Some("gleam/Float"),
  [wasm.ValueType(None, wasm.Immutable, wasm.F64)],
)

pub const float_index = 1

/// Prelude type: Bool - boxed so it can be referenced
pub const bool_type = wasm.Struct(
  Some("gleam/Bool"),
  [wasm.ValueType(None, wasm.Immutable, wasm.I32)],
)

pub const bool_index = 2

/// Prelude type: Nil - represented by an empty struct
pub const nil_type = wasm.Struct(Some("gleam/Nil"), [])

pub const nil_index = 3

/// Prelude type: List (variant types at 5 & 6)
///
/// An empty list sentinel is used so that we don't need to pass nullable
/// references to this (or any) type.
pub const list_type = [
  wasm.SubOpen([], wasm.Struct(Some("gleam/List"), [])),
  wasm.SubFinal([list_index], wasm.Struct(Some("gleam/List/EmptyList"), [])),
  wasm.SubFinal(
    [list_index],
    wasm.Struct(
      Some("gleam/List/NonEmptyList"),
      [
        wasm.ValueType(
          Some("next"),
          wasm.Immutable,
          wasm.Ref(wasm.NonNull(wasm.ConcreteType(list_index))),
        ),
        wasm.ValueType(
          Some("item"),
          wasm.Immutable,
          wasm.Ref(wasm.NonNull(wasm.AbstractAny)),
        ),
      ],
    ),
  ),
]

pub const list_index = 4

/// Prelude supporting type: a byte array to use as backing for String and BitArray
pub const byte_array = wasm.Array(
  None,
  wasm.PackedType(None, wasm.Immutable, wasm.I8),
)

pub const byte_array_index = 7

/// Prelude type: String
pub const string_type = wasm.Struct(
  Some("gleam/String"),
  [
    wasm.ValueType(Some("length"), wasm.Immutable, wasm.I32),
    wasm.ValueType(
      Some("bytes"),
      wasm.Immutable,
      wasm.Ref(wasm.NonNull(wasm.ConcreteType(byte_array_index))),
    ),
  ],
)

pub const string_index = 8

/// Prelude type: BitArray
pub const bit_array_type = wasm.Struct(
  Some("gleam/BitArray"),
  [
    wasm.ValueType(Some("byte_size"), wasm.Immutable, wasm.I32),
    wasm.ValueType(Some("bit_size"), wasm.Immutable, wasm.I64),
    wasm.ValueType(
      Some("bytes"),
      wasm.Immutable,
      wasm.Ref(wasm.NonNull(wasm.ConcreteType(byte_array_index))),
    ),
  ],
)

pub const bit_array_index = 9

/// Prelude type 8: Result
pub const result_type = [
  wasm.SubOpen([], wasm.Struct(Some("gleam/Result"), [])),
  wasm.SubFinal(
    [result_index],
    wasm.Struct(
      Some("gleam/Result/Ok"),
      [
        wasm.ValueType(
          Some("item"),
          wasm.Immutable,
          wasm.Ref(wasm.NonNull(wasm.AbstractAny)),
        ),
      ],
    ),
  ),
  wasm.SubFinal(
    [result_index],
    wasm.Struct(
      Some("gleam/Result/Error"),
      [
        wasm.ValueType(
          Some("item"),
          wasm.Immutable,
          wasm.Ref(wasm.NonNull(wasm.AbstractAny)),
        ),
      ],
    ),
  ),
]

pub const result_index = 10

fn wrap(t) {
  [wasm.SubFinal([], t)]
}

/// Register the prelude types in any module
pub fn add_prelude_types(
  mb: wasm.ModuleBuilder,
) -> Result(wasm.ModuleBuilder, String) {
  // TODO: validate that we get the expected type indices
  list.try_fold(
    over: [
      wrap(int_type),
      wrap(float_type),
      wrap(bool_type),
      wrap(nil_type),
      list_type,
      wrap(byte_array),
      wrap(string_type),
      wrap(bit_array_type),
      result_type,
    ],
    from: mb,
    with: fn(mb, t) {
      wasm.add_sub_type_group(mb, t)
      |> result.map(fn(res) { res.0 })
    },
  )
}

pub fn build_prelude() -> wasm.ModuleBuilder {
  let mb = wasm.create_module_builder(Some("gleam"))
  let assert Ok(mb) = add_prelude_types(mb)

  // Create the Nil instance
  let assert Ok(#(mb, gb)) =
    wasm.create_global_builder(
      mb,
      Some("gleam/nil"),
      wasm.Immutable,
      wasm.Ref(wasm.NonNull(wasm.ConcreteType(nil_index))),
    )
  let assert Ok(gb) = wasm.add_instruction(gb, wasm.StructNew(nil_index))
  let assert Ok(gb) = wasm.add_instruction(gb, wasm.End)
  let assert Ok(mb) = wasm.finalize_global(mb, gb)

  // Create the empty List instance
  let assert Ok(#(mb, gb)) =
    wasm.create_global_builder(
      mb,
      Some("gleam/empty_list"),
      wasm.Immutable,
      wasm.Ref(wasm.NonNull(wasm.ConcreteType(list_index))),
    )
  let assert Ok(gb) = wasm.add_instruction(gb, wasm.StructNew(list_index + 1))
  let assert Ok(gb) = wasm.add_instruction(gb, wasm.End)
  let assert Ok(mb) = wasm.finalize_global(mb, gb)

  let assert Ok(mb) = wasm.add_export(mb, wasm.ExportGlobal("nil", 0))
  let assert Ok(mb) = wasm.add_export(mb, wasm.ExportGlobal("empty_list", 1))

  mb
}
