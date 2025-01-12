import gl_wasm/wasm
import gleam/list
import gleam/option.{None, Some}
import gleam/result

/// Prelude type 0: Int - boxed so it can be referenced
pub const int_type = wasm.Struct(
  Some("gleam/Int"),
  [wasm.ValueType(None, wasm.Immutable, wasm.I64)],
)

/// Prelude type 1: Float - boxed so it can be referenced
pub const float_type = wasm.Struct(
  Some("gleam/Float"),
  [wasm.ValueType(None, wasm.Immutable, wasm.F64)],
)

/// Prelude type 2: Bool - boxed so it can be referenced
pub const bool_type = wasm.Struct(
  Some("gleam/Bool"),
  [wasm.ValueType(None, wasm.Immutable, wasm.I32)],
)

/// Prelude type 3: Nil - represented by an empty struct
pub const nil_type = wasm.Struct(Some("gleam/Nil"), [])

/// Prelude type 4: List
///
/// An empty list sentinel is used so that we don't need to pass nullable
/// references to this (or any) type.
pub const list_type = wasm.Struct(
  Some("gleam/List"),
  [
    wasm.ValueType(
      Some("data"),
      wasm.Immutable,
      wasm.Ref(wasm.NonNull(wasm.AbstractAny)),
    ),
    wasm.ValueType(
      Some("next"),
      wasm.Immutable,
      wasm.Ref(wasm.Nullable(wasm.ConcreteType(4))),
    ),
  ],
)

/// Prelude type 5: a byte array to use as backing for String and BitArray
pub const byte_array = wasm.Array(
  None,
  wasm.PackedType(None, wasm.Immutable, wasm.I8),
)

/// Prelude type 6: String
pub const string_type = wasm.Struct(
  Some("gleam/String"),
  [
    wasm.ValueType(Some("length"), wasm.Immutable, wasm.I32),
    wasm.ValueType(
      Some("bytes"),
      wasm.Immutable,
      wasm.Ref(wasm.NonNull(wasm.ConcreteType(5))),
    ),
  ],
)

/// Prelude type 8: BitArray
pub const bit_array_type = wasm.Struct(
  Some("gleam/BitArray"),
  [
    wasm.ValueType(Some("byte_size"), wasm.Immutable, wasm.I32),
    wasm.ValueType(Some("bit_size"), wasm.Immutable, wasm.I64),
    wasm.ValueType(
      Some("bytes"),
      wasm.Immutable,
      wasm.Ref(wasm.NonNull(wasm.ConcreteType(5))),
    ),
  ],
)

/// Prelude type 9: Result
pub const result_type = wasm.Struct(
  // TODO: have a representation using sub-types?
  Some("gleam/Result"),
  [
    wasm.ValueType(
      Some("error"),
      wasm.Immutable,
      wasm.Ref(wasm.Nullable(wasm.AbstractAny)),
    ),
    wasm.ValueType(
      Some("ok"),
      wasm.Immutable,
      wasm.Ref(wasm.Nullable(wasm.AbstractAny)),
    ),
  ],
)

/// Register the prelude types in any module
pub fn add_prelude_types(
  mb: wasm.ModuleBuilder,
) -> Result(wasm.ModuleBuilder, String) {
  // TODO: validate that we get the expected type indices
  list.try_fold(
    over: [
      int_type,
      float_type,
      bool_type,
      nil_type,
      list_type,
      byte_array,
      string_type,
      bit_array_type,
      result_type,
    ],
    from: mb,
    with: fn(mb, t) {
      wasm.add_type(mb, t)
      |> result.map(fn(res) { res.0 })
    },
  )
}

pub fn build_prelude() -> wasm.ModuleBuilder {
  let mb = wasm.create_module_builder(Some("gleam"))
  let assert Ok(mb) = add_prelude_types(mb)
  mb
}
