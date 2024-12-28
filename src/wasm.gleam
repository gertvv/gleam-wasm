import gleam/bit_array
import gleam/io
import gleam/list
import gleam/result
import gleb128
import simplifile

// API sketch:
// create_module(fname) -> module
// register_type(module, type) -> type_idx
// create_function(module, type_idx) -> FunctionBuilder w/ function_idx
// add_instruction(function, instruction) -> FunctionBuilder
// register_function(function, module) -> module
// import_function??
// emit_module
//gert@archer:~/repos/gleam-wasm$ hexdump -C simple.wasm 
// 00 61 73 6d 01 00 00 00 ; header + version
// 01 0d 02                ; type section, length 12, 2 elements
// 5f 02 7e 00 7e 00       ; struct, 2 fields, i64 imm, i64 imm
// 60 01 64 00 01 7e       ; func, 1 param, NonNullable $0, 1 result, i64
// 03 02 01 01             ; func section, lenght 2, 1 func, type $1
// 0a 0a 01                ; code section, length 10, 1 func
// 08 00                   ; code size 8, 0 locals
// 20 00                   ; local get $0
// fb 02 00 00             ; struct.get $0 $0
// 0b                      ; func end

pub fn main() {
  impl() |> io.debug
}

fn impl() {
  // TODO: implement add/fold/sum
  let mb = create_module_builder("out.wasm")
  use mb <- result.try(
    add_type_group(mb, [
      Struct([ValueType(Immutable, I64), ValueType(Immutable, I64)]),
    ]),
  )
  use mb <- result.try(
    add_type_group(mb, [Func([Ref(NonNull, ConcreteType(0))], [I64])]),
  )
  use #(mb, fb) <- result.try(create_function(mb, 1))
  use fb <- result.try(list.try_fold(
    over: [LocalGet(0), StructGet(0, 0), End],
    from: fb,
    with: add_instruction,
  ))
  use fd <- result.try(finalize_function(fb))
  let mb = ModuleBuilder(..mb, functions: [fd, ..list.drop(mb.functions, 1)])
  emit_module(mb) |> result.replace_error("Error writing to file")
}

// --------------------------------------------------------------------------- 
// Type definitions
// --------------------------------------------------------------------------- 

pub type ModuleBuilder {
  ModuleBuilder(
    output_file_path: String,
    /// type groups stored in reverse order
    types: List(List(CompositeType)),
    next_type_index: Int,
    /// functions stored in reverse order
    functions: List(FunctionDefinition),
    next_function_index: Int,
    // TODO: store function imports separately?
  )
}

pub type FunctionBuilder {
  FunctionBuilder(
    function_index: Int,
    type_index: Int,
    params: List(ValueType),
    results: List(ValueType),
    locals: List(ValueType),
    next_local_index: Int,
    code: List(Instruction),
    // TODO: some representation of the stack
  )
}

pub type FunctionDefinition {
  FunctionImport(type_index: Int, Import)
  FunctionImplementation(
    type_index: Int,
    locals: List(ValueType),
    code: List(Instruction),
  )
  FunctionMissing(type_index: Int)
}

pub type Import {
  Import(module: String, name: String)
}

/// Value types are the types that a variable accepts
pub type ValueType {
  I32
  I64
  F32
  F64
  V128
  Ref(Nullability, HeapType)
}

/// Indicates wheter a reference may be null
pub type Nullability {
  NonNull
  Nullable
}

/// Heap types classify the objects in the runtime store.
/// They may be abstract or concrete
pub type HeapType {
  AbstractFunc
  AbstractNoFunc
  AbstractExtern
  AbstractNoExtern
  AbstractAny
  AbstractEq
  AbstractI31
  AbstractStruct
  AbstractArray
  AbstractNone
  ConcreteType(Int)
}

/// Composite types are those composed from simpler types
pub type CompositeType {
  Func(List(ValueType), List(ValueType))
  Array(FieldType)
  Struct(List(FieldType))
}

/// Field types describe the components of aggregate types (arrays and
/// structs).
pub type FieldType {
  PackedType(mutable: Mutability, PackedType)
  ValueType(mutable: Mutability, ValueType)
}

/// Indicates wheter a field is mutable
pub type Mutability {
  Mutable
  Immutable
}

/// Packed types can not exist as standalone values but can be fields
pub type PackedType {
  I8
  I16
}

/// WebAssembly instructions (code)
pub type Instruction {
  // Control instructions
  Unreachable
  Nop
  Block(block_type: BlockType)
  Loop(block_type: BlockType)
  If(block_type: BlockType)
  Else
  Break(label_index: Int)
  BreakIf(label_index: Int)
  // TODO: BreakTable
  Return
  Call(function_index: Int)
  // TODO: CallIndirect
  ReturnCall(function_index: Int)
  // TODO: ReturnCallIndirect
  CallRef(type_index: Int)
  ReturnCallRef(type_index: Int)
  // TODO: BreakOn*
  End
  //
  // Reference instructions
  // TODO: ref comparisons
  StructNew(type_index: Int)
  StructNewDefault(type_index: Int)
  StructGet(type_index: Int, field_index: Int)
  // TODO: struct.get_s / struct.get_u
  StructSet(type_index: Int, field_index: Int)
  // TODO: remaining reference instructions
  //
  // Parametric instructions
  // TODO: drop, select
  //
  // Variable instructions
  LocalGet(local_index: Int)
  LocalSet(local_index: Int)
  LocalTee(local_index: Int)
  GlobalGet(global_index: Int)
  GlobalSet(global_index: Int)
  //
  // Table instructions
  // TODO: table instructions
  //
  // Memory instructions
  // TODO: memory instructions
  //
  // Numeric instructions
  // TODO: I32, F32, F64
  I64Const(value: Int)
  // TODO: BitArray?
  I64EqZ
  I64Eq
  I64NE
  I64LtS
  I64LtU
  I64GtS
  I64GtU
  I64LeS
  I64LeU
  I64GeS
  I64GeU
  I64CntLZ
  I64CntTZ
  I64PopCnt
  I64Add
  I64Sub
  I64Mul
  I64DivS
  I64DivU
  I64RemS
  I64RemU
  I64And
  I64Or
  I64Xor
  I64ShL
  I64ShRS
  I64ShLU
  I64RotL
  I64RotR
  // TODO: conversions / truncations / etc.
  //
  // Vector instructions
  // TODO: vector instructions
}

/// BlockType represents the value produced by a block (or empty)
pub type BlockType {
  BlockEmpty
  BlockValue(ValueType)
}

// --------------------------------------------------------------------------- 
// WebAssembly module & code builders
// --------------------------------------------------------------------------- 

pub fn create_module_builder(output_file_path: String) -> ModuleBuilder {
  ModuleBuilder(
    output_file_path:,
    types: [],
    next_type_index: 0,
    functions: [],
    next_function_index: 0,
  )
}

fn get_type_by_index(
  mb: ModuleBuilder,
  index: Int,
) -> Result(CompositeType, String) {
  case index < mb.next_type_index {
    True -> get_type_by_index_loop(mb.types, mb.next_type_index - index)
    False -> Error("Type index out of bounds")
  }
}

fn get_type_by_index_loop(type_groups, index) -> Result(CompositeType, String) {
  case type_groups {
    [] -> Error("Could not get type by index")
    [group, ..rest] -> {
      let length = list.length(group)
      case length > index {
        True -> get_type_by_index_loop(rest, index - length)
        False ->
          list_index(group, index - 1)
          |> result.replace_error("Could not get type by index")
      }
    }
  }
}

fn list_index(lst: List(a), idx: Int) -> Result(a, Nil) {
  case lst, idx {
    [head, ..], 0 -> Ok(head)
    [_, ..tail], n if n > 0 -> list_index(tail, n - 1)
    _, _ -> Error(Nil)
  }
}

pub fn add_type_group(
  mb: ModuleBuilder,
  group: List(CompositeType),
) -> Result(ModuleBuilder, String) {
  // TODO: validation
  Ok(
    ModuleBuilder(
      ..mb,
      types: [group, ..mb.types],
      next_type_index: mb.next_type_index + list.length(group),
    ),
  )
}

pub fn create_function(
  mb: ModuleBuilder,
  type_index: Int,
) -> Result(#(ModuleBuilder, FunctionBuilder), String) {
  case get_type_by_index(mb, type_index) {
    Ok(Func(params, results)) ->
      Ok(#(
        ModuleBuilder(
          ..mb,
          functions: [FunctionMissing(type_index)],
          next_function_index: mb.next_type_index + 1,
        ),
        FunctionBuilder(
          mb.next_function_index,
          type_index,
          params,
          results,
          [],
          list.length(params),
          [],
        ),
      ))
    _ -> Error("No func type at this index")
  }
}

pub fn add_instruction(
  fb: FunctionBuilder,
  instr: Instruction,
) -> Result(FunctionBuilder, String) {
  // TODO: validation
  Ok(FunctionBuilder(..fb, code: [instr, ..fb.code]))
}

pub fn add_local(fb: FunctionBuilder, t: ValueType) -> #(FunctionBuilder, Int) {
  #(
    FunctionBuilder(
      ..fb,
      locals: [t, ..fb.locals],
      next_local_index: fb.next_local_index + 1,
    ),
    fb.next_local_index,
  )
}

pub fn finalize_function(
  fb: FunctionBuilder,
) -> Result(FunctionDefinition, String) {
  Ok(FunctionImplementation(
    fb.type_index,
    list.reverse(fb.locals),
    list.reverse(fb.code),
  ))
}

// --------------------------------------------------------------------------- 
// WebAssembly binary encoding
// --------------------------------------------------------------------------- 

// TODO: abstract away simplifile
pub fn emit_module(mb: ModuleBuilder) -> Result(Nil, simplifile.FileError) {
  let header = <<0x00, 0x61, 0x73, 0x6d>>
  let version = <<0x01, 0x00, 0x00, 0x00>>
  let _ = simplifile.write_bits("out.wasm", <<header:bits, version:bits>>)

  // emit types
  // TODO: recursive type groups (get rid of flatten)!
  let types = list.reverse(mb.types) |> list.flatten |> list.map(encode_typedef)
  use _ <- result.try(simplifile.append_bits(
    "out.wasm",
    encode_section(section_type, encode_vector(types)),
  ))

  // emit function type declarations
  let func_types =
    list.reverse(mb.functions)
    |> list.map(fn(func) { leb128_encode_unsigned(func.type_index) })
  use _ <- result.try(simplifile.append_bits(
    "out.wasm",
    encode_section(section_func, encode_vector(func_types)),
  ))

  // emit function code
  let func_codes =
    list.reverse(mb.functions)
    |> list.filter_map(fn(func) {
      case func {
        FunctionImplementation(_type_index, locals, code) ->
          Ok(encode_function_code(locals, code))
        FunctionImport(_, _) -> Error(Nil)
        FunctionMissing(_) -> Error(Nil)
      }
    })
  use _ <- result.try(simplifile.append_bits(
    "out.wasm",
    encode_section(section_code, encode_vector(func_codes)),
  ))

  Ok(Nil)
}

fn encode_section(section: Int, data: BitArray) {
  bit_array.append(<<section:size(8)>>, prepend_byte_size(data))
}

fn prepend_byte_size(data: BitArray) -> BitArray {
  bit_array.append(leb128_encode_unsigned(bit_array.byte_size(data)), data)
}

fn encode_vector(data: List(BitArray)) -> BitArray {
  bit_array.append(
    leb128_encode_unsigned(list.length(data)),
    list.fold(data, <<>>, bit_array.append),
  )
}

fn leb128_encode_unsigned(u: Int) -> BitArray {
  case gleb128.encode_unsigned(u) {
    Ok(val) -> val
    Error(msg) -> panic as msg
  }
}

fn encode_value_type(t: ValueType) -> BitArray {
  case t {
    F32 -> code_type_i32
    F64 -> code_type_i64
    I32 -> code_type_f32
    I64 -> code_type_f64
    Ref(null, heap_type) ->
      bit_array.append(
        case null {
          NonNull -> code_ref_non_null
          Nullable -> code_ref_nullable
        },
        encode_heap_type(heap_type),
      )
    V128 -> code_type_v128
  }
}

fn encode_heap_type(h: HeapType) {
  case h {
    AbstractAny -> code_type_abs_any
    AbstractArray -> code_type_abs_array
    AbstractEq -> code_type_abs_eq
    AbstractExtern -> code_type_abs_extern
    AbstractFunc -> code_type_abs_func
    AbstractI31 -> code_type_abs_i31
    AbstractNoExtern -> code_type_abs_noextern
    AbstractNoFunc -> code_type_abs_nofunc
    AbstractNone -> code_type_abs_none
    AbstractStruct -> code_type_abs_struct
    ConcreteType(index) -> leb128_encode_unsigned(index)
  }
}

fn encode_field_types(fs: List(FieldType)) -> BitArray {
  list.map(fs, encode_field_type)
  |> encode_vector
}

fn encode_field_type(f: FieldType) -> BitArray {
  let valtype = case f {
    PackedType(_, I8) -> code_type_i8
    PackedType(_, I16) -> code_type_i16
    ValueType(_, value) -> encode_value_type(value)
  }
  let mut = case f.mutable {
    Mutable -> code_var
    Immutable -> code_const
  }
  bit_array.append(valtype, mut)
}

fn encode_result_type(rs: List(ValueType)) -> BitArray {
  list.map(rs, encode_value_type)
  |> encode_vector
}

fn encode_typedef(t: CompositeType) -> BitArray {
  case t {
    Array(element) -> [code_type_array, encode_field_type(element)]
    Func(params, results) -> [
      code_type_func,
      encode_result_type(params),
      encode_result_type(results),
    ]
    Struct(fields) -> [code_type_struct, encode_field_types(fields)]
  }
  |> bit_array.concat
}

fn encode_function_code(
  locals: List(ValueType),
  code: List(Instruction),
) -> BitArray {
  [encode_result_type(locals), ..list.map(code, encode_instruction)]
  |> bit_array.concat
  |> prepend_byte_size
}

fn encode_instruction(instr: Instruction) -> BitArray {
  case instr {
    // Control instructions
    Unreachable -> code_instr_unreachable
    Nop -> code_instr_nop
    Block(block_type) ->
      bit_array.concat([code_instr_block, encode_block_type(block_type)])
    Loop(block_type) ->
      bit_array.concat([code_instr_loop, encode_block_type(block_type)])
    If(block_type) ->
      bit_array.concat([code_instr_if, encode_block_type(block_type)])
    Else -> code_instr_else
    Break(label_index) ->
      bit_array.concat([code_instr_break, leb128_encode_unsigned(label_index)])
    BreakIf(label_index) ->
      bit_array.concat([
        code_instr_break_if,
        leb128_encode_unsigned(label_index),
      ])
    Return -> code_instr_return
    Call(function_index) ->
      bit_array.concat([code_instr_call, leb128_encode_unsigned(function_index)])
    // TODO: CallIndirect
    ReturnCall(function_index) ->
      bit_array.concat([
        code_instr_return_call,
        leb128_encode_unsigned(function_index),
      ])
    // TODO: ReturnCallIndirect
    CallRef(type_index) ->
      bit_array.concat([code_instr_call_ref, leb128_encode_unsigned(type_index)])
    ReturnCallRef(type_index) ->
      bit_array.concat([
        code_instr_return_call_ref,
        leb128_encode_unsigned(type_index),
      ])
    // TODO: BreakOn*
    End -> code_instr_end
    //
    // Reference instructions
    // TODO: ref comparisons
    StructNew(type_index) ->
      bit_array.concat([
        code_instr_struct_new,
        leb128_encode_unsigned(type_index),
      ])
    StructNewDefault(type_index) ->
      bit_array.concat([
        code_instr_struct_new_default,
        leb128_encode_unsigned(type_index),
      ])
    StructGet(type_index, field_index) ->
      bit_array.concat([
        code_instr_struct_get,
        leb128_encode_unsigned(type_index),
        leb128_encode_unsigned(field_index),
      ])
    // TODO: struct.get_s / struct.get_u
    StructSet(type_index, field_index) ->
      bit_array.concat([
        code_instr_struct_set,
        leb128_encode_unsigned(type_index),
        leb128_encode_unsigned(field_index),
      ])
    // TODO: remaining reference instructions
    //
    // Parametric instructions
    // TODO: drop, select
    //
    // Variable instructions
    LocalGet(local_index) ->
      bit_array.concat([
        code_instr_local_get,
        leb128_encode_unsigned(local_index),
      ])
    LocalSet(local_index) ->
      bit_array.concat([
        code_instr_local_set,
        leb128_encode_unsigned(local_index),
      ])
    LocalTee(local_index) ->
      bit_array.concat([
        code_instr_local_tee,
        leb128_encode_unsigned(local_index),
      ])
    GlobalGet(global_index) ->
      bit_array.concat([
        code_instr_global_get,
        leb128_encode_unsigned(global_index),
      ])
    GlobalSet(global_index) ->
      bit_array.concat([
        code_instr_global_set,
        leb128_encode_unsigned(global_index),
      ])
    //
    // Table instructions
    // TODO: table instructions
    //
    // Memory instructions
    // TODO: memory instructions
    //
    // Numeric instructions
    // TODO: I32, F32, F64
    I64Const(value) ->
      bit_array.concat([code_instr_i64_const, gleb128.encode_signed(value)])
    I64EqZ -> code_instr_i64_eq_z
    I64Eq -> code_instr_i64_eq
    I64NE -> code_instr_i64_ne
    I64LtS -> code_instr_i64_lt_s
    I64LtU -> code_instr_i64_lt_u
    I64GtS -> code_instr_i64_gt_s
    I64GtU -> code_instr_i64_gt_u
    I64LeS -> code_instr_i64_le_s
    I64LeU -> code_instr_i64_le_u
    I64GeS -> code_instr_i64_ge_s
    I64GeU -> code_instr_i64_ge_u
    I64CntLZ -> code_instr_i64_cnt_lz
    I64CntTZ -> code_instr_i64_cnt_tz
    I64PopCnt -> code_instr_i64_pop_cnt
    I64Add -> code_instr_i64_add
    I64Sub -> code_instr_i64_sub
    I64Mul -> code_instr_i64_mul
    I64DivS -> code_instr_i64_div_s
    I64DivU -> code_instr_i64_div_u
    I64RemS -> code_instr_i64_rem_s
    I64RemU -> code_instr_i64_rem_u
    I64And -> code_instr_i64_and
    I64Or -> code_instr_i64_or
    I64Xor -> code_instr_i64_xor
    I64ShL -> code_instr_i64_sh_l
    I64ShRS -> code_instr_i64_sh_r_s
    I64ShLU -> code_instr_i64_sh_r_u
    I64RotL -> code_instr_i64_rot_l
    I64RotR -> code_instr_i64_rot_r
    // TODO: conversions / truncations / etc.
    //
    // Vector instructions
    // TODO: vector instructions
  }
}

fn encode_block_type(t: BlockType) -> BitArray {
  case t {
    BlockEmpty -> code_type_empty
    BlockValue(vt) -> encode_value_type(vt)
  }
}

// --------------------------------------------------------------------------- 
// WebAssembly binary codes
// --------------------------------------------------------------------------- 

const section_custom = 0

const section_type = 1

const section_import = 2

const section_func = 3

const section_table = 4

const section_memory = 5

const section_global = 6

const section_export = 7

const section_start = 8

const section_element = 9

const section_code = 10

const section_data = 11

const code_type_i32 = <<0x7f>>

const code_type_i64 = <<0x7e>>

const code_type_f32 = <<0x7d>>

const code_type_f64 = <<0x7c>>

const code_type_v128 = <<0x7b>>

const code_type_i16 = <<0x77>>

const code_type_i8 = <<0x78>>

const code_type_abs_nofunc = <<0x73>>

const code_type_abs_noextern = <<0x72>>

const code_type_abs_none = <<0x71>>

const code_type_abs_func = <<0x70>>

const code_type_abs_extern = <<0x6f>>

const code_type_abs_any = <<0x6e>>

const code_type_abs_eq = <<0x6d>>

const code_type_abs_i31 = <<0x6c>>

const code_type_abs_struct = <<0x6b>>

const code_type_abs_array = <<0x6a>>

const code_ref_non_null = <<0x64>>

const code_ref_nullable = <<0x63>>

const code_type_func = <<0x60>>

const code_type_struct = <<0x5f>>

const code_type_array = <<0x5e>>

const code_type_rec = <<0x4e>>

const code_type_empty = <<0x40>>

const code_const = <<0x00>>

const code_var = <<0x01>>

const code_instr_unreachable = <<0x00>>

const code_instr_nop = <<0x01>>

const code_instr_block = <<0x02>>

const code_instr_loop = <<0x03>>

const code_instr_if = <<0x04>>

const code_instr_else = <<0x05>>

const code_instr_end = <<0x0b>>

const code_instr_break = <<0x0c>>

const code_instr_break_if = <<0x0d>>

const code_instr_break_table = <<0x0e>>

const code_instr_return = <<0x0f>>

const code_instr_call = <<0x10>>

const code_instr_call_indirect = <<0x11>>

const code_instr_return_call = <<0x12>>

const code_instr_return_call_indirect = <<0x13>>

const code_instr_call_ref = <<0x14>>

const code_instr_return_call_ref = <<0x15>>

const code_instr_local_get = <<0x20>>

const code_instr_local_set = <<0x21>>

const code_instr_local_tee = <<0x22>>

const code_instr_global_get = <<0x23>>

const code_instr_global_set = <<0x24>>

const code_instr_i32_const = <<0x41>>

const code_instr_i64_const = <<0x42>>

const code_instr_f32_const = <<0x43>>

const code_instr_f64_const = <<0x44>>

const code_instr_i64_eq_z = <<0x50>>

const code_instr_i64_eq = <<0x51>>

const code_instr_i64_ne = <<0x52>>

const code_instr_i64_lt_s = <<0x53>>

const code_instr_i64_lt_u = <<0x54>>

const code_instr_i64_gt_s = <<0x55>>

const code_instr_i64_gt_u = <<0x56>>

const code_instr_i64_le_s = <<0x57>>

const code_instr_i64_le_u = <<0x58>>

const code_instr_i64_ge_s = <<0x59>>

const code_instr_i64_ge_u = <<0x5a>>

const code_instr_i64_cnt_lz = <<0x79>>

const code_instr_i64_cnt_tz = <<0x7a>>

const code_instr_i64_pop_cnt = <<0x7b>>

const code_instr_i64_add = <<0x7c>>

const code_instr_i64_sub = <<0x7d>>

const code_instr_i64_mul = <<0x7e>>

const code_instr_i64_div_s = <<0x7f>>

const code_instr_i64_div_u = <<0x80>>

const code_instr_i64_rem_s = <<0x81>>

const code_instr_i64_rem_u = <<0x82>>

const code_instr_i64_and = <<0x83>>

const code_instr_i64_or = <<0x84>>

const code_instr_i64_xor = <<0x85>>

const code_instr_i64_sh_l = <<0x85>>

const code_instr_i64_sh_r_s = <<0x86>>

const code_instr_i64_sh_r_u = <<0x87>>

const code_instr_i64_rot_l = <<0x88>>

const code_instr_i64_rot_r = <<0x8a>>

const code_instr_struct_new = <<0xfb, 0x00>>

const code_instr_struct_new_default = <<0xfb, 0x01>>

const code_instr_struct_get = <<0xfb, 0x02>>

const code_instr_struct_set = <<0xfb, 0x05>>
