import glance.{type Expression, type Type}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None}
import gleam/result
import gleam/set.{type Set}
import gloml
import simplifile.{type FileError}
import wat.{
  type WatDefinition, type WatExpression, type WatFunctionType, type WatType,
}

pub type ErrorLocation {
  AtExpression(Expression)
  AtStatement(glance.Statement)
  AtPattern(glance.Pattern)
}

pub type CompilerError {
  FileError(error: FileError)
  PackageTomlError(error: gloml.DecodeError)
  CircularDependencyError
  ParseError(error: glance.Error)
  SyntaxError(error: String)
  ReferenceError(name: String)
  TypeError(at: ErrorLocation, expected_type: Type, actual_type: Type)
  NotAFunctionError(at: ErrorLocation)
  ArityError(at: ErrorLocation, expected_number: Int, actual_number: Int)
  PositionalArgsAfterLabeledArgsError(at: ErrorLocation)
  NoSuchFieldError(at: ErrorLocation, field: String)
  TypeArityError(type_: Type, expected_number: Int, actual_number: Int)
  AnotherTypeError(String)
}

pub type FunctionImport {
  FunctionImport(module: String, name: String, function_type: WatFunctionType)
}

pub type GlobalState {
  GlobalState(
    defined_types: Dict(String, WatType),
    modules: Dict(String, String),
    defined_functions: Dict(String, Type),
    imported_functions: Dict(String, FunctionImport),
    compiled_functions: List(WatDefinition),
    exported_functions: Set(String),
  )
}

pub type LocalState {
  LocalState(
    global: GlobalState,
    in_scope_variables: Dict(String, String),
    params: Dict(String, Type),
    locals: Dict(String, Type),
    variable_types: Dict(String, Type),
    anon_fn_prefix: String,
    anon_fn_count: Int,
    case_count: Int,
  )
}

pub type ExpressionState {
  ExpressionState(
    local: LocalState,
    compiled: WatExpression,
    type_: Type,
    tail: Bool,
  )
}

pub fn new_global_state() {
  GlobalState(dict.new(), dict.new(), dict.new(), dict.new(), [], set.new())
}

pub fn new_local_state(global: GlobalState, prefix: String) -> LocalState {
  LocalState(
    global,
    dict.new(),
    dict.new(),
    dict.new(),
    dict.new(),
    prefix,
    0,
    0,
  )
}

pub fn with_params(state: LocalState, params: List(#(String, Type))) {
  LocalState(
    ..state,
    params: dict.merge(state.params, dict.from_list(params)),
    in_scope_variables: dict.merge(
      state.in_scope_variables,
      dict.from_list(list.map(params, fn(param) { #(param.0, param.0) })),
    ),
  )
}

pub fn with_locals(state: LocalState, locals: List(#(String, Type))) {
  // TODO: name mangling for uniqueness
  LocalState(
    ..state,
    locals: dict.merge(state.locals, dict.from_list(locals)),
    in_scope_variables: dict.merge(
      state.in_scope_variables,
      dict.from_list(list.map(locals, fn(param) { #(param.0, param.0) })),
    ),
  )
}

pub fn get_variable(
  state: LocalState,
  variable_name: String,
) -> Result(#(String, Type), CompilerError) {
  dict.get(state.in_scope_variables, variable_name)
  |> result.try(fn(local_name) {
    result.or(
      dict.get(state.locals, local_name),
      dict.get(state.params, local_name),
    )
    |> result.map(fn(type_) { #(local_name, type_) })
  })
  |> result.map_error(fn(_) { ReferenceError(variable_name) })
}

pub const int_type = glance.NamedType("Int", None, [])

pub const float_type = glance.NamedType("Float", None, [])

pub fn list_type(elem_type: Type) -> Type {
  glance.NamedType("List", None, [elem_type])
}

pub fn closure_type_def(encoded_signature: String) -> #(String, wat.WatType) {
  #(
    "closure:" <> encoded_signature,
    wat.Struct([
      wat.WatVariableDefinition("context", wat.NullRef(wat.Direct(wat.Any))),
      wat.WatVariableDefinition(
        "function",
        wat.Ref(wat.Id("function:" <> encoded_signature)),
      ),
    ]),
  )
}

pub const import_int_box = FunctionImport(
  "gleam",
  "int_box",
  wat.WatFunctionType([wat.Int64], wat.Ref(wat.Id("gleam/Int"))),
)

// TODO: why support (ref any) here? For everything else we cast before calling.
pub const import_int_unbox = FunctionImport(
  "gleam",
  "int_unbox",
  wat.WatFunctionType([wat.Ref(wat.Direct(wat.Any))], wat.Int64),
)

pub const import_float_box = FunctionImport(
  "gleam",
  "float_box",
  wat.WatFunctionType([wat.Float64], wat.Ref(wat.Id("gleam/Float"))),
)

pub const import_float_unbox = FunctionImport(
  "gleam",
  "float_unbox",
  wat.WatFunctionType([wat.Ref(wat.Direct(wat.Any))], wat.Float64),
)

pub const import_list_is_empty = FunctionImport(
  "gleam",
  "list_is_empty",
  wat.WatFunctionType([wat.Ref(wat.Id("gleam/List"))], wat.Int32),
)

pub const import_list_head = FunctionImport(
  "gleam",
  "list_head",
  wat.WatFunctionType(
    [wat.Ref(wat.Id("gleam/List"))],
    wat.Ref(wat.Direct(wat.Any)),
  ),
)

pub const import_list_tail = FunctionImport(
  "gleam",
  "list_tail",
  wat.WatFunctionType(
    [wat.Ref(wat.Id("gleam/List"))],
    wat.Ref(wat.Id("gleam/List")),
  ),
)

pub fn import_functions(state: LocalState, functions: List(FunctionImport)) {
  let imports =
    functions
    |> list.map(fn(fun) { #(fun.module <> "/" <> fun.name, fun) })
    |> dict.from_list
  LocalState(
    ..state,
    global: GlobalState(
      ..state.global,
      imported_functions: dict.merge(state.global.imported_functions, imports),
    ),
  )
}
