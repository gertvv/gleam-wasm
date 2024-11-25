import compiler.{ExpressionState, GlobalState, LocalState}
import expression_compiler.{compile_expression}
import glance
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import wat

pub fn compile_int_test() {
  let local_state =
    compiler.new_local_state(compiler.new_global_state(), "prefix")
  compile_expression(glance.Int("42"), local_state, False)
  |> should.equal(
    Ok(ExpressionState(
      local_state,
      wat.Int64Const(42),
      compiler.int_type,
      False,
    )),
  )
  compile_expression(glance.Int("-1"), local_state, False)
  |> should.equal(
    Ok(ExpressionState(
      local_state,
      wat.Int64Const(-1),
      compiler.int_type,
      False,
    )),
  )
  compile_expression(glance.Int("#"), local_state, False)
  |> should.equal(Error(compiler.SyntaxError("Invalid Int value: #")))

  compile_expression(glance.Int("42"), local_state, True)
  |> should.equal(
    Ok(ExpressionState(
      local_state,
      wat.Return(wat.Int64Const(42)),
      compiler.int_type,
      True,
    )),
  )
}

fn state_with_functions_and_locals(functions, locals) {
  compiler.with_locals(
    compiler.new_local_state(
      GlobalState(
        ..compiler.new_global_state(),
        defined_functions: dict.from_list(functions),
      ),
      "prefix",
    ),
    locals,
  )
}

pub fn compile_variable_test() {
  let fn1_type = glance.FunctionType([compiler.int_type], compiler.int_type)
  let functions = [#("fn1", fn1_type), #("am1", fn1_type)]
  let locals = [#("l1", compiler.int_type), #("am1", compiler.int_type)]
  let local_state = state_with_functions_and_locals(functions, locals)
  compile_expression(glance.Variable("fn1"), local_state, False)
  |> should.equal(
    Ok(ExpressionState(local_state, wat.RefFunc("fn1"), fn1_type, False)),
  )
  compile_expression(glance.Variable("l1"), local_state, False)
  |> should.equal(
    Ok(ExpressionState(
      local_state,
      wat.LocalGet("l1"),
      compiler.int_type,
      False,
    )),
  )
  compile_expression(glance.Variable("l1"), local_state, False)
  |> should.equal(
    Ok(ExpressionState(
      local_state,
      wat.LocalGet("l1"),
      compiler.int_type,
      False,
    )),
  )
  compile_expression(glance.Variable("am1"), local_state, False)
  |> should.equal(
    Ok(ExpressionState(
      local_state,
      wat.LocalGet("am1"),
      compiler.int_type,
      False,
    )),
  )
  compile_expression(glance.Variable("missing"), local_state, False)
  |> should.equal(Error(compiler.ReferenceError("missing")))
  compile_expression(glance.Variable("am1"), local_state, True)
  |> should.equal(
    Ok(ExpressionState(
      local_state,
      wat.Return(wat.LocalGet("am1")),
      compiler.int_type,
      True,
    )),
  )
}

pub fn compile_add_variables_test() {
  let locals = [
    #("a", compiler.int_type),
    #("b", compiler.int_type),
    #("c", compiler.float_type),
  ]
  let local_state = state_with_functions_and_locals([], locals)
  compile_expression(
    glance.BinaryOperator(
      glance.AddInt,
      glance.Variable("a"),
      glance.Variable("b"),
    ),
    local_state,
    False,
  )
  |> should.equal(
    Ok(ExpressionState(
      local_state,
      wat.Int64Add(wat.LocalGet("a"), wat.LocalGet("b")),
      compiler.int_type,
      False,
    )),
  )
  compile_expression(
    glance.BinaryOperator(
      glance.AddInt,
      glance.Variable("a"),
      glance.Variable("b"),
    ),
    local_state,
    True,
  )
  |> should.equal(
    Ok(ExpressionState(
      local_state,
      wat.Return(wat.Int64Add(wat.LocalGet("a"), wat.LocalGet("b"))),
      compiler.int_type,
      True,
    )),
  )
  compile_expression(
    glance.BinaryOperator(
      glance.AddInt,
      glance.Variable("a"),
      glance.Variable("d"),
    ),
    local_state,
    False,
  )
  |> should.equal(Error(compiler.ReferenceError("d")))
  compile_expression(
    glance.BinaryOperator(
      glance.AddInt,
      glance.Variable("a"),
      glance.Variable("c"),
    ),
    local_state,
    False,
  )
  |> should.equal(
    Error(compiler.TypeError(
      compiler.AtExpression(glance.Variable("c")),
      compiler.int_type,
      compiler.float_type,
    )),
  )
}

pub fn compile_function_call_simple_test() {
  // References to functions (param, let, return values) are encoded
  // as closures and called using `call.ref`. Global functions are called
  // directly using `call`.
  let fn_type = glance.FunctionType([compiler.int_type], compiler.float_type)
  let functions = [#("fn1", fn_type)]
  let locals = [#("fn2", fn_type), #("i", compiler.int_type)]
  let local_state = state_with_functions_and_locals(functions, locals)
  compile_expression(
    glance.Call(glance.Variable("fn1"), [glance.Field(None, glance.Int("42"))]),
    local_state,
    False,
  )
  |> should.equal(
    Ok(ExpressionState(
      local_state,
      wat.Call("fn1", [wat.Int64Const(42)]),
      compiler.float_type,
      False,
    )),
  )
  compile_expression(
    glance.Call(glance.Variable("fn1"), [glance.Field(None, glance.Int("42"))]),
    local_state,
    True,
  )
  |> should.equal(
    Ok(ExpressionState(
      local_state,
      wat.Return(wat.Call("fn1", [wat.Int64Const(42)])),
      compiler.float_type,
      True,
    )),
  )
  compile_expression(
    glance.Call(glance.Variable("fn2"), [glance.Field(None, glance.Int("42"))]),
    local_state,
    False,
  )
  |> should.equal(
    Ok(ExpressionState(
      local_state,
      wat.CallRef(
        "function:Int:Float",
        [
          wat.StructGet("closure:Int:Float", "context", wat.LocalGet("fn2")),
          wat.Int64Const(42),
        ],
        wat.StructGet("closure:Int:Float", "function", wat.LocalGet("fn2")),
      ),
      compiler.float_type,
      False,
    )),
  )
  compile_expression(
    glance.Call(glance.Variable("fn1"), [
      glance.Field(None, glance.Float("3.14")),
    ]),
    local_state,
    False,
  )
  |> should.equal(
    Error(compiler.TypeError(
      compiler.AtExpression(glance.Float("3.14")),
      compiler.int_type,
      compiler.float_type,
    )),
  )
  compile_expression(
    glance.Call(glance.Variable("i"), [glance.Field(None, glance.Float("3.14"))]),
    local_state,
    False,
  )
  |> should.equal(
    Error(
      compiler.NotAFunctionError(compiler.AtExpression(glance.Variable("i"))),
    ),
  )
  compile_expression(
    glance.Call(glance.Variable("j"), [glance.Field(None, glance.Float("3.14"))]),
    local_state,
    False,
  )
  |> should.equal(Error(compiler.ReferenceError("j")))
  compile_expression(
    glance.Call(glance.Variable("fn1"), [
      glance.Field(None, glance.Int("42")),
      glance.Field(None, glance.Int("13")),
    ]),
    local_state,
    False,
  )
  |> should.equal(
    Error(compiler.ArityError(
      compiler.AtExpression(glance.Variable("fn1")),
      1,
      2,
    )),
  )
}

pub fn compile_function_call_generic_test() {
  let tv_a = glance.VariableType("a")
  let fn1_type = glance.FunctionType([tv_a], tv_a)
  let fn2_type = glance.FunctionType([compiler.list_type(tv_a)], tv_a)
  let functions = [#("fn1", fn1_type), #("fn2", fn2_type)]
  let locals = [#("list", compiler.list_type(compiler.int_type))]
  let local_state = state_with_functions_and_locals(functions, locals)
  compile_expression(
    glance.Call(glance.Variable("fn1"), [glance.Field(None, glance.Int("42"))]),
    local_state,
    False,
  )
  |> should.equal(
    Ok(ExpressionState(
      compiler.import_functions(local_state, [
        compiler.import_int_box,
        compiler.import_int_unbox,
      ]),
      wat.Call("gleam/int_unbox", [
        wat.Call("fn1", [wat.Call("gleam/int_box", [wat.Int64Const(42)])]),
      ]),
      compiler.int_type,
      False,
    )),
  )
  compile_expression(
    glance.Call(glance.Variable("fn2"), [
      glance.Field(None, glance.Variable("list")),
    ]),
    local_state,
    False,
  )
  |> should.equal(
    Ok(ExpressionState(
      compiler.import_functions(local_state, [compiler.import_int_unbox]),
      wat.Call("gleam/int_unbox", [wat.Call("fn2", [wat.LocalGet("list")])]),
      compiler.int_type,
      False,
    )),
  )
}

pub fn compile_function_call_pass_generic_function_test() {
  let tv_a = glance.VariableType("a")
  let reduce_type =
    glance.FunctionType(
      [compiler.list_type(tv_a), glance.FunctionType([tv_a, tv_a], tv_a)],
      tv_a,
    )
  let add_type =
    glance.FunctionType(
      [compiler.int_type, compiler.int_type],
      compiler.int_type,
    )
  let add_signature = "Int.Int:Int"
  // stub types since we don't use them for anything
  let add_types = [
    #("function:" <> add_signature, wat.Any),
    #("closure:" <> add_signature, wat.Any),
  ]
  let functions = [#("reduce", reduce_type), #("add", add_type)]
  let locals = [
    #("list", compiler.list_type(compiler.int_type)),
    #("add_ref", add_type),
  ]
  let reducer_signature = "0.0:0"
  let reducer_fn_type =
    wat.Function(wat.WatFunctionType(
      [
        wat.NullRef(wat.Direct(wat.Any)),
        wat.Ref(wat.Direct(wat.Any)),
        wat.Ref(wat.Direct(wat.Any)),
      ],
      wat.Ref(wat.Direct(wat.Any)),
    ))
  let reducer_cl_type =
    wat.Struct([
      wat.WatVariableDefinition("context", wat.NullRef(wat.Direct(wat.Any))),
      wat.WatVariableDefinition(
        "function",
        wat.Ref(wat.Id("function:" <> reducer_signature)),
      ),
    ])
  let reducer_types = [
    #("function:" <> reducer_signature, reducer_fn_type),
    #("closure:" <> reducer_signature, reducer_cl_type),
  ]
  let local_state = state_with_functions_and_locals(functions, locals)
  let local_state =
    LocalState(
      ..local_state,
      global: GlobalState(
        ..local_state.global,
        defined_types: dict.from_list(add_types),
      ),
    )
  let wrapper_global =
    wat.FunctionDefinition(
      "@prefix_anon_0",
      [
        wat.WatVariableDefinition("@ctx_any", wat.NullRef(wat.Direct(wat.Any))),
        wat.WatVariableDefinition("p0", wat.Ref(wat.Direct(wat.Any))),
        wat.WatVariableDefinition("p1", wat.Ref(wat.Direct(wat.Any))),
      ],
      wat.Ref(wat.Direct(wat.Any)),
      [],
      [
        wat.Call("gleam/int_box", [
          wat.Call("add", [
            wat.Call("gleam/int_unbox", [wat.LocalGet("p0")]),
            wat.Call("gleam/int_unbox", [wat.LocalGet("p1")]),
          ]),
        ]),
      ],
    )
  let wrapper_closure =
    wat.FunctionDefinition(
      "@prefix_anon_0",
      [
        wat.WatVariableDefinition("@ctx_any", wat.NullRef(wat.Direct(wat.Any))),
        wat.WatVariableDefinition("p0", wat.Ref(wat.Direct(wat.Any))),
        wat.WatVariableDefinition("p1", wat.Ref(wat.Direct(wat.Any))),
      ],
      wat.Ref(wat.Direct(wat.Any)),
      [
        wat.WatVariableDefinition(
          "@ctx",
          wat.Ref(wat.Id("closure:" <> add_signature)),
        ),
      ],
      [
        wat.LocalSet(
          "@ctx",
          wat.RefCast(
            "closure:" <> add_signature,
            wat.RefAsNonNull(wat.LocalGet("@ctx_any")),
          ),
        ),
        wat.Call("gleam/int_box", [
          wat.CallRef(
            "function:" <> add_signature,
            [
              wat.StructGet(
                "closure:" <> add_signature,
                "context",
                wat.LocalGet("@ctx"),
              ),
              wat.Call("gleam/int_unbox", [wat.LocalGet("p0")]),
              wat.Call("gleam/int_unbox", [wat.LocalGet("p1")]),
            ],
            wat.StructGet(
              "closure:" <> add_signature,
              "function",
              wat.LocalGet("@ctx"),
            ),
          ),
        ]),
      ],
    )

  compile_expression(
    glance.Call(glance.Variable("reduce"), [
      glance.Field(None, glance.Variable("list")),
      glance.Field(None, glance.Variable("add")),
    ]),
    local_state,
    False,
  )
  |> should.equal(
    Ok(ExpressionState(
      compiler.LocalState(
        ..local_state,
        anon_fn_count: 1,
        global: compiler.GlobalState(
          ..compiler.import_functions(local_state, [
            compiler.import_int_unbox,
            compiler.import_int_box,
          ]).global,
          defined_types: dict.from_list(list.append(add_types, reducer_types)),
          compiled_functions: [wrapper_global],
        ),
      ),
      wat.Call("gleam/int_unbox", [
        wat.Call("reduce", [
          wat.LocalGet("list"),
          wat.StructNew("closure:" <> reducer_signature, [
            wat.RefNull(wat.Direct(wat.Any)),
            wat.RefFunc("@prefix_anon_0"),
          ]),
        ]),
      ]),
      compiler.int_type,
      False,
    )),
  )

  compile_expression(
    glance.Call(glance.Variable("reduce"), [
      glance.Field(None, glance.Variable("list")),
      glance.Field(None, glance.Variable("add_ref")),
    ]),
    local_state,
    False,
  )
  |> should.equal(
    Ok(ExpressionState(
      compiler.LocalState(
        ..local_state,
        anon_fn_count: 1,
        global: compiler.GlobalState(
          ..compiler.import_functions(local_state, [
            compiler.import_int_unbox,
            compiler.import_int_box,
          ]).global,
          compiled_functions: [wrapper_closure],
          defined_types: dict.from_list(list.append(add_types, reducer_types)),
        ),
      ),
      wat.Call("gleam/int_unbox", [
        wat.Call("reduce", [
          wat.LocalGet("list"),
          wat.StructNew("closure:" <> reducer_signature, [
            wat.LocalGet("add_ref"),
            wat.RefFunc("@prefix_anon_0"),
          ]),
        ]),
      ]),
      compiler.int_type,
      False,
    )),
  )
}

pub fn compile_function_call_pass_generic_function_concrete_args_test() {
  let tv_a = glance.VariableType("a")
  let list_a = compiler.list_type(tv_a)
  let list_int = compiler.list_type(compiler.int_type)
  let receiver_fn_type =
    glance.FunctionType(
      [glance.FunctionType([list_a, compiler.int_type, tv_a], list_a)],
      list_a,
    )
  let passed_fn_type =
    glance.FunctionType(
      [list_int, compiler.int_type, compiler.int_type],
      list_int,
    )
  let functions = [
    #("receiver_fn", receiver_fn_type),
    #("passed_fn", passed_fn_type),
  ]
  let local_state = state_with_functions_and_locals(functions, [])

  let wat_anon_fn_type =
    wat.Function(wat.WatFunctionType(
      [
        wat.NullRef(wat.Direct(wat.Any)),
        wat.Ref(wat.Id("gleam/List")),
        wat.Int64,
        wat.Ref(wat.Direct(wat.Any)),
      ],
      wat.Ref(wat.Id("gleam/List")),
    ))
  let wrapper =
    wat.FunctionDefinition(
      "@prefix_anon_0",
      [
        wat.WatVariableDefinition("@ctx_any", wat.NullRef(wat.Direct(wat.Any))),
        wat.WatVariableDefinition("p0", wat.Ref(wat.Id("gleam/List"))),
        wat.WatVariableDefinition("p1", wat.Int64),
        wat.WatVariableDefinition("p2", wat.Ref(wat.Direct(wat.Any))),
      ],
      wat.Ref(wat.Id("gleam/List")),
      [],
      [
        wat.Call("passed_fn", [
          wat.LocalGet("p0"),
          wat.LocalGet("p1"),
          wat.Call("gleam/int_unbox", [wat.LocalGet("p2")]),
        ]),
      ],
    )

  compile_expression(
    glance.Call(glance.Variable("receiver_fn"), [
      glance.Field(None, glance.Variable("passed_fn")),
    ]),
    local_state,
    False,
  )
  |> should.equal(
    Ok(ExpressionState(
      compiler.LocalState(
        ..local_state,
        anon_fn_count: 1,
        global: compiler.GlobalState(
          ..compiler.import_functions(local_state, [compiler.import_int_unbox]).global,
          compiled_functions: [wrapper],
          defined_types: dict.from_list([
            #("function:List<0>.Int.0:List<0>", wat_anon_fn_type),
            #(
              "closure:List<0>.Int.0:List<0>",
              wat.Struct([
                wat.WatVariableDefinition(
                  "context",
                  wat.NullRef(wat.Direct(wat.Any)),
                ),
                wat.WatVariableDefinition(
                  "function",
                  wat.Ref(wat.Id("function:List<0>.Int.0:List<0>")),
                ),
              ]),
            ),
          ]),
        ),
      ),
      wat.Call("receiver_fn", [
        wat.StructNew("closure:List<0>.Int.0:List<0>", [
          wat.RefNull(wat.Direct(wat.Any)),
          wat.RefFunc("@prefix_anon_0"),
        ]),
      ]),
      list_a,
      False,
    )),
  )
}

pub fn compile_case_test() {
  let locals = [
    #("lst", compiler.list_type(compiler.int_type)),
    #("i", compiler.int_type),
  ]
  let local_state = state_with_functions_and_locals([], locals)

  // inconsistent clause types should error
  compile_expression(
    glance.Case([glance.Variable("lst")], [
      glance.Clause(
        [
          [
            glance.PatternList(
              [glance.PatternDiscard("head")],
              Some(glance.PatternDiscard("tail")),
            ),
          ],
        ],
        None,
        glance.Int("0"),
      ),
      glance.Clause([[glance.PatternList([], None)]], None, glance.Float("1.0")),
    ]),
    local_state,
    False,
  )
  |> should.equal(
    Error(compiler.TypeError(
      compiler.AtExpression(glance.Int("0")),
      compiler.float_type,
      compiler.int_type,
    )),
  )

  // TODO: for now all list-related functions are imported
  // when there are any non-trivial list patterns - reconsider?
  let with_list_functions =
    compiler.import_functions(local_state, [
      compiler.import_list_is_empty,
      compiler.import_list_head,
      compiler.import_list_tail,
    ])

  // single subject, no assignments
  compile_expression(
    glance.Case([glance.Variable("lst")], [
      glance.Clause(
        [
          [
            glance.PatternList(
              [glance.PatternDiscard("head")],
              Some(glance.PatternDiscard("tail")),
            ),
          ],
        ],
        None,
        glance.Int("0"),
      ),
      glance.Clause([[glance.PatternList([], None)]], None, glance.Int("1")),
    ]),
    local_state,
    False,
  )
  |> should.equal(
    Ok(ExpressionState(
      compiler.with_locals(LocalState(..with_list_functions, case_count: 1), [
        #("@case_0_0", compiler.list_type(compiler.int_type)),
      ]),
      wat.Block([
        wat.LocalSet("@case_0_0", wat.LocalGet("lst")),
        wat.If(
          wat.Int32EqualZero(
            wat.Call("gleam/list_is_empty", [wat.LocalGet("@case_0_0")]),
          ),
          wat.Int64Const(0),
          Some(wat.If(
            wat.Call("gleam/list_is_empty", [wat.LocalGet("@case_0_0")]),
            wat.Int64Const(1),
            Some(wat.Unreachable),
          )),
        ),
      ]),
      compiler.int_type,
      False,
    )),
  )

  // single subject, no assignments, tail position
  compile_expression(
    glance.Case([glance.Variable("lst")], [
      glance.Clause(
        [
          [
            glance.PatternList(
              [glance.PatternDiscard("head")],
              Some(glance.PatternDiscard("tail")),
            ),
          ],
        ],
        None,
        glance.Int("0"),
      ),
      glance.Clause([[glance.PatternList([], None)]], None, glance.Int("1")),
    ]),
    local_state,
    True,
  )
  |> should.equal(
    Ok(ExpressionState(
      compiler.with_locals(LocalState(..with_list_functions, case_count: 1), [
        #("@case_0_0", compiler.list_type(compiler.int_type)),
      ]),
      wat.Block([
        wat.LocalSet("@case_0_0", wat.LocalGet("lst")),
        wat.If(
          wat.Int32EqualZero(
            wat.Call("gleam/list_is_empty", [wat.LocalGet("@case_0_0")]),
          ),
          wat.Return(wat.Int64Const(0)),
          Some(wat.If(
            wat.Call("gleam/list_is_empty", [wat.LocalGet("@case_0_0")]),
            wat.Return(wat.Int64Const(1)),
            Some(wat.Unreachable),
          )),
        ),
      ]),
      compiler.int_type,
      False,
    )),
  )

  // multiple subjects
  compile_expression(
    glance.Case([glance.Variable("i"), glance.Variable("lst")], [
      glance.Clause(
        [
          [
            glance.PatternInt("7"),
            glance.PatternList(
              [glance.PatternDiscard("head")],
              Some(glance.PatternDiscard("tail")),
            ),
          ],
        ],
        None,
        glance.Int("1"),
      ),
      glance.Clause(
        [[glance.PatternDiscard(""), glance.PatternDiscard("")]],
        None,
        glance.Int("0"),
      ),
    ]),
    local_state,
    False,
  )
  |> should.equal(
    Ok(ExpressionState(
      compiler.with_locals(LocalState(..with_list_functions, case_count: 1), [
        #("@case_0_0", compiler.int_type),
        #("@case_0_1", compiler.list_type(compiler.int_type)),
      ]),
      wat.Block([
        wat.LocalSet("@case_0_0", wat.LocalGet("i")),
        wat.LocalSet("@case_0_1", wat.LocalGet("lst")),
        wat.If(
          wat.If(
            wat.Int64Equal(wat.LocalGet("@case_0_0"), wat.Int64Const(7)),
            wat.Int32EqualZero(
              wat.Call("gleam/list_is_empty", [wat.LocalGet("@case_0_1")]),
            ),
            Some(wat.Int32Const(0)),
          ),
          wat.Int64Const(1),
          Some(wat.If(
            wat.Int32Const(1),
            wat.Int64Const(0),
            Some(wat.Unreachable),
          )),
        ),
      ]),
      compiler.int_type,
      False,
    )),
  )

  // Assignments via pattern
  // TODO: need to review how variables are treated in general as let statements
  // can override eachother. "head" here needs to be mangled (at least in the general case). 
  compile_expression(
    glance.Case([glance.Variable("lst")], [
      glance.Clause(
        [
          [
            glance.PatternList(
              [glance.PatternVariable("head")],
              Some(glance.PatternDiscard("tail")),
            ),
          ],
        ],
        None,
        glance.Variable("head"),
      ),
      glance.Clause([[glance.PatternList([], None)]], None, glance.Int("0")),
    ]),
    local_state,
    False,
  )
  |> should.equal(
    Ok(ExpressionState(
      compiler.with_locals(LocalState(..with_list_functions, case_count: 1), [
        #("@case_0_0", compiler.list_type(compiler.int_type)),
        #("head", compiler.int_type),
      ]),
      wat.Block([
        wat.LocalSet("@case_0_0", wat.LocalGet("lst")),
        wat.If(
          wat.Int32EqualZero(
            wat.Call("gleam/list_is_empty", [wat.LocalGet("@case_0_0")]),
          ),
          wat.Block([
            wat.LocalSet(
              "head",
              wat.Call("gleam/list_head", [wat.LocalGet("@case_0_0")]),
            ),
            wat.LocalGet("head"),
          ]),
          Some(wat.If(
            wat.Call("gleam/list_is_empty", [wat.LocalGet("@case_0_0")]),
            wat.Int64Const(0),
            Some(wat.Unreachable),
          )),
        ),
      ]),
      compiler.int_type,
      False,
    )),
  )

  compile_expression(
    glance.Case([glance.Variable("lst")], [
      glance.Clause(
        [
          [
            glance.PatternList(
              [glance.PatternDiscard("head")],
              Some(glance.PatternVariable("tail")),
            ),
          ],
        ],
        None,
        glance.Variable("tail"),
      ),
      glance.Clause(
        [[glance.PatternList([], None)]],
        None,
        glance.List([], None),
      ),
    ]),
    local_state,
    False,
  )
  |> should.equal(
    Ok(ExpressionState(
      compiler.with_locals(LocalState(..with_list_functions, case_count: 1), [
        #("@case_0_0", compiler.list_type(compiler.int_type)),
        #("tail", compiler.list_type(compiler.int_type)),
      ]),
      wat.Block([
        wat.LocalSet("@case_0_0", wat.LocalGet("lst")),
        wat.If(
          wat.Int32EqualZero(
            wat.Call("gleam/list_is_empty", [wat.LocalGet("@case_0_0")]),
          ),
          wat.Block([
            wat.LocalSet(
              "tail",
              wat.Call("gleam/list_tail", [wat.LocalGet("@case_0_0")]),
            ),
            wat.LocalGet("tail"),
          ]),
          Some(wat.If(
            wat.Call("gleam/list_is_empty", [wat.LocalGet("@case_0_0")]),
            wat.Call("gleam/list_empty", []),
            Some(wat.Unreachable),
          )),
        ),
      ]),
      compiler.list_type(compiler.int_type),
      False,
    )),
  )
  // TODO: test nested case expressions
}
