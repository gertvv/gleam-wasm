import analysis
import compiler
import glance
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/result
import gleam/set
import gleeunit/should
import pprint
import project

fn empty_module_internals(package_name, module_path) {
  let project =
    project.Project(package_name, "javascript", dict.new(), fn(_a, _b) {
      Error(compiler.ReferenceError("test"))
    })
  let location = project.SourceLocation(package_name, module_path)
  analysis.ModuleInternals(
    project,
    location,
    dict.new(),
    dict.new(),
    dict.new(),
    dict.new(),
    set.new(),
    set.new(),
  )
}

pub fn pair_args_test() {
  let at = compiler.AtExpression(glance.String("boo"))
  let named = fn(str, val) { glance.Field(Some(str), val) }
  let positional = fn(val) { glance.Field(None, val) }
  let signature = fn(pos, lab) {
    analysis.FunctionSignature(
      "a_function",
      list.map(pos, analysis.TypeVariable),
      list.map(lab, fn(name) {
        analysis.Labeled(name, analysis.TypeVariable(name))
      }),
      analysis.int_type,
    )
  }
  let tv = analysis.TypeVariable

  // More actual than expected should error
  analysis.pair_args(
    expected: signature(["a"], []),
    actual: [positional("b"), positional("c")],
    at:,
  )
  |> should.equal(Error(compiler.ArityError(at, 1, 2)))

  // Fewer actual than expected returns None
  analysis.pair_args(
    expected: signature(["a", "b"], []),
    actual: [positional("c")],
    at:,
  )
  |> should.equal(Ok([#(tv("a"), Some("c")), #(tv("b"), None)]))

  // Labeled args may be provided as positional
  analysis.pair_args(
    expected: signature([], ["a", "b"]),
    actual: [positional("c"), positional("d")],
    at:,
  )
  |> should.equal(Ok([#(tv("a"), Some("c")), #(tv("b"), Some("d"))]))

  // Positional args after labeled not allowed
  analysis.pair_args(
    expected: signature([], ["a", "b"]),
    actual: [named("a", "c"), positional("d")],
    at:,
  )
  |> should.equal(Error(compiler.PositionalArgsAfterLabeledArgsError(at)))

  // Unexpected labeled args not allowed
  analysis.pair_args(
    expected: signature([], ["a", "b"]),
    actual: [named("c", "c")],
    at:,
  )
  |> should.equal(Error(compiler.NoSuchFieldError(at, "c")))

  // Labeled args should get paired
  analysis.pair_args(
    expected: signature([], ["a", "b"]),
    actual: [named("b", "c"), named("a", "d")],
    at:,
  )
  |> should.equal(Ok([#(tv("a"), Some("d")), #(tv("b"), Some("c"))]))
}

pub fn resolve_basic_types_test() {
  analysis.resolve_type(
    empty_module_internals("foo", "bar"),
    glance.NamedType("Int", None, []),
  )
  |> should.equal(Ok(analysis.int_type))

  analysis.resolve_type(
    empty_module_internals("foo", "bar"),
    glance.NamedType("Float", None, []),
  )
  |> should.equal(Ok(analysis.float_type))

  analysis.resolve_type(
    empty_module_internals("foo", "bar"),
    glance.NamedType("MyFloat", None, []),
  )
  |> should.equal(Error(compiler.ReferenceError("MyFloat")))

  analysis.resolve_type(
    empty_module_internals("foo", "bar"),
    glance.NamedType("List", None, [glance.NamedType("Int", None, [])]),
  )
  |> should.equal(Ok(analysis.list_type(analysis.int_type)))

  analysis.resolve_type(
    empty_module_internals("foo", "bar"),
    glance.NamedType("List", None, [glance.NamedType("MyFloat", None, [])]),
  )
  |> should.equal(Error(compiler.ReferenceError("MyFloat")))

  let generic_custom_type_id =
    analysis.TypeFromModule(project.SourceLocation("foo", "bar"), "CustomType")
  let generic_custom_type =
    analysis.GenericType(analysis.TypeVariable(""), parameters: ["b", "c"])

  analysis.resolve_type(
    analysis.ModuleInternals(
      ..empty_module_internals("foo", "bar"),
      types: dict.from_list([#("CustomType", generic_custom_type)]),
    ),
    glance.NamedType("CustomType", None, [
      glance.NamedType("Float", None, []),
      glance.NamedType("Int", None, []),
    ]),
  )
  |> should.equal(
    Ok(
      analysis.TypeConstructor(generic_custom_type_id, [
        analysis.float_type,
        analysis.int_type,
      ]),
    ),
  )

  analysis.resolve_type(
    analysis.ModuleInternals(
      ..empty_module_internals("foo", "bar"),
      types: dict.from_list([#("CustomType", generic_custom_type)]),
    ),
    glance.NamedType("CustomType", None, [
      glance.VariableType("x"),
      glance.NamedType("Int", None, []),
    ]),
  )
  |> should.equal(
    Ok(
      analysis.TypeConstructor(generic_custom_type_id, [
        analysis.TypeVariable("x"),
        analysis.int_type,
      ]),
    ),
  )

  analysis.resolve_type(
    analysis.ModuleInternals(
      ..empty_module_internals("foo", "bar"),
      types: dict.from_list([#("CustomType", generic_custom_type)]),
    ),
    glance.NamedType("CustomType", None, [glance.VariableType("x")]),
  )
  |> should.equal(
    Error(compiler.TypeArityError(
      glance.NamedType("CustomType", None, [glance.VariableType("x")]),
      2,
      1,
    )),
  )
}

pub fn resolve_dependent_custom_types_test() {
  let project =
    project.Project("test", "javascript", dict.new(), fn(_a, _b) {
      Error(compiler.ReferenceError("foo"))
    })
  let location = project.SourceLocation("test", "bar")

  let assert Ok(internals) =
    analysis.resolve_types(
      project,
      location,
      dict.new(),
      dict.new(),
      glance.Module(
        imports: [],
        custom_types: [],
        type_aliases: [],
        constants: [],
        functions: [],
      ),
    )
  internals.types |> should.equal(dict.new())
  internals.custom_types |> should.equal(dict.new())

  let type_a_id = analysis.TypeFromModule(location, "TypeA")
  let type_b_id = analysis.TypeFromModule(location, "TypeB")

  let b_depends_on_a =
    dict.from_list([
      #(
        "TypeA",
        analysis.CustomType([], False, [
          analysis.FunctionSignature(
            "TypeA",
            [analysis.int_type],
            [],
            analysis.TypeConstructor(type_a_id, []),
          ),
        ]),
      ),
      #(
        "TypeB",
        analysis.CustomType([], False, [
          analysis.FunctionSignature(
            "TypeB",
            [analysis.TypeConstructor(type_a_id, [])],
            [],
            analysis.TypeConstructor(type_b_id, []),
          ),
        ]),
      ),
    ])

  analysis.resolve_types(
    project,
    location,
    dict.new(),
    dict.new(),
    glance.Module(
      imports: [],
      custom_types: [
        glance.Definition(
          [],
          glance.CustomType("TypeA", glance.Public, False, [], [
            glance.Variant("TypeA", [glance.Field(None, compiler.int_type)]),
          ]),
        ),
        glance.Definition(
          [],
          glance.CustomType("TypeB", glance.Public, False, [], [
            glance.Variant("TypeB", [
              glance.Field(None, glance.NamedType("TypeA", None, [])),
            ]),
          ]),
        ),
      ],
      type_aliases: [],
      constants: [],
      functions: [],
    ),
  )
  |> result.map(fn(r) { r.custom_types })
  |> should.equal(Ok(b_depends_on_a))

  analysis.resolve_types(
    project,
    location,
    dict.new(),
    dict.new(),
    glance.Module(
      imports: [],
      custom_types: [
        glance.Definition(
          [],
          glance.CustomType("TypeA", glance.Public, False, [], [
            glance.Variant("TypeA", [glance.Field(None, compiler.int_type)]),
          ]),
        ),
        glance.Definition(
          [],
          glance.CustomType("TypeB", glance.Public, False, [], [
            glance.Variant("TypeB", [
              glance.Field(None, glance.NamedType("TypeA", None, [])),
            ]),
          ]),
        ),
      ],
      type_aliases: [],
      constants: [],
      functions: [],
    ),
  )
  |> result.map(fn(r) { r.custom_types })
  |> should.equal(Ok(b_depends_on_a))

  let tree_type =
    analysis.TypeConstructor(analysis.TypeFromModule(location, "Tree"), [])

  analysis.resolve_types(
    project,
    location,
    dict.new(),
    dict.new(),
    glance.Module(
      imports: [],
      custom_types: [
        glance.Definition(
          [],
          glance.CustomType("Tree", glance.Public, False, [], [
            glance.Variant("Node", [
              glance.Field(Some("left"), glance.NamedType("Tree", None, [])),
              glance.Field(Some("right"), glance.NamedType("Tree", None, [])),
            ]),
            glance.Variant("Leaf", []),
          ]),
        ),
      ],
      type_aliases: [],
      constants: [],
      functions: [],
    ),
  )
  |> result.map(fn(r) { r.custom_types })
  |> should.equal(
    Ok(
      dict.from_list([
        #(
          "Tree",
          analysis.CustomType([], False, [
            analysis.FunctionSignature(
              "Node",
              [],
              [
                analysis.Labeled("left", tree_type),
                analysis.Labeled("right", tree_type),
              ],
              tree_type,
            ),
            analysis.FunctionSignature("Leaf", [], [], tree_type),
          ]),
        ),
      ]),
    ),
  )
}

pub fn resolve_type_aliases_test() {
  let project =
    project.Project("test", "javascript", dict.new(), fn(_a, _b) {
      Error(compiler.ReferenceError("foo"))
    })
  let location = project.SourceLocation("test", "bar")

  analysis.resolve_types(
    project,
    location,
    dict.new(),
    dict.new(),
    glance.Module(
      imports: [],
      custom_types: [],
      type_aliases: [
        glance.Definition(
          [],
          glance.TypeAlias("Alias", glance.Public, [], compiler.int_type),
        ),
      ],
      constants: [],
      functions: [],
    ),
  )
  |> result.map(fn(r) { r.types })
  |> should.equal(
    Ok(
      dict.from_list([#("Alias", analysis.GenericType(analysis.int_type, []))]),
    ),
  )

  analysis.resolve_types(
    project,
    location,
    dict.new(),
    dict.new(),
    glance.Module(
      imports: [],
      custom_types: [],
      type_aliases: [
        glance.Definition(
          [],
          glance.TypeAlias(
            "Alias",
            glance.Public,
            [],
            compiler.list_type(compiler.int_type),
          ),
        ),
      ],
      constants: [],
      functions: [],
    ),
  )
  |> result.map(fn(r) { r.types })
  |> should.equal(
    Ok(
      dict.from_list([
        #(
          "Alias",
          analysis.GenericType(analysis.list_type(analysis.int_type), []),
        ),
      ]),
    ),
  )

  analysis.resolve_types(
    project,
    location,
    dict.new(),
    dict.new(),
    glance.Module(
      imports: [],
      custom_types: [],
      type_aliases: [
        glance.Definition(
          [],
          glance.TypeAlias(
            "Alias",
            glance.Public,
            ["a"],
            compiler.list_type(glance.VariableType("a")),
          ),
        ),
      ],
      constants: [],
      functions: [],
    ),
  )
  |> result.map(fn(r) { r.types })
  |> should.equal(
    Ok(
      dict.from_list([
        #(
          "Alias",
          analysis.GenericType(analysis.list_type(analysis.TypeVariable("a")), [
            "a",
          ]),
        ),
      ]),
    ),
  )

  analysis.resolve_type(
    analysis.ModuleInternals(
      ..empty_module_internals("foo", "bar"),
      types: dict.from_list([
        #("Alias", analysis.GenericType(analysis.int_type, [])),
      ]),
    ),
    glance.NamedType("Alias", None, []),
  )
  |> should.equal(Ok(analysis.int_type))

  analysis.resolve_type(
    analysis.ModuleInternals(
      ..empty_module_internals("foo", "bar"),
      types: dict.from_list([
        #(
          "Alias",
          analysis.GenericType(
            analysis.FunctionType(
              [analysis.TypeVariable("a")],
              analysis.TypeVariable("b"),
            ),
            ["a", "b"],
          ),
        ),
      ]),
    ),
    glance.NamedType("Alias", None, [compiler.int_type, compiler.float_type]),
  )
  |> should.equal(
    Ok(analysis.FunctionType([analysis.int_type], analysis.float_type)),
  )
}

pub fn type_infer_function_test() {
  // fn(x, y) { x * y }

  let parsed_fn =
    glance.Fn(
      [
        glance.FnParameter(glance.Named("x"), None),
        glance.FnParameter(glance.Named("y"), None),
      ],
      None,
      [
        glance.Expression(glance.BinaryOperator(
          glance.MultInt,
          glance.Variable("x"),
          glance.Variable("y"),
        )),
      ],
    )

  let fn_with_blanks =
    analysis.Fn(
      typ: analysis.FunctionType(
        [analysis.TypeVariable("$3"), analysis.TypeVariable("$4")],
        analysis.TypeVariable("$2"),
      ),
      argument_names: [glance.Named("x"), glance.Named("y")],
      body: [
        analysis.Expression(analysis.Call(
          //analysis.TypeVariable("$2"),
          analysis.FunctionReference(
            analysis.FunctionType(
              [analysis.int_type, analysis.int_type],
              analysis.int_type,
            ),
            analysis.BuiltInFunction(analysis.BinaryOperator(glance.MultInt)),
          ),
          [
            analysis.Variable(analysis.TypeVariable("$3"), "x"),
            analysis.Variable(analysis.TypeVariable("$4"), "y"),
          ],
        )),
      ],
    )

  let constraints = [
    analysis.Equal(
      analysis.TypeVariable("$1"),
      analysis.FunctionType(
        [analysis.TypeVariable("$3"), analysis.TypeVariable("$4")],
        analysis.TypeVariable("$2"),
      ),
    ),
    analysis.Equal(analysis.TypeVariable("$2"), analysis.int_type),
    analysis.Equal(analysis.int_type, analysis.TypeVariable("$4")),
    analysis.Equal(analysis.int_type, analysis.TypeVariable("$3")),
  ]

  let substitution =
    dict.from_list([
      #(
        "$1",
        analysis.FunctionType(
          [analysis.TypeVariable("$3"), analysis.TypeVariable("$4")],
          analysis.TypeVariable("$2"),
        ),
      ),
      #("$2", analysis.int_type),
      #("$3", analysis.int_type),
      #("$4", analysis.int_type),
    ])

  let typed_fn =
    analysis.Fn(
      typ: analysis.FunctionType(
        [analysis.int_type, analysis.int_type],
        analysis.int_type,
      ),
      argument_names: [glance.Named("x"), glance.Named("y")],
      body: [
        analysis.Expression(analysis.Call(
          //analysis.TypeVariable("$2"),
          analysis.FunctionReference(
            analysis.FunctionType(
              [analysis.int_type, analysis.int_type],
              analysis.int_type,
            ),
            analysis.BuiltInFunction(analysis.BinaryOperator(glance.MultInt)),
          ),
          [
            analysis.Variable(analysis.int_type, "x"),
            analysis.Variable(analysis.int_type, "y"),
          ],
        )),
      ],
    )

  let assert Ok(#(context, initd_fn)) =
    parsed_fn
    |> analysis.init_inference(
      analysis.TypeVariable("$1"),
      dict.new(),
      analysis.Context(
        empty_module_internals("foo", "bar"),
        dict.from_list([#("$1", analysis.TypeVariable("$1"))]),
        [],
        2,
      ),
    )

  initd_fn |> should.equal(fn_with_blanks)
  context.constraints |> should.equal(constraints)

  let assert Ok(subst) =
    context
    |> analysis.solve_constraints

  subst
  |> should.equal(substitution)

  fn_with_blanks
  |> analysis.substitute_expression(substitution)
  |> should.equal(typed_fn)
}

pub fn type_infer_function_using_import_test() {
  // fn(x) { int.to_string(x) }

  let parsed_fn =
    glance.Fn([glance.FnParameter(glance.Named("x"), None)], None, [
      glance.Expression(
        glance.Call(glance.FieldAccess(glance.Variable("int"), "to_string"), [
          glance.Field(None, glance.Variable("x")),
        ]),
      ),
    ])

  let module_int =
    analysis.Module(
      project.SourceLocation("gleam_stdlib", "gleam/int"),
      dict.new(),
      dict.new(),
      dict.new(),
      dict.from_list([
        #(
          "to_string",
          analysis.FunctionSignature(
            "to_string",
            [analysis.int_type],
            [],
            analysis.string_type,
          ),
        ),
      ]),
    )

  let fn_with_blanks =
    analysis.Fn(
      typ: analysis.FunctionType(
        [analysis.TypeVariable("$3")],
        analysis.TypeVariable("$2"),
      ),
      argument_names: [glance.Named("x")],
      body: [
        analysis.Expression(
          analysis.Call(
            analysis.FunctionReference(
              analysis.FunctionType([analysis.int_type], analysis.string_type),
              analysis.FunctionFromModule(module_int.location, "to_string"),
            ),
            [analysis.Variable(analysis.TypeVariable("$3"), "x")],
          ),
        ),
      ],
    )

  let constraints = [
    analysis.Equal(
      analysis.TypeVariable("$1"),
      analysis.FunctionType(
        [analysis.TypeVariable("$3")],
        analysis.TypeVariable("$2"),
      ),
    ),
    analysis.Equal(analysis.TypeVariable("$4"), analysis.TypeVariable("$3")),
    analysis.Equal(
      analysis.FunctionType(
        [analysis.TypeVariable("$4")],
        analysis.TypeVariable("$2"),
      ),
      analysis.FunctionType([analysis.int_type], analysis.string_type),
    ),
  ]

  let substitution =
    dict.from_list([
      #(
        "$1",
        analysis.FunctionType(
          [analysis.TypeVariable("$3")],
          analysis.TypeVariable("$2"),
        ),
      ),
      #("$2", analysis.string_type),
      #("$3", analysis.int_type),
      #("$4", analysis.TypeVariable("$3")),
    ])

  let typed_fn =
    analysis.Fn(
      typ: analysis.FunctionType([analysis.int_type], analysis.string_type),
      argument_names: [glance.Named("x")],
      body: [
        analysis.Expression(
          analysis.Call(
            analysis.FunctionReference(
              analysis.FunctionType([analysis.int_type], analysis.string_type),
              analysis.FunctionFromModule(module_int.location, "to_string"),
            ),
            [analysis.Variable(analysis.int_type, "x")],
          ),
        ),
      ],
    )

  let assert Ok(#(context, initd_fn)) =
    parsed_fn
    |> analysis.init_inference(
      analysis.TypeVariable("$1"),
      dict.new(),
      analysis.Context(
        analysis.ModuleInternals(
          ..empty_module_internals("foo", "bar"),
          imports: dict.from_list([#("int", module_int)]),
        ),
        dict.from_list([#("$1", analysis.TypeVariable("$1"))]),
        [],
        2,
      ),
    )

  initd_fn |> should.equal(fn_with_blanks)
  context.constraints |> should.equal(constraints)

  let assert Ok(subst) =
    context
    |> analysis.solve_constraints

  subst
  |> should.equal(substitution)

  initd_fn
  |> analysis.substitute_expression(substitution)
  |> should.equal(typed_fn)
}

pub fn type_infer_function_with_return_annotation_test() {
  // fn() -> Int { 42 }
  glance.Fn([], Some(glance.NamedType("Int", None, [])), [
    glance.Expression(glance.Int("42")),
  ])
  |> analysis.init_inference(
    analysis.TypeVariable("$1"),
    dict.from_list([]),
    analysis.Context(
      empty_module_internals("foo", "bar"),
      dict.from_list([#("$1", analysis.TypeVariable("$1"))]),
      [],
      2,
    ),
  )
  |> result.map(pair.second)
  |> should.equal(
    Ok(
      analysis.Fn(analysis.FunctionType([], analysis.int_type), [], [
        analysis.Expression(analysis.Int("42")),
      ]),
    ),
  )

  // fn() -> Bar { 42 }, where type Bar = Int
  glance.Fn([], Some(glance.NamedType("Bar", None, [])), [
    glance.Expression(glance.Int("42")),
  ])
  |> analysis.init_inference(
    analysis.TypeVariable("$1"),
    dict.from_list([]),
    analysis.Context(
      analysis.ModuleInternals(
        ..empty_module_internals("foo", "bar"),
        types: dict.from_list([
          #("Bar", analysis.GenericType(analysis.int_type, [])),
        ]),
      ),
      dict.from_list([#("$1", analysis.TypeVariable("$1"))]),
      [],
      2,
    ),
  )
  |> result.map(pair.second)
  |> should.equal(
    Ok(
      analysis.Fn(analysis.FunctionType([], analysis.int_type), [], [
        analysis.Expression(analysis.Int("42")),
      ]),
    ),
  )

  let option_module_id = project.SourceLocation("gleam_stdlib", "gleam/option")
  let option_type =
    analysis.TypeConstructor(
      analysis.TypeFromModule(option_module_id, "Option"),
      [analysis.TypeVariable("a")],
    )
  let option_module =
    analysis.Module(
      location: option_module_id,
      imports: dict.new(),
      types: dict.from_list([
        #("option", analysis.GenericType(option_type, ["a"])),
      ]),
      custom_types: dict.new(),
      functions: dict.from_list([
        #(
          "Some",
          analysis.FunctionSignature(
            "Some",
            [analysis.TypeVariable("a")],
            [],
            option_type,
          ),
        ),
      ]),
    )

  // fn(x) { Some(x) }
  // TODO: $5 is pretty arbitrary!
  let type_var = analysis.TypeVariable("$5")
  let option_with_type_var =
    analysis.TypeConstructor(
      analysis.TypeFromModule(option_module_id, "Option"),
      [type_var],
    )
  glance.Fn([glance.FnParameter(glance.Named("x"), None)], None, [
    glance.Expression(
      glance.Call(glance.FieldAccess(glance.Variable("option"), "Some"), [
        glance.Field(None, glance.Variable("x")),
      ]),
    ),
  ])
  |> analysis.infer(
    analysis.ModuleInternals(
      ..empty_module_internals("foo", "bar"),
      imports: dict.from_list([#("option", option_module)]),
    ),
  )
  |> should.equal(
    Ok(
      analysis.Fn(
        analysis.FunctionType([type_var], option_with_type_var),
        [glance.Named("x")],
        [
          analysis.Expression(
            analysis.Call(
              analysis.FunctionReference(
                analysis.FunctionType([type_var], option_with_type_var),
                analysis.FunctionFromModule(option_module_id, "Some"),
              ),
              [analysis.Variable(type_var, "x")],
            ),
          ),
        ],
      ),
    ),
  )
}

pub fn type_infer_block_test() {
  glance.Block([
    glance.Assignment(
      kind: glance.Let,
      pattern: glance.PatternVariable("x"),
      annotation: option.None,
      value: glance.Int("42"),
    ),
    glance.Expression(glance.Variable("x")),
  ])
  |> analysis.infer(empty_module_internals("foo", "bar"))
  |> should.equal(
    Ok(
      analysis.Call(
        analysis.Fn(analysis.FunctionType([], analysis.int_type), [], [
          analysis.Assignment(
            glance.Let,
            analysis.PatternWithVariables(
              analysis.PatternVariable("x"),
              dict.from_list([#("x", analysis.int_type)]),
            ),
            analysis.Int("42"),
          ),
          analysis.Expression(analysis.Variable(analysis.int_type, "x")),
        ]),
        [],
      ),
    ),
  )
}

pub fn type_infer_tuple_test() {
  glance.Tuple([glance.Int("42"), glance.Float("3.14")])
  |> analysis.infer(empty_module_internals("foo", "bar"))
  |> should.equal(
    Ok(
      analysis.Call(
        analysis.FunctionReference(
          analysis.FunctionType(
            [analysis.int_type, analysis.float_type],
            analysis.TypeConstructor(
              analysis.BuiltInType(analysis.TupleType(2)),
              [analysis.int_type, analysis.float_type],
            ),
          ),
          analysis.BuiltInFunction(analysis.TupleConstructor(2)),
        ),
        [analysis.Int("42"), analysis.Float("3.14")],
      ),
    ),
  )
}

pub fn type_infer_tuple_index_test() {
  let my_tuple_type =
    analysis.TypeConstructor(analysis.BuiltInType(analysis.TupleType(2)), [
      analysis.int_type,
      analysis.float_type,
    ])
  glance.TupleIndex(glance.Tuple([glance.Int("42"), glance.Float("3.14")]), 1)
  |> analysis.infer(empty_module_internals("foo", "bar"))
  |> should.equal(
    Ok(
      analysis.Call(
        analysis.FunctionReference(
          analysis.FunctionType([my_tuple_type], analysis.float_type),
          analysis.BuiltInFunction(analysis.AccessField(
            None,
            analysis.ByPosition(1),
          )),
        ),
        [
          analysis.Call(
            analysis.FunctionReference(
              analysis.FunctionType(
                [analysis.int_type, analysis.float_type],
                my_tuple_type,
              ),
              analysis.BuiltInFunction(analysis.TupleConstructor(2)),
            ),
            [analysis.Int("42"), analysis.Float("3.14")],
          ),
        ],
      ),
    ),
  )
}

pub fn type_infer_nested_generics_test() {
  let module_id = project.SourceLocation("foo", "bar")

  let holder = fn(t) {
    analysis.TypeConstructor(analysis.TypeFromModule(module_id, "Holder"), [t])
  }
  let holder_constructor = fn(t) {
    analysis.FunctionReference(
      analysis.FunctionType([t], holder(t)),
      analysis.FunctionFromModule(module_id, "Holder"),
    )
  }
  let a = analysis.TypeVariable("a")

  glance.Call(glance.Variable("Holder"), [
    glance.Field(
      None,
      glance.Call(glance.Variable("Holder"), [
        glance.Field(None, glance.Int("42")),
      ]),
    ),
  ])
  |> analysis.infer(
    analysis.ModuleInternals(
      ..empty_module_internals("foo", "bar"),
      functions: dict.from_list([
        #("Holder", analysis.FunctionSignature("Holder", [a], [], holder(a))),
      ]),
    ),
  )
  |> should.equal(
    Ok(
      analysis.Call(holder_constructor(holder(analysis.int_type)), [
        analysis.Call(holder_constructor(analysis.int_type), [
          analysis.Int("42"),
        ]),
      ]),
    ),
  )
}

pub fn type_infer_let_pattern_test() {
  glance.Block([
    glance.Assignment(
      kind: glance.Let,
      pattern: glance.PatternTuple([
        glance.PatternDiscard(""),
        glance.PatternVariable("y"),
      ]),
      annotation: option.None,
      value: glance.Tuple([glance.Int("42"), glance.Float("3.14")]),
    ),
    glance.Expression(glance.Variable("y")),
  ])
  |> analysis.infer(empty_module_internals("foo", "bar"))
  |> should.equal(
    Ok(
      analysis.Call(
        analysis.Fn(analysis.FunctionType([], analysis.float_type), [], [
          analysis.Assignment(
            glance.Let,
            analysis.PatternWithVariables(
              analysis.PatternTuple([
                analysis.PatternDiscard(""),
                analysis.PatternVariable("y"),
              ]),
              dict.from_list([#("y", analysis.float_type)]),
            ),
            analysis.Call(
              analysis.FunctionReference(
                analysis.FunctionType(
                  [analysis.int_type, analysis.float_type],
                  analysis.TypeConstructor(
                    analysis.BuiltInType(analysis.TupleType(2)),
                    [analysis.int_type, analysis.float_type],
                  ),
                ),
                analysis.BuiltInFunction(analysis.TupleConstructor(2)),
              ),
              [analysis.Int("42"), analysis.Float("3.14")],
            ),
          ),
          analysis.Expression(analysis.Variable(analysis.float_type, "y")),
        ]),
        [],
      ),
    ),
  )
  // TODO: let assert scenario with Result
}

pub fn type_infer_list_pattern_test() {
  let context =
    analysis.Context(
      empty_module_internals("foo", "bar"),
      dict.from_list([#("$1", analysis.TypeVariable("$1"))]),
      [],
      2,
    )

  let assert Ok(#(new_context, empty_list_pattern)) =
    glance.PatternList([], None)
    |> analysis.init_inference_pattern(
      analysis.TypeVariable("$1"),
      dict.new(),
      context,
    )
  empty_list_pattern
  |> should.equal(analysis.PatternWithVariables(
    analysis.PatternList(analysis.PatternEmpty),
    dict.new(),
  ))
  new_context.constraints
  |> should.equal([
    analysis.Equal(
      analysis.TypeVariable("$1"),
      analysis.list_type(analysis.TypeVariable("$2")),
    ),
  ])

  let assert Ok(#(new_context, singleton_list_pattern)) =
    glance.PatternList([glance.PatternInt("42")], None)
    |> analysis.init_inference_pattern(
      analysis.TypeVariable("$1"),
      dict.new(),
      context,
    )
  singleton_list_pattern
  |> should.equal(analysis.PatternWithVariables(
    analysis.PatternList(analysis.PatternNonEmpty(
      analysis.PatternInt("42"),
      analysis.PatternEmpty,
    )),
    dict.new(),
  ))
  new_context.constraints
  |> should.equal([
    analysis.Equal(
      analysis.TypeVariable("$1"),
      analysis.list_type(analysis.TypeVariable("$2")),
    ),
    analysis.Equal(analysis.TypeVariable("$2"), analysis.int_type),
  ])

  let assert Ok(#(new_context, simple_list_pattern)) =
    glance.PatternList(
      [glance.PatternVariable("head")],
      Some(glance.PatternVariable("tail")),
    )
    |> analysis.init_inference_pattern(
      analysis.TypeVariable("$1"),
      dict.new(),
      context,
    )
  simple_list_pattern
  |> should.equal(analysis.PatternWithVariables(
    analysis.PatternList(analysis.PatternNonEmpty(
      analysis.PatternVariable("head"),
      analysis.PatternTail(analysis.PatternVariable("tail")),
    )),
    dict.from_list([
      #("head", analysis.TypeVariable("$2")),
      #("tail", analysis.list_type(analysis.TypeVariable("$3"))),
    ]),
  ))
  new_context.constraints
  |> should.equal([
    analysis.Equal(
      analysis.TypeVariable("$1"),
      analysis.list_type(analysis.TypeVariable("$2")),
    ),
    analysis.Equal(
      analysis.list_type(analysis.TypeVariable("$2")),
      analysis.list_type(analysis.TypeVariable("$3")),
    ),
  ])
}

pub fn type_infer_constructor_pattern_test() {
  let module_id = project.SourceLocation("foo", "bar")
  let my_type_id = analysis.TypeFromModule(module_id, "MyType")
  let my_type =
    analysis.TypeConstructor(my_type_id, [analysis.TypeVariable("a")])
  let variant_a =
    analysis.FunctionSignature(
      "VariantA",
      [],
      [analysis.Labeled("x", analysis.int_type)],
      my_type,
    )
  let variant_b =
    analysis.FunctionSignature(
      "VariantB",
      [],
      [
        analysis.Labeled("x", analysis.int_type),
        analysis.Labeled("y", analysis.TypeVariable("a")),
      ],
      my_type,
    )
  let my_type_definition =
    analysis.CustomType([], False, [variant_a, variant_b])
  let internals =
    analysis.ModuleInternals(
      project: project.Project("name", "javascript", dict.new(), fn(_, _) {
        panic
      }),
      location: module_id,
      imports: dict.new(),
      types: dict.from_list([#("MyType", analysis.GenericType(my_type, []))]),
      custom_types: dict.from_list([#("MyType", my_type_definition)]),
      functions: dict.from_list([
        #("VariantA", variant_a),
        #("VariantB", variant_b),
      ]),
      public_types: set.new(),
      public_functions: set.new(),
    )

  let assert Ok(#(new_context, pattern)) =
    glance.PatternConstructor(
      None,
      "VariantB",
      [glance.Field(Some("y"), glance.PatternString("str_val"))],
      True,
    )
    |> analysis.init_inference_pattern(
      analysis.TypeVariable("$1"),
      dict.new(),
      analysis.Context(
        internals,
        dict.from_list([#("$1", analysis.TypeVariable("$1"))]),
        [],
        2,
      ),
    )

  let variant_b_subst =
    analysis.substitute_signature(
      variant_b,
      dict.from_list([#("a", analysis.TypeVariable("$2"))]),
    )

  pattern
  |> should.equal(analysis.PatternWithVariables(
    analysis.PatternConstructor(my_type_id, variant_b_subst, [
      analysis.PatternDiscard(""),
      analysis.PatternString("str_val"),
    ]),
    dict.new(),
  ))
  new_context.constraints
  |> should.equal([
    analysis.Equal(
      analysis.TypeVariable("$1"),
      analysis.TypeConstructor(my_type_id, [analysis.TypeVariable("$2")]),
    ),
    analysis.Equal(analysis.TypeVariable("$2"), analysis.string_type),
  ])

  let assert Ok(#(_context, pattern)) =
    glance.PatternConstructor(
      None,
      "VariantB",
      [
        glance.Field(None, glance.PatternVariable("g")),
        glance.Field(None, glance.PatternVariable("h")),
      ],
      False,
    )
    |> analysis.init_inference_pattern(
      analysis.TypeVariable("$1"),
      dict.new(),
      analysis.Context(
        internals,
        dict.from_list([#("$1", analysis.TypeVariable("$1"))]),
        [],
        2,
      ),
    )

  pattern
  |> should.equal(analysis.PatternWithVariables(
    analysis.PatternConstructor(my_type_id, variant_b_subst, [
      analysis.PatternVariable("g"),
      analysis.PatternVariable("h"),
    ]),
    dict.from_list([
      #("g", analysis.int_type),
      #("h", analysis.TypeVariable("$2")),
    ]),
  ))
}

pub fn type_infer_field_access_test() {
  let module_id = project.SourceLocation("foo", "bar")
  let my_type =
    analysis.TypeConstructor(analysis.TypeFromModule(module_id, "MyType"), [])
  let variant_a =
    analysis.FunctionSignature(
      "VariantA",
      [],
      [analysis.Labeled("a", analysis.int_type)],
      my_type,
    )
  let variant_b =
    analysis.FunctionSignature(
      "VariantB",
      [],
      [
        analysis.Labeled("a", analysis.int_type),
        analysis.Labeled("b", analysis.float_type),
      ],
      my_type,
    )
  let my_type_definition =
    analysis.CustomType([], False, [variant_a, variant_b])
  let access_field_a =
    analysis.FunctionReference(
      analysis.FunctionType([my_type], analysis.int_type),
      analysis.BuiltInFunction(analysis.AccessField(None, analysis.ByLabel("a"))),
    )
  let internals =
    analysis.ModuleInternals(
      project: project.Project("name", "javascript", dict.new(), fn(_, _) {
        panic
      }),
      location: module_id,
      imports: dict.new(),
      types: dict.from_list([#("MyType", analysis.GenericType(my_type, []))]),
      custom_types: dict.from_list([#("MyType", my_type_definition)]),
      functions: dict.from_list([
        #("VariantA", variant_a),
        #("VariantB", variant_b),
      ]),
      public_types: set.new(),
      public_functions: set.new(),
    )
  glance.FieldAccess(
    glance.Call(glance.Variable("VariantA"), [
      glance.Field(None, glance.Int("42")),
    ]),
    "a",
  )
  |> analysis.infer(internals)
  |> should.equal(
    Ok(
      analysis.Call(access_field_a, [
        analysis.Call(
          analysis.FunctionReference(
            analysis.FunctionType([analysis.int_type], my_type),
            analysis.FunctionFromModule(module_id, "VariantA"),
          ),
          [analysis.Int("42")],
        ),
      ]),
    ),
  )

  // redefining the field access here to use the variant-specific version
  // TODO: consider changing to always generate the general accessor when possible
  let access_field_a =
    analysis.FunctionReference(
      analysis.FunctionType([my_type], analysis.int_type),
      analysis.BuiltInFunction(analysis.AccessField(
        Some("VariantB"),
        analysis.ByPosition(0),
      )),
    )
  let variant_b_constructor =
    analysis.FunctionReference(
      analysis.signature_type(variant_b),
      analysis.FunctionFromModule(module_id, "VariantB"),
    )
  glance.Block([
    glance.Assignment(
      glance.Let,
      glance.PatternVariable("record"),
      None,
      glance.Call(glance.Variable("VariantB"), [
        glance.Field(None, glance.Int("42")),
        glance.Field(None, glance.Float("3.14")),
      ]),
    ),
    glance.Expression(
      glance.RecordUpdate(None, "VariantB", glance.Variable("record"), [
        #("b", glance.Float("2.71")),
      ]),
    ),
  ])
  |> analysis.infer(internals)
  |> should.equal(
    Ok(
      analysis.Call(
        analysis.Fn(analysis.FunctionType([], my_type), [], [
          analysis.Assignment(
            glance.Let,
            analysis.PatternWithVariables(
              analysis.PatternVariable("record"),
              dict.from_list([#("record", my_type)]),
            ),
            analysis.Call(variant_b_constructor, [
              analysis.Int("42"),
              analysis.Float("3.14"),
            ]),
          ),
          analysis.Expression(
            analysis.Call(variant_b_constructor, [
              analysis.Call(access_field_a, [
                analysis.Variable(my_type, "record"),
              ]),
              analysis.Float("2.71"),
            ]),
          ),
        ]),
        [],
      ),
    ),
  )
}

pub fn case_expr_inference_test() {
  // Two subjects, one clause
  glance.Case([glance.Int("42"), glance.Float("3.1")], [
    glance.Clause(
      [[glance.PatternVariable("n"), glance.PatternDiscard("x")]],
      None,
      glance.Variable("n"),
    ),
  ])
  |> analysis.infer(empty_module_internals("foo", "bar"))
  |> should.equal(
    Ok(
      analysis.Case(
        analysis.int_type,
        [analysis.Int("42"), analysis.Float("3.1")],
        [
          analysis.Clause(
            [[analysis.PatternVariable("n"), analysis.PatternDiscard("x")]],
            dict.from_list([#("n", analysis.int_type)]),
            analysis.Call(
              analysis.FunctionReference(
                analysis.FunctionType([], analysis.bool_type),
                analysis.BuiltInFunction(analysis.BoolConstructor(True)),
              ),
              [],
            ),
            analysis.Variable(analysis.int_type, "n"),
          ),
        ],
      ),
    ),
  )

  // Test with two clauses and a guard
  glance.Case([glance.Int("42"), glance.Int("13")], [
    glance.Clause(
      [[glance.PatternVariable("n"), glance.PatternDiscard("m")]],
      Some(glance.BinaryOperator(
        glance.GtInt,
        glance.Variable("n"),
        glance.Int("31"),
      )),
      glance.Variable("n"),
    ),
    glance.Clause(
      [[glance.PatternDiscard("n"), glance.PatternVariable("m")]],
      None,
      glance.Variable("m"),
    ),
  ])
  |> analysis.infer(empty_module_internals("foo", "bar"))
  |> should.equal(
    Ok(
      analysis.Case(
        analysis.int_type,
        [analysis.Int("42"), analysis.Int("13")],
        [
          analysis.Clause(
            [[analysis.PatternVariable("n"), analysis.PatternDiscard("m")]],
            dict.from_list([#("n", analysis.int_type)]),
            analysis.Call(
              analysis.FunctionReference(
                analysis.FunctionType(
                  [analysis.int_type, analysis.int_type],
                  analysis.bool_type,
                ),
                analysis.BuiltInFunction(analysis.BinaryOperator(glance.GtInt)),
              ),
              [analysis.Variable(analysis.int_type, "n"), analysis.Int("31")],
            ),
            analysis.Variable(analysis.int_type, "n"),
          ),
          analysis.Clause(
            [[analysis.PatternDiscard("n"), analysis.PatternVariable("m")]],
            dict.from_list([#("m", analysis.int_type)]),
            analysis.Call(
              analysis.FunctionReference(
                analysis.FunctionType([], analysis.bool_type),
                analysis.BuiltInFunction(analysis.BoolConstructor(True)),
              ),
              [],
            ),
            analysis.Variable(analysis.int_type, "m"),
          ),
        ],
      ),
    ),
  )
  // TODO: test with alternative patterns
}

pub fn list_inference_test() {
  let int_list = analysis.list_type(analysis.int_type)
  let empty_list =
    analysis.FunctionReference(
      analysis.FunctionType([], int_list),
      analysis.BuiltInFunction(analysis.EmptyListConstructor),
    )
  let non_empty_list =
    analysis.FunctionReference(
      analysis.FunctionType([analysis.int_type, int_list], int_list),
      analysis.BuiltInFunction(analysis.NonEmptyListConstructor),
    )
  glance.List(
    [glance.Int("1"), glance.Int("2")],
    Some(glance.List([glance.Int("3"), glance.Int("4")], None)),
  )
  |> analysis.infer(empty_module_internals("foo", "bar"))
  |> should.equal(
    Ok(
      analysis.Call(non_empty_list, [
        analysis.Int("1"),
        analysis.Call(non_empty_list, [
          analysis.Int("2"),
          analysis.Call(non_empty_list, [
            analysis.Int("3"),
            analysis.Call(non_empty_list, [
              analysis.Int("4"),
              analysis.Call(empty_list, []),
            ]),
          ]),
        ]),
      ]),
    ),
  )
}

pub fn pipe_operator_to_call_test() {
  let assert Ok(#(context, expr)) =
    glance.BinaryOperator(
      glance.Pipe,
      glance.Variable("lst"),
      glance.Call(glance.Variable("map"), [
        glance.Field(None, glance.Variable("fun")),
      ]),
    )
    |> analysis.init_inference(
      analysis.TypeVariable("$1"),
      dict.from_list([
        #("lst", analysis.TypeVariable("a")),
        #("map", analysis.TypeVariable("b")),
        #("fun", analysis.TypeVariable("c")),
      ]),
      analysis.Context(empty_module_internals("foo", "bar"), dict.new(), [], 2),
    )

  context.constraints
  |> should.equal([
    analysis.Equal(analysis.TypeVariable("$3"), analysis.TypeVariable("c")),
    analysis.Equal(analysis.TypeVariable("$2"), analysis.TypeVariable("a")),
    analysis.Equal(
      analysis.FunctionType(
        [analysis.TypeVariable("$2"), analysis.TypeVariable("$3")],
        analysis.TypeVariable("$1"),
      ),
      analysis.TypeVariable("b"),
    ),
  ])

  expr
  |> should.equal(
    analysis.Call(analysis.Variable(analysis.TypeVariable("b"), "map"), [
      analysis.Variable(analysis.TypeVariable("a"), "lst"),
      analysis.Variable(analysis.TypeVariable("c"), "fun"),
    ]),
  )
}

pub fn pipe_operator_to_var_test() {
  let assert Ok(#(context, expr)) =
    glance.BinaryOperator(
      glance.Pipe,
      glance.Variable("lst"),
      glance.Variable("fun"),
    )
    |> analysis.init_inference(
      analysis.TypeVariable("$1"),
      dict.from_list([
        #("lst", analysis.TypeVariable("a")),
        #("fun", analysis.TypeVariable("b")),
      ]),
      analysis.Context(empty_module_internals("foo", "bar"), dict.new(), [], 2),
    )

  context.constraints
  |> should.equal([
    analysis.Equal(analysis.TypeVariable("$2"), analysis.TypeVariable("a")),
    analysis.Equal(
      analysis.FunctionType(
        [analysis.TypeVariable("$2")],
        analysis.TypeVariable("$1"),
      ),
      analysis.TypeVariable("b"),
    ),
  ])

  expr
  |> should.equal(
    analysis.Call(analysis.Variable(analysis.TypeVariable("b"), "fun"), [
      analysis.Variable(analysis.TypeVariable("a"), "lst"),
    ]),
  )
}

pub fn fn_capture_test() {
  let assert Ok(#(context, expr)) =
    glance.FnCapture(
      None,
      glance.Variable("fun"),
      [glance.Field(None, glance.Variable("x"))],
      [glance.Field(None, glance.Variable("y"))],
    )
    |> analysis.init_inference(
      analysis.TypeVariable("$1"),
      dict.from_list([
        #("fun", analysis.TypeVariable("a")),
        #("x", analysis.int_type),
        #("y", analysis.int_type),
      ]),
      analysis.Context(empty_module_internals("foo", "bar"), dict.new(), [], 2),
    )

  expr
  |> should.equal(
    analysis.Fn(analysis.TypeVariable("$1"), [glance.Named("?")], [
      analysis.Expression(
        analysis.Call(analysis.Variable(analysis.TypeVariable("a"), "fun"), [
          analysis.Variable(analysis.int_type, "x"),
          analysis.Variable(analysis.TypeVariable("$2"), "?"),
          analysis.Variable(analysis.int_type, "y"),
        ]),
      ),
    ]),
  )

  context.constraints
  |> should.equal([
    analysis.Equal(
      analysis.TypeVariable("$1"),
      analysis.FunctionType(
        [analysis.TypeVariable("$2")],
        analysis.TypeVariable("$3"),
      ),
    ),
    analysis.Equal(
      analysis.TypeVariable("$6"),
      analysis.TypeConstructor(analysis.BuiltInType(analysis.IntType), []),
    ),
    analysis.Equal(analysis.TypeVariable("$5"), analysis.TypeVariable("$2")),
    analysis.Equal(
      analysis.TypeVariable("$4"),
      analysis.TypeConstructor(analysis.BuiltInType(analysis.IntType), []),
    ),
    analysis.Equal(
      analysis.FunctionType(
        [
          analysis.TypeVariable("$4"),
          analysis.TypeVariable("$5"),
          analysis.TypeVariable("$6"),
        ],
        analysis.TypeVariable("$3"),
      ),
      analysis.TypeVariable("a"),
    ),
  ])
}

pub fn infer_function_test() {
  let tv_a = analysis.TypeVariable("a")
  let tv_b = analysis.TypeVariable("b")
  let add =
    analysis.FunctionSignature(
      "add",
      [analysis.int_type, analysis.int_type],
      [],
      analysis.int_type,
    )
  let fold =
    analysis.FunctionSignature(
      "fold",
      [],
      [
        analysis.Labeled("over", analysis.list_type(tv_a)),
        analysis.Labeled("from", tv_b),
        analysis.Labeled("with", analysis.FunctionType([tv_b, tv_a], tv_b)),
      ],
      tv_b,
    )

  let funcref = fn(sig) {
    analysis.FunctionReference(
      analysis.signature_type(sig),
      analysis.FunctionFromModule(
        project.SourceLocation("foo", "bar"),
        sig.name,
      ),
    )
  }

  let sum_expected =
    Ok(analysis.Function(
      analysis.FunctionSignature(
        "sum",
        [analysis.list_type(analysis.int_type)],
        [],
        analysis.int_type,
      ),
      [glance.Named("lst")],
      analysis.GleamBody([
        analysis.Expression(
          analysis.Call(
            funcref(analysis.substitute_signature(
              fold,
              dict.from_list([
                #("a", analysis.int_type),
                #("b", analysis.int_type),
              ]),
            )),
            [
              analysis.Variable(analysis.list_type(analysis.int_type), "lst"),
              analysis.Int("0"),
              funcref(add),
            ],
          ),
        ),
      ]),
    ))

  // Positional arguments
  analysis.infer_function(
    analysis.ModuleInternals(
      ..empty_module_internals("foo", "bar"),
      functions: dict.from_list([#("add", add), #("fold", fold)]),
    ),
    glance.Definition(
      [],
      glance.Function(
        "sum",
        glance.Public,
        [glance.FunctionParameter(None, glance.Named("lst"), None)],
        None,
        [
          glance.Expression(
            glance.Call(glance.Variable("fold"), [
              glance.Field(None, glance.Variable("lst")),
              glance.Field(None, glance.Int("0")),
              glance.Field(None, glance.Variable("add")),
            ]),
          ),
        ],
        glance.Span(0, 1),
      ),
    ),
  )
  |> should.equal(sum_expected)

  // Labeled arguments, in order
  analysis.infer_function(
    analysis.ModuleInternals(
      ..empty_module_internals("foo", "bar"),
      functions: dict.from_list([#("add", add), #("fold", fold)]),
    ),
    glance.Definition(
      [],
      glance.Function(
        "sum",
        glance.Public,
        [glance.FunctionParameter(None, glance.Named("lst"), None)],
        None,
        [
          glance.Expression(
            glance.Call(glance.Variable("fold"), [
              glance.Field(Some("over"), glance.Variable("lst")),
              glance.Field(Some("from"), glance.Int("0")),
              glance.Field(Some("with"), glance.Variable("add")),
            ]),
          ),
        ],
        glance.Span(0, 1),
      ),
    ),
  )
  |> should.equal(sum_expected)

  // Labeled arguments, out of order
  analysis.infer_function(
    analysis.ModuleInternals(
      ..empty_module_internals("foo", "bar"),
      functions: dict.from_list([#("add", add), #("fold", fold)]),
    ),
    glance.Definition(
      [],
      glance.Function(
        "sum",
        glance.Public,
        [glance.FunctionParameter(None, glance.Named("lst"), None)],
        None,
        [
          glance.Expression(
            glance.Call(glance.Variable("fold"), [
              glance.Field(Some("from"), glance.Int("0")),
              glance.Field(Some("with"), glance.Variable("add")),
              glance.Field(Some("over"), glance.Variable("lst")),
            ]),
          ),
        ],
        glance.Span(0, 1),
      ),
    ),
  )
  |> should.equal(sum_expected)
}

pub fn no_arg_constructor_test() {
  let internals = empty_module_internals("foo", "bar")
  let order_id = analysis.TypeFromModule(internals.location, "Order")
  let order =
    analysis.CustomType([], False, [
      analysis.FunctionSignature(
        "Lt",
        [],
        [],
        analysis.TypeConstructor(order_id, []),
      ),
      analysis.FunctionSignature(
        "Eq",
        [],
        [],
        analysis.TypeConstructor(order_id, []),
      ),
      analysis.FunctionSignature(
        "Gt",
        [],
        [],
        analysis.TypeConstructor(order_id, []),
      ),
    ])
  let internals =
    analysis.ModuleInternals(
      ..internals,
      types: dict.from_list([
        #(
          "Order",
          analysis.GenericType(analysis.TypeConstructor(order_id, []), []),
        ),
      ]),
      functions: dict.from_list(
        order.variants |> list.map(fn(variant) { #(variant.name, variant) }),
      ),
    )
  "pub fn less_than() -> Order {
    Lt
  }"
  |> glance.module
  |> result.replace_error(Nil)
  |> result.try(fn(module) {
    list.find(module.functions, fn(fun) { fun.definition.name == "less_than" })
  })
  |> result.replace_error(compiler.AnotherTypeError("Sorry!"))
  |> result.try(analysis.infer_function(internals, _))
  |> result.is_ok
  |> should.equal(True)

  "pub fn to_int(order: Order) {
    case order {
      Lt -> -1
      Eq -> 0
      Gt -> 1
    }
  }"
  |> glance.module
  |> result.replace_error(Nil)
  |> result.try(fn(module) {
    list.find(module.functions, fn(fun) { fun.definition.name == "to_int" })
  })
  |> result.replace_error(compiler.AnotherTypeError("Sorry!"))
  |> result.try(analysis.infer_function(internals, _))
  |> result.is_ok
  |> should.equal(True)
}

pub fn result_test() {
  let result_type =
    analysis.TypeConstructor(
      analysis.TypeFromModule(
        project.SourceLocation("gleam", "gleam"),
        "Result",
      ),
      [analysis.TypeVariable("a"), analysis.TypeVariable("b")],
    )
  let result_custom_type =
    analysis.CustomType(parameters: ["a", "b"], opaque_: False, variants: [
      analysis.FunctionSignature(
        "Ok",
        [analysis.TypeVariable("a")],
        [],
        result_type,
      ),
      analysis.FunctionSignature(
        "Error",
        [analysis.TypeVariable("b")],
        [],
        result_type,
      ),
    ])
  let internals =
    analysis.ModuleInternals(
      ..empty_module_internals("foo", "bar"),
      custom_types: dict.from_list([#("Result", result_custom_type)]),
      types: dict.from_list([
        #("Result", analysis.GenericType(result_type, ["a", "b"])),
      ]),
      functions: list.map(result_custom_type.variants, fn(variant) {
          #(variant.name, variant)
        })
        |> dict.from_list,
    )
  let make_fn = fn(constructor_name, argument_expr) {
    glance.Definition(
      [],
      glance.Function(
        "func",
        glance.Public,
        [],
        Some(
          glance.NamedType("Result", None, [
            compiler.float_type,
            compiler.int_type,
          ]),
        ),
        [
          glance.Expression(
            glance.Call(glance.Variable(constructor_name), [
              glance.Field(None, argument_expr),
            ]),
          ),
        ],
        glance.Span(0, 1),
      ),
    )
  }
  analysis.infer_function(internals, make_fn("Ok", glance.Float("3.14")))
  |> result.is_ok
  |> should.equal(True)

  analysis.infer_function(internals, make_fn("Error", glance.Int("42")))
  |> result.is_ok
  |> should.equal(True)
}

fn infer_single_function(code: String) {
  let internals = empty_module_internals("foo", "bar")
  glance.module(code)
  |> result.replace_error(Nil)
  |> result.try(fn(module) { list.first(module.functions) })
  |> result.try(fn(def) {
    analysis.infer_function(internals, def)
    |> result.replace_error(Nil)
  })
}

pub fn external_test() {
  infer_single_function(
    "@external(javascript, \"../gleam_stdlib.mjs\", \"ceiling\")
  fn do_ceiling(a: Float) -> Float",
  )
  |> should.equal(
    Ok(analysis.Function(
      analysis.FunctionSignature(
        "do_ceiling",
        [analysis.TypeConstructor(analysis.BuiltInType(analysis.FloatType), [])],
        [],
        analysis.TypeConstructor(analysis.BuiltInType(analysis.FloatType), []),
      ),
      [glance.Named("a")],
      analysis.External("../gleam_stdlib.mjs", "ceiling"),
    )),
  )
}
