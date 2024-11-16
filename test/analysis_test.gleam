import analysis
import compiler
import glance
import gleam/dict
import gleam/option.{None, Some}
import gleam/pair
import gleam/result
import gleeunit/should
import pprint
import project

fn empty_module_internals(package_name, module_path) {
  let project =
    project.Project(package_name, dict.new(), fn(_a, _b) {
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
  )
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
    project.Project("test", dict.new(), fn(_a, _b) {
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
      type_aliases: [],
      constants: [],
      functions: [],
    ),
  )
  |> should.equal(Ok(#(dict.new(), dict.new())))

  let b_depends_on_a =
    dict.from_list([
      #(
        "TypeA",
        analysis.CustomType([], False, [
          analysis.Variant("TypeA", [analysis.Field(None, analysis.int_type)]),
        ]),
      ),
      #(
        "TypeB",
        analysis.CustomType([], False, [
          analysis.Variant("TypeB", [
            analysis.Field(
              None,
              analysis.TypeConstructor(
                analysis.TypeFromModule(location, "TypeA"),
                [],
              ),
            ),
          ]),
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
  |> result.map(fn(r) { r.1 })
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
  |> result.map(fn(r) { r.1 })
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
  |> result.map(fn(r) { r.1 })
  |> should.equal(
    Ok(
      dict.from_list([
        #(
          "Tree",
          analysis.CustomType([], False, [
            analysis.Variant("Node", [
              analysis.Field(
                Some("left"),
                analysis.TypeConstructor(
                  analysis.TypeFromModule(location, "Tree"),
                  [],
                ),
              ),
              analysis.Field(
                Some("right"),
                analysis.TypeConstructor(
                  analysis.TypeFromModule(location, "Tree"),
                  [],
                ),
              ),
            ]),
            analysis.Variant("Leaf", []),
          ]),
        ),
      ]),
    ),
  )
}

pub fn resolve_type_aliases_test() {
  let project =
    project.Project("test", dict.new(), fn(_a, _b) {
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
  |> result.map(fn(r) { r.0 })
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
  |> result.map(fn(r) { r.0 })
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
  |> result.map(fn(r) { r.0 })
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
          analysis.FunctionType([analysis.int_type], analysis.string_type),
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
          analysis.FunctionType([analysis.TypeVariable("a")], option_type),
        ),
      ]),
    )

  // fn(x) { Some(x) }
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
        analysis.FunctionType([analysis.TypeVariable("a")], option_type),
        [glance.Named("x")],
        [
          analysis.Expression(
            analysis.Call(
              analysis.FunctionReference(
                analysis.FunctionType([analysis.TypeVariable("a")], option_type),
                analysis.FunctionFromModule(option_module_id, "Some"),
              ),
              [analysis.Variable(analysis.TypeVariable("a"), "x")],
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
          analysis.Assignment("x", analysis.Int("42")),
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

pub fn type_infer_field_access_test() {
  let module_id = project.SourceLocation("foo", "bar")
  let my_type =
    analysis.TypeConstructor(analysis.TypeFromModule(module_id, "MyType"), [])
  let my_type_definition =
    analysis.CustomType([], False, [
      analysis.Variant("VariantA", [
        analysis.Field(Some("a"), analysis.int_type),
      ]),
      analysis.Variant("VariantB", [
        analysis.Field(Some("a"), analysis.int_type),
        analysis.Field(Some("b"), analysis.float_type),
      ]),
    ])
  let internals =
    analysis.ModuleInternals(
      project: project.Project("name", dict.new(), fn(_, _) { panic }),
      location: module_id,
      imports: dict.new(),
      types: dict.from_list([#("MyType", analysis.GenericType(my_type, []))]),
      custom_types: dict.from_list([#("MyType", my_type_definition)]),
      functions: dict.from_list([
        #("VariantA", analysis.FunctionType([analysis.int_type], my_type)),
        #(
          "VariantB",
          analysis.FunctionType(
            [analysis.int_type, analysis.float_type],
            my_type,
          ),
        ),
      ]),
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
      analysis.Call(
        analysis.FunctionReference(
          analysis.FunctionType([my_type], analysis.int_type),
          analysis.BuiltInFunction(analysis.AccessField("a")),
        ),
        [
          analysis.Call(
            analysis.FunctionReference(
              analysis.FunctionType([analysis.int_type], my_type),
              analysis.FunctionFromModule(module_id, "VariantA"),
            ),
            [analysis.Int("42")],
          ),
        ],
      ),
    ),
  )
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
// TODO: pipe to FnCapture, pipe to Fn
