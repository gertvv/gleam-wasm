import analysis
import compiler
import glance
import gleam/dict
import gleam/option.{None, Some}
import gleam/result
import gleeunit/should
import project

pub fn resolve_basic_types_test() {
  let project =
    project.Project("test", dict.new(), fn(_a, _b) {
      Error(compiler.ReferenceError("foo"))
    })
  let location = project.SourceLocation("test", "bar")
  let default_types = dict.new()

  let int_type = analysis.TypeConstructor(analysis.BuiltInType("Int"), [])
  let float_type = analysis.TypeConstructor(analysis.BuiltInType("Float"), [])
  let list_type = fn(item_type) {
    analysis.TypeConstructor(analysis.BuiltInType("List"), [item_type])
  }

  analysis.resolve_type(
    analysis.ModuleInternals(project, location, dict.new(), default_types),
    dict.new(),
    glance.NamedType("Int", None, []),
  )
  |> should.equal(Ok(int_type))

  analysis.resolve_type(
    analysis.ModuleInternals(project, location, dict.new(), default_types),
    dict.new(),
    glance.NamedType("Float", None, []),
  )
  |> should.equal(Ok(float_type))

  analysis.resolve_type(
    analysis.ModuleInternals(project, location, dict.new(), default_types),
    dict.new(),
    glance.NamedType("MyFloat", None, []),
  )
  |> should.equal(Error(compiler.ReferenceError("MyFloat")))

  analysis.resolve_type(
    analysis.ModuleInternals(project, location, dict.new(), default_types),
    dict.new(),
    glance.NamedType("List", None, [glance.NamedType("Int", None, [])]),
  )
  |> should.equal(Ok(list_type(int_type)))

  analysis.resolve_type(
    analysis.ModuleInternals(project, location, dict.new(), default_types),
    dict.new(),
    glance.NamedType("List", None, [glance.NamedType("MyFloat", None, [])]),
  )
  |> should.equal(Error(compiler.ReferenceError("MyFloat")))

  let generic_custom_type_id = analysis.TypeFromModule(location, "CustomType")
  let generic_custom_type = analysis.Prototype(parameters: ["b", "c"])

  analysis.resolve_type(
    analysis.ModuleInternals(project, location, dict.new(), default_types),
    dict.from_list([#("CustomType", generic_custom_type)]),
    glance.NamedType("CustomType", None, [
      glance.NamedType("Float", None, []),
      glance.NamedType("Int", None, []),
    ]),
  )
  |> should.equal(
    Ok(analysis.TypeConstructor(generic_custom_type_id, [float_type, int_type])),
  )

  analysis.resolve_type(
    analysis.ModuleInternals(project, location, dict.new(), default_types),
    dict.from_list([#("CustomType", generic_custom_type)]),
    glance.NamedType("CustomType", None, [
      glance.VariableType("x"),
      glance.NamedType("Int", None, []),
    ]),
  )
  |> should.equal(
    Ok(
      analysis.TypeConstructor(generic_custom_type_id, [
        analysis.TypeVariable("x"),
        int_type,
      ]),
    ),
  )

  analysis.resolve_type(
    analysis.ModuleInternals(project, location, dict.new(), default_types),
    dict.from_list([#("CustomType", generic_custom_type)]),
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

  let int_type = analysis.TypeConstructor(analysis.BuiltInType("Int"), [])

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
          analysis.Variant("TypeA", [analysis.Field(None, int_type)]),
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

  let int_type = analysis.TypeConstructor(analysis.BuiltInType("Int"), [])

  let list_type = fn(item_type) {
    analysis.TypeConstructor(analysis.BuiltInType("List"), [item_type])
  }

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
    Ok(dict.from_list([#("Alias", analysis.GenericType(int_type, []))])),
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
      dict.from_list([#("Alias", analysis.GenericType(list_type(int_type), []))]),
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
          analysis.GenericType(list_type(analysis.TypeVariable("a")), ["a"]),
        ),
      ]),
    ),
  )
}

pub fn type_infer_function_test() {
  let int_type = analysis.TypeConstructor(analysis.BuiltInType("Int"), [])
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
        analysis.BinaryOperator(
          analysis.TypeVariable("$2"),
          glance.MultInt,
          analysis.Variable(analysis.TypeVariable("$3"), "x"),
          analysis.Variable(analysis.TypeVariable("$4"), "y"),
        ),
      ],
    )

  // TODO: from here adapt
  let constraints = [
    analysis.Equal(
      analysis.TypeVariable("$1"),
      analysis.FunctionType(
        [analysis.TypeVariable("$3"), analysis.TypeVariable("$4")],
        analysis.TypeVariable("$2"),
      ),
    ),
    analysis.Equal(analysis.TypeVariable("$2"), int_type),
    analysis.Equal(int_type, analysis.TypeVariable("$4")),
    analysis.Equal(int_type, analysis.TypeVariable("$3")),
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
      #("$2", int_type),
      #("$3", int_type),
      #("$4", int_type),
    ])

  let typed_fn =
    analysis.Fn(
      typ: analysis.FunctionType([int_type, int_type], int_type),
      argument_names: [glance.Named("x"), glance.Named("y")],
      body: [
        analysis.BinaryOperator(
          int_type,
          glance.MultInt,
          analysis.Variable(int_type, "x"),
          analysis.Variable(int_type, "y"),
        ),
      ],
    )

  let #(context, initd_fn) =
    parsed_fn
    |> analysis.init_inference(
      analysis.TypeVariable("$1"),
      dict.new(),
      analysis.Context(
        dict.from_list([#("$1", analysis.TypeVariable("$1"))]),
        [],
        2,
      ),
    )

  initd_fn |> should.equal(fn_with_blanks)
  context.constraints |> should.equal(constraints)

  let assert Ok(analysis.Context(subst, _, _)) =
    context
    |> analysis.solve_constraints

  subst
  |> should.equal(substitution)

  fn_with_blanks
  |> analysis.substitute_expression(substitution)
  |> should.equal(typed_fn)
}

pub fn type_infer_function_using_import_test() {
  let int_type = analysis.TypeConstructor(analysis.BuiltInType("Int"), [])
  let string_type = analysis.TypeConstructor(analysis.BuiltInType("String"), [])
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
        #("to_string", analysis.FunctionType([int_type], string_type)),
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
        analysis.Call(
          analysis.FunctionReference(
            analysis.FunctionType([int_type], string_type),
            module_int.location,
            "to_string",
          ),
          [analysis.Variable(analysis.TypeVariable("$3"), "x")],
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
      analysis.FunctionType([int_type], string_type),
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
      #("$2", string_type),
      #("$3", int_type),
      #("$4", analysis.TypeVariable("$3")),
    ])

  let typed_fn =
    analysis.Fn(
      typ: analysis.FunctionType([int_type], string_type),
      argument_names: [glance.Named("x")],
      body: [
        analysis.Call(
          analysis.FunctionReference(
            analysis.FunctionType([int_type], string_type),
            module_int.location,
            "to_string",
          ),
          [analysis.Variable(int_type, "x")],
        ),
      ],
    )

  let #(context, initd_fn) =
    parsed_fn
    |> analysis.init_inference(
      analysis.TypeVariable("$1"),
      dict.from_list([#("int", analysis.ModuleType(module_int))]),
      analysis.Context(
        dict.from_list([#("$1", analysis.TypeVariable("$1"))]),
        [],
        2,
      ),
    )

  initd_fn |> should.equal(fn_with_blanks)
  context.constraints |> should.equal(constraints)

  let assert Ok(analysis.Context(subst, _, _)) =
    context
    |> analysis.solve_constraints

  subst
  |> should.equal(substitution)

  fn_with_blanks
  |> analysis.substitute_expression(substitution)
  |> should.equal(typed_fn)
}
