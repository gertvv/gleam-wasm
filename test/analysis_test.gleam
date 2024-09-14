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
  let location = project.SourceLocation("test", "bar", "bar")
  let default_types = dict.new()

  analysis.resolve_type(
    analysis.ModuleInternals(project, location, dict.new(), default_types),
    [],
    glance.NamedType("Int", None, []),
  )
  |> should.equal(Ok(analysis.IntType))

  analysis.resolve_type(
    analysis.ModuleInternals(project, location, dict.new(), default_types),
    [],
    glance.NamedType("Float", None, []),
  )
  |> should.equal(Ok(analysis.FloatType))

  analysis.resolve_type(
    analysis.ModuleInternals(project, location, dict.new(), default_types),
    [],
    glance.NamedType("MyFloat", None, []),
  )
  |> should.equal(Error(compiler.ReferenceError("MyFloat")))

  analysis.resolve_type(
    analysis.ModuleInternals(project, location, dict.new(), default_types),
    [],
    glance.NamedType("List", None, [glance.NamedType("Int", None, [])]),
  )
  |> should.equal(Ok(analysis.ListType(analysis.IntType)))

  analysis.resolve_type(
    analysis.ModuleInternals(project, location, dict.new(), default_types),
    [],
    glance.NamedType("List", None, [glance.NamedType("MyFloat", None, [])]),
  )
  |> should.equal(Error(compiler.ReferenceError("MyFloat")))

  let generic_custom_type =
    analysis.CustomType(
      id: analysis.TypeId(location, "CustomType"),
      publicity: glance.Public,
      opaque_: False,
      parameters: ["b", "c"],
      variants: [
        analysis.Variant(name: "CustomType", fields: [
          analysis.Field(
            label: None,
            item: analysis.ListType(analysis.VariableType("b")),
          ),
          analysis.Field(label: None, item: analysis.VariableType("c")),
          analysis.Field(label: None, item: analysis.VariableType("b")),
        ]),
      ],
    )
  analysis.resolve_type(
    analysis.ModuleInternals(
      project,
      location,
      dict.new(),
      dict.insert(default_types, "CustomType", generic_custom_type),
    ),
    [],
    glance.NamedType("CustomType", None, [
      glance.NamedType("Float", None, []),
      glance.NamedType("Int", None, []),
    ]),
  )
  |> should.equal(
    Ok(analysis.IdentifiedType(
      generic_custom_type.id,
      dict.from_list([#("b", analysis.FloatType), #("c", analysis.IntType)]),
    )),
  )

  analysis.resolve_type(
    analysis.ModuleInternals(
      project,
      location,
      dict.new(),
      dict.insert(default_types, "CustomType", generic_custom_type),
    ),
    ["x"],
    glance.NamedType("CustomType", None, [
      glance.VariableType("x"),
      glance.NamedType("Int", None, []),
    ]),
  )
  |> should.equal(
    Ok(analysis.IdentifiedType(
      generic_custom_type.id,
      dict.from_list([
        #("b", analysis.VariableType("x")),
        #("c", analysis.IntType),
      ]),
    )),
  )

  analysis.resolve_type(
    analysis.ModuleInternals(
      project,
      location,
      dict.new(),
      dict.insert(default_types, "CustomType", generic_custom_type),
    ),
    ["x"],
    glance.NamedType("CustomType", None, [glance.VariableType("x")]),
  )
  |> should.equal(
    Error(compiler.TypeArityError(
      glance.NamedType("CustomType", None, [glance.VariableType("x")]),
      2,
      1,
    )),
  )

  analysis.resolve_type(
    analysis.ModuleInternals(
      project,
      location,
      dict.new(),
      dict.insert(default_types, "CustomType", generic_custom_type),
    ),
    ["x"],
    glance.NamedType("CustomType", None, [
      glance.VariableType("y"),
      glance.NamedType("Int", None, []),
    ]),
  )
  |> should.equal(Error(compiler.ReferenceError("y")))
}

pub fn resolve_dependent_custom_types_test() {
  let project =
    project.Project("test", dict.new(), fn(_a, _b) {
      Error(compiler.ReferenceError("foo"))
    })
  let location = project.SourceLocation("test", "bar", "bar")

  analysis.resolve_types(
    project,
    location,
    dict.new(),
    glance.Module(
      imports: [],
      custom_types: [],
      type_aliases: [],
      constants: [],
      functions: [],
    ),
  )
  |> result.map(fn(a) { a.types })
  |> should.equal(Ok(dict.new()))

  let b_depends_on_a =
    dict.from_list([
      #(
        "TypeA",
        analysis.CustomType(
          analysis.TypeId(location, "TypeA"),
          glance.Public,
          False,
          [],
          [analysis.Variant("TypeA", [analysis.Field(None, analysis.IntType)])],
        ),
      ),
      #(
        "TypeB",
        analysis.CustomType(
          analysis.TypeId(location, "TypeB"),
          glance.Public,
          False,
          [],
          [
            analysis.Variant("TypeB", [
              analysis.Field(
                None,
                analysis.IdentifiedType(
                  analysis.TypeId(location, "TypeA"),
                  dict.new(),
                ),
              ),
            ]),
          ],
        ),
      ),
    ])

  analysis.resolve_types(
    project,
    location,
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
  |> result.map(fn(a) { a.types })
  |> should.equal(Ok(b_depends_on_a))

  analysis.resolve_types(
    project,
    location,
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
  |> result.map(fn(a) { a.types })
  |> should.equal(Ok(b_depends_on_a))

  analysis.resolve_types(
    project,
    location,
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
  |> result.map(fn(a) { a.types })
  |> should.equal(
    Ok(
      dict.from_list([
        #(
          "Tree",
          analysis.CustomType(
            analysis.TypeId(location, "Tree"),
            glance.Public,
            False,
            [],
            [
              analysis.Variant("Node", [
                analysis.Field(
                  Some("left"),
                  analysis.IdentifiedType(
                    analysis.TypeId(location, "Tree"),
                    dict.new(),
                  ),
                ),
                analysis.Field(
                  Some("right"),
                  analysis.IdentifiedType(
                    analysis.TypeId(location, "Tree"),
                    dict.new(),
                  ),
                ),
              ]),
              analysis.Variant("Leaf", []),
            ],
          ),
        ),
      ]),
    ),
  )
}
