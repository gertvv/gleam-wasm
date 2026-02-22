import gl_to_wasm/analysis
import gl_to_wasm/compiler_ng
import gl_to_wasm/project
import glance
import gleam/option.{None, Some}
import gleeunit/should

pub fn implicit_functions_none_test() {
  analysis.Function(
    analysis.FunctionSignature(
      "add",
      [analysis.int_type, analysis.int_type],
      [],
      analysis.int_type,
    ),
    [glance.Named("a"), glance.Named("b")],
    analysis.GleamBody([
      analysis.Expression(
        analysis.Call(
          analysis.FunctionReference(
            analysis.FunctionType(
              [analysis.int_type, analysis.int_type],
              analysis.int_type,
            ),
            analysis.BuiltInFunction(analysis.BinaryOperator(glance.AddInt)),
          ),
          [
            analysis.Variable(analysis.int_type, "a"),
            analysis.Variable(analysis.int_type, "b"),
          ],
        ),
      ),
    ]),
  )
  |> compiler_ng.implicit_functions
  |> should.equal([])
}

pub fn implicit_functions_anon_test() {
  let add_fn =
    analysis.Fn(
      analysis.FunctionType(
        [analysis.int_type, analysis.int_type],
        analysis.int_type,
      ),
      [glance.Named("a"), glance.Named("b")],
      [
        analysis.Expression(
          analysis.Call(
            analysis.FunctionReference(
              analysis.FunctionType(
                [analysis.int_type, analysis.int_type],
                analysis.int_type,
              ),
              analysis.BuiltInFunction(analysis.BinaryOperator(glance.AddInt)),
            ),
            [
              analysis.Variable(analysis.int_type, "a"),
              analysis.Variable(analysis.int_type, "b"),
            ],
          ),
        ),
      ],
      [],
    )
  analysis.Function(
    analysis.FunctionSignature(
      "sum",
      [analysis.list_type(analysis.int_type)],
      [],
      analysis.int_type,
    ),
    [glance.Named("list")],
    analysis.GleamBody([
      analysis.Expression(
        analysis.Call(
          analysis.FunctionReference(
            analysis.FunctionType(
              [
                analysis.list_type(analysis.int_type),
                analysis.int_type,
                analysis.FunctionType(
                  [analysis.int_type, analysis.int_type],
                  analysis.int_type,
                ),
              ],
              analysis.int_type,
            ),
            analysis.FunctionFromModule(
              project.ModuleId("some", "module"),
              "fold_int",
            ),
          ),
          [
            analysis.Variable(analysis.list_type(analysis.int_type), "list"),
            analysis.Int("0"),
            add_fn,
          ],
        ),
      ),
    ]),
  )
  |> compiler_ng.implicit_functions
  |> should.equal([
    compiler_ng.AnonymousFunction(
      add_fn.typ,
      add_fn.argument_names,
      add_fn.body,
      add_fn.captures,
    ),
  ])
}

pub fn implicit_function_wrap_in_closure_test() {
  analysis.Function(
    analysis.FunctionSignature(
      "sum",
      [analysis.list_type(analysis.int_type)],
      [],
      analysis.int_type,
    ),
    [glance.Named("list")],
    analysis.GleamBody([
      analysis.Expression(
        analysis.Call(
          analysis.FunctionReference(
            analysis.FunctionType(
              [
                analysis.list_type(analysis.int_type),
                analysis.int_type,
                analysis.FunctionType(
                  [analysis.int_type, analysis.int_type],
                  analysis.int_type,
                ),
              ],
              analysis.int_type,
            ),
            analysis.FunctionFromModule(
              project.ModuleId("some", "module"),
              "fold_int",
            ),
          ),
          [
            analysis.Variable(analysis.list_type(analysis.int_type), "list"),
            analysis.Int("0"),
            analysis.FunctionReference(
              analysis.FunctionType(
                [analysis.int_type, analysis.int_type],
                analysis.int_type,
              ),
              analysis.FunctionFromModule(
                project.ModuleId("gleam_stdlib", "gleam/int"),
                "add",
              ),
            ),
          ],
        ),
      ),
    ]),
  )
  |> compiler_ng.implicit_functions
  |> should.equal([
    compiler_ng.FunctionAsClosure(analysis.FunctionFromModule(
      project.ModuleId("gleam_stdlib", "gleam/int"),
      "add",
    )),
  ])
}

pub fn implicit_function_wrap_and_map_args_test() {
  let add_func =
    analysis.FunctionReference(
      analysis.FunctionType(
        [analysis.int_type, analysis.int_type],
        analysis.int_type,
      ),
      analysis.FunctionFromModule(
        project.ModuleId("gleam_stdlib", "gleam/int"),
        "add",
      ),
    )
  analysis.Function(
    analysis.FunctionSignature(
      "sum",
      [analysis.list_type(analysis.int_type)],
      [],
      analysis.int_type,
    ),
    [glance.Named("list")],
    analysis.GleamBody([
      analysis.Expression(
        analysis.Call(
          analysis.FunctionReference(
            analysis.FunctionType(
              [
                analysis.list_type(analysis.TypeVariable("a")),
                analysis.TypeVariable("b"),
                analysis.FunctionType(
                  [analysis.TypeVariable("a"), analysis.TypeVariable("a")],
                  analysis.TypeVariable("b"),
                ),
              ],
              analysis.TypeVariable("b"),
            ),
            analysis.FunctionFromModule(
              project.ModuleId("gleam_stdlib", "gleam/list"),
              "fold",
            ),
          ),
          [
            analysis.Variable(analysis.list_type(analysis.int_type), "list"),
            analysis.Int("0"),
            add_func,
          ],
        ),
      ),
    ]),
  )
  |> compiler_ng.implicit_functions
  |> should.equal([
    compiler_ng.ConcreteArgsAsGeneric(add_func, [
      Some(analysis.int_type),
      Some(analysis.int_type),
    ]),
  ])
}
