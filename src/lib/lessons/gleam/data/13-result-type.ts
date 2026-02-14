import type { Lesson } from "../../types";

export const resultType: Lesson = {
  id: "result-type",
  title: "The Result Type",
  chapterId: "error-handling",
  content: `## Error Handling with Result

Gleam has no exceptions. Instead, functions that can fail return a \`Result\` type:

\`\`\`gleam
pub type Result(value, error) {
  Ok(value)
  Error(error)
}
\`\`\`

A \`Result\` is either \`Ok\` with a success value, or \`Error\` with an error value.

### Returning Results

\`\`\`gleam
fn divide(a: Int, b: Int) -> Result(Int, String) {
  case b {
    0 -> Error("division by zero")
    _ -> Ok(a / b)
  }
}
\`\`\`

### Handling Results

Use \`case\` to handle both outcomes:

\`\`\`gleam
case divide(10, 3) {
  Ok(value) -> io.println("Result: " <> int.to_string(value))
  Error(msg) -> io.println("Error: " <> msg)
}
\`\`\`

### The Result Module

The \`gleam/result\` module provides functions for working with results:

\`\`\`gleam
import gleam/result

// Transform the Ok value
result.map(Ok(5), fn(x) { x * 2 })           // Ok(10)
result.map(Error("oops"), fn(x) { x * 2 })   // Error("oops")

// Chain operations that return Results
result.try(Ok(5), fn(x) { Ok(x * 2) })       // Ok(10)
result.try(Error("oops"), fn(x) { Ok(x * 2) }) // Error("oops")

// Provide a default value
result.unwrap(Ok(5), 0)          // 5
result.unwrap(Error("oops"), 0)  // 0
\`\`\`

### Chaining with result.try

\`result.try\` is key for chaining operations. If any step returns \`Error\`, the chain short-circuits:

\`\`\`gleam
fn process() -> Result(Int, String) {
  result.try(parse_int("42"), fn(x) {
    result.try(divide(x, 2), fn(y) {
      Ok(y + 1)
    })
  })
}
\`\`\`

This nesting can get deep, which is why Gleam provides the \`use\` expression (next lesson).

### Your Task

Write a function called \`safe_divide\` that takes two integers and returns \`Result(Int, String)\`. It should return \`Error("division by zero")\` when the divisor is 0, otherwise \`Ok\` with the result.

Write a function called \`try_divide_and_add\` that divides \`a\` by \`b\`, then adds \`c\` to the result using \`result.map\`. Print the results for \`(10, 2, 3)\` and \`(10, 0, 3)\`.`,

  starterCode: `import gleam/io
import gleam/int
import gleam/result

fn safe_divide(a: Int, b: Int) -> Result(Int, String) {
\t// Return Ok or Error
\tOk(0)
}

fn try_divide_and_add(a: Int, b: Int, c: Int) -> Result(Int, String) {
\t// Divide a by b, then add c
\tOk(0)
}

fn print_result(r: Result(Int, String)) -> Nil {
\tcase r {
\t\tOk(value) -> io.println("Ok: " <> int.to_string(value))
\t\tError(msg) -> io.println("Error: " <> msg)
\t}
}

pub fn main() {
\tprint_result(try_divide_and_add(10, 2, 3))
\tprint_result(try_divide_and_add(10, 0, 3))
}
`,

  solution: `import gleam/io
import gleam/int
import gleam/result

fn safe_divide(a: Int, b: Int) -> Result(Int, String) {
\tcase b {
\t\t0 -> Error("division by zero")
\t\t_ -> Ok(a / b)
\t}
}

fn try_divide_and_add(a: Int, b: Int, c: Int) -> Result(Int, String) {
\tsafe_divide(a, b)
\t|> result.map(fn(x) { x + c })
}

fn print_result(r: Result(Int, String)) -> Nil {
\tcase r {
\t\tOk(value) -> io.println("Ok: " <> int.to_string(value))
\t\tError(msg) -> io.println("Error: " <> msg)
\t}
}

pub fn main() {
\tprint_result(try_divide_and_add(10, 2, 3))
\tprint_result(try_divide_and_add(10, 0, 3))
}
`,

  tests: [
    {
      name: "handles division results",
      expected: "Ok: 8\nError: division by zero\n",
    },
  ],
};
