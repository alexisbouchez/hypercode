import type { Lesson } from "../../types";

export const useExpressions: Lesson = {
  id: "use-expressions",
  title: "Use Expressions",
  chapterId: "error-handling",
  content: `## The Use Expression

Gleam's \`use\` expression is syntactic sugar for callback-based patterns. It flattens nested callbacks into sequential-looking code.

### The Problem

Chaining \`result.try\` calls leads to deeply nested code:

\`\`\`gleam
fn process() -> Result(Int, String) {
  result.try(step_one(), fn(a) {
    result.try(step_two(a), fn(b) {
      result.try(step_three(b), fn(c) {
        Ok(c + 1)
      })
    })
  })
}
\`\`\`

### The Solution: use

The \`use\` expression flattens this into:

\`\`\`gleam
fn process() -> Result(Int, String) {
  use a <- result.try(step_one())
  use b <- result.try(step_two(a))
  use c <- result.try(step_three(b))
  Ok(c + 1)
}
\`\`\`

Each \`use\` line binds the success value. If any step returns \`Error\`, execution stops and the error propagates.

### How It Works

\`use x <- f(arg)\` is syntactic sugar for \`f(arg, fn(x) { ... })\`. The rest of the function body becomes the callback. The function \`f\` must accept a callback as its last argument.

### use with Other Functions

\`use\` is not limited to \`result.try\`. Any function that takes a callback as its last argument works:

\`\`\`gleam
import gleam/bool

// bool.guard runs the callback only if the condition is False
use <- bool.guard(when: list.is_empty(items), return: 0)
// This code only runs if items is not empty
calculate_sum(items)
\`\`\`

### use with Multiple Values

Some callbacks receive multiple values:

\`\`\`gleam
use first, rest <- pop_front(my_list)
// first and rest are now available
\`\`\`

### Your Task

Write a function \`parse_and_add\` that takes two strings, parses each as an integer using \`int.parse\`, and returns their sum. Use \`use\` with \`result.try\` to chain the operations. If either string is not a valid integer, return an error.

Print the results for \`("10", "20")\` and \`("10", "abc")\`.`,

  starterCode: `import gleam/io
import gleam/int
import gleam/result

fn parse_and_add(a: String, b: String) -> Result(Int, Nil) {
\t// Parse both strings and add them using use
\tOk(0)
}

fn print_result(r: Result(Int, Nil)) -> Nil {
\tcase r {
\t\tOk(value) -> io.println("Ok: " <> int.to_string(value))
\t\tError(_) -> io.println("Error: not a number")
\t}
}

pub fn main() {
\tprint_result(parse_and_add("10", "20"))
\tprint_result(parse_and_add("10", "abc"))
}
`,

  solution: `import gleam/io
import gleam/int
import gleam/result

fn parse_and_add(a: String, b: String) -> Result(Int, Nil) {
\tuse x <- result.try(int.parse(a))
\tuse y <- result.try(int.parse(b))
\tOk(x + y)
}

fn print_result(r: Result(Int, Nil)) -> Nil {
\tcase r {
\t\tOk(value) -> io.println("Ok: " <> int.to_string(value))
\t\tError(_) -> io.println("Error: not a number")
\t}
}

pub fn main() {
\tprint_result(parse_and_add("10", "20"))
\tprint_result(parse_and_add("10", "abc"))
}
`,

  tests: [
    {
      name: "parses and adds or returns error",
      expected: "Ok: 30\nError: not a number\n",
    },
  ],
};
