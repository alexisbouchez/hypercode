import type { Lesson } from "../../types";

export const patternMatching: Lesson = {
  id: "pattern-matching",
  title: "Pattern Matching",
  chapterId: "control-flow",
  content: `## Destructuring with Patterns

Pattern matching in Gleam goes beyond simple value comparison. You can destructure complex data structures and bind parts to variables.

### String Patterns

You can match on string prefixes:

\`\`\`gleam
case name {
  "Dr. " <> rest -> "Doctor: " <> rest
  "Mr. " <> rest -> "Mister: " <> rest
  name -> "Person: " <> name
}
\`\`\`

### List Patterns

You can destructure lists to access their elements:

\`\`\`gleam
case my_list {
  [] -> "empty"
  [x] -> "one element: " <> int.to_string(x)
  [x, y] -> "two elements"
  [first, ..rest] -> "first is " <> int.to_string(first)
}
\`\`\`

The \`[first, ..rest]\` pattern binds the first element and the remaining list.

### Tuple Patterns

Tuples can be destructured in case expressions:

\`\`\`gleam
case #(x, y) {
  #(0, 0) -> "origin"
  #(x, 0) -> "on x-axis"
  #(0, y) -> "on y-axis"
  #(x, y) -> "at point"
}
\`\`\`

### Variable Binding

When you use a name in a pattern, it binds the matched value:

\`\`\`gleam
case some_value {
  value if value > 10 -> "big: " <> int.to_string(value)
  value -> "small: " <> int.to_string(value)
}
\`\`\`

### Let Patterns

You can also pattern match in \`let\` bindings:

\`\`\`gleam
let #(first, second) = #("hello", "world")
// first = "hello", second = "world"
\`\`\`

### Exhaustiveness

Gleam requires case expressions to be exhaustive -- you must handle every possible value. The compiler will tell you if you miss a case. This prevents bugs caused by unhandled scenarios.

### Your Task

Write a function called \`describe_list\` that takes a list of integers and returns:
- \`"empty"\` for an empty list
- \`"singleton: <n>"\` for a list with one element
- \`"pair: <a>, <b>"\` for a list with two elements
- \`"long: starts with <n>"\` for a list with three or more elements

Print the result for four different lists.`,

  starterCode: `import gleam/io
import gleam/int

fn describe_list(items: List(Int)) -> String {
\t// Use pattern matching on the list
\t""
}

pub fn main() {
\tio.println(describe_list([]))
\tio.println(describe_list([42]))
\tio.println(describe_list([1, 2]))
\tio.println(describe_list([10, 20, 30]))
}
`,

  solution: `import gleam/io
import gleam/int

fn describe_list(items: List(Int)) -> String {
\tcase items {
\t\t[] -> "empty"
\t\t[n] -> "singleton: " <> int.to_string(n)
\t\t[a, b] -> "pair: " <> int.to_string(a) <> ", " <> int.to_string(b)
\t\t[n, ..] -> "long: starts with " <> int.to_string(n)
\t}
}

pub fn main() {
\tio.println(describe_list([]))
\tio.println(describe_list([42]))
\tio.println(describe_list([1, 2]))
\tio.println(describe_list([10, 20, 30]))
}
`,

  tests: [
    {
      name: "describes lists correctly",
      expected: "empty\nsingleton: 42\npair: 1, 2\nlong: starts with 10\n",
    },
  ],
};
