import type { Lesson } from "../../types";

export const genericTypes: Lesson = {
  id: "generic-types",
  title: "Generic Types",
  chapterId: "custom-types",
  content: `## Type Parameters

Generic types let you write code that works with any type. You define them using type parameters in parentheses:

\`\`\`gleam
pub type Box(a) {
  Box(value: a)
}
\`\`\`

The \`a\` is a type parameter. You can create boxes containing any type:

\`\`\`gleam
let int_box = Box(value: 42)         // Box(Int)
let str_box = Box(value: "hello")    // Box(String)
\`\`\`

### Generic Functions

Functions can also use type parameters:

\`\`\`gleam
fn identity(x: a) -> a {
  x
}

fn apply(value: a, f: fn(a) -> b) -> b {
  f(value)
}
\`\`\`

The type parameter \`a\` means "any type." The compiler ensures consistency -- if a function takes \`a\` and returns \`a\`, both must be the same type.

### Built-in Generic Types

Several built-in types are generic:

- \`List(a)\` -- a list of any type
- \`Result(value, error)\` -- success or failure with typed values
- \`Option(a)\` -- a value that may or may not exist

### Multiple Type Parameters

Types can have multiple parameters:

\`\`\`gleam
pub type Pair(a, b) {
  Pair(first: a, second: b)
}

let p = Pair(first: 1, second: "hello")  // Pair(Int, String)
\`\`\`

### Pattern Matching on Generic Types

Pattern matching works the same way with generic types:

\`\`\`gleam
fn unwrap_or(box: Box(a), default: a) -> a {
  case box {
    Box(value: v) -> v
  }
}
\`\`\`

### Your Task

Define a generic \`Wrapper\` type with a \`value\` field. Write a function \`wrap\` that takes a value and returns it wrapped, and a function \`map_wrapper\` that applies a function to the wrapped value. Use them to wrap an integer, double it, and print the result.`,

  starterCode: `import gleam/io
import gleam/int

pub type Wrapper(a) {
\t// Define the Wrapper type
}

fn wrap(value: a) -> Wrapper(a) {
\t// Wrap the value
\tWrapper(value: value)
}

fn map_wrapper(wrapper: Wrapper(a), f: fn(a) -> b) -> Wrapper(b) {
\t// Apply f to the wrapped value
\tWrapper(value: f(wrapper.value))
}

pub fn main() {
\tlet w = wrap(21)
\tlet doubled = map_wrapper(w, fn(x) { x * 2 })
\tio.println(int.to_string(doubled.value))
}
`,

  solution: `import gleam/io
import gleam/int

pub type Wrapper(a) {
\tWrapper(value: a)
}

fn wrap(value: a) -> Wrapper(a) {
\tWrapper(value: value)
}

fn map_wrapper(wrapper: Wrapper(a), f: fn(a) -> b) -> Wrapper(b) {
\tWrapper(value: f(wrapper.value))
}

pub fn main() {
\tlet w = wrap(21)
\tlet doubled = map_wrapper(w, fn(x) { x * 2 })
\tio.println(int.to_string(doubled.value))
}
`,

  tests: [
    {
      name: "wraps and maps correctly",
      expected: "42\n",
    },
  ],
};
