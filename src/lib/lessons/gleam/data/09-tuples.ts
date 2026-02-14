import type { Lesson } from "../../types";

export const tuples: Lesson = {
  id: "tuples",
  title: "Tuples",
  chapterId: "collections",
  content: `## Tuples

Tuples are fixed-size collections that can hold values of different types:

\`\`\`gleam
let pair = #(1, "hello")
let triple = #(True, 42, "world")
\`\`\`

Unlike lists, tuples can contain mixed types and have a fixed size known at compile time.

### Accessing Tuple Elements

Use pattern matching to access tuple elements:

\`\`\`gleam
let point = #(3, 4)
let #(x, y) = point
// x = 3, y = 4
\`\`\`

You can also access elements by index with \`.0\`, \`.1\`, etc.:

\`\`\`gleam
let point = #(3, 4)
point.0  // 3
point.1  // 4
\`\`\`

### Multiple Return Values

Tuples are commonly used to return multiple values from a function:

\`\`\`gleam
fn min_max(numbers: List(Int)) -> #(Int, Int) {
  // Return both minimum and maximum
  #(minimum, maximum)
}

let #(min, max) = min_max([3, 1, 4, 1, 5])
\`\`\`

### Pattern Matching with Tuples

You can pattern match on tuples in case expressions:

\`\`\`gleam
case #(status, value) {
  #("ok", v) -> "Success: " <> v
  #("error", msg) -> "Error: " <> msg
  #(_, _) -> "Unknown"
}
\`\`\`

### The Pair Module

The \`gleam/pair\` module provides utilities for 2-element tuples:

\`\`\`gleam
import gleam/pair

pair.first(#(1, "a"))      // 1
pair.second(#(1, "a"))     // "a"
pair.swap(#(1, "a"))       // #("a", 1)
pair.map_first(#(1, "a"), fn(x) { x + 1 })  // #(2, "a")
\`\`\`

### Your Task

Write a function called \`swap\` that takes a tuple of two strings and returns them swapped. Write another function called \`format_pair\` that takes a \`#(String, String)\` tuple and returns it as \`"(first, second)"\`.

Print the formatted original and swapped pairs.`,

  starterCode: `import gleam/io

fn swap(pair: #(String, String)) -> #(String, String) {
\t// Swap the elements
\t#("", "")
}

fn format_pair(pair: #(String, String)) -> String {
\t// Format as "(first, second)"
\t""
}

pub fn main() {
\tlet pair = #("hello", "world")
\tio.println(format_pair(pair))
\tio.println(format_pair(swap(pair)))
}
`,

  solution: `import gleam/io

fn swap(pair: #(String, String)) -> #(String, String) {
\tlet #(a, b) = pair
\t#(b, a)
}

fn format_pair(pair: #(String, String)) -> String {
\tlet #(a, b) = pair
\t"(" <> a <> ", " <> b <> ")"
}

pub fn main() {
\tlet pair = #("hello", "world")
\tio.println(format_pair(pair))
\tio.println(format_pair(swap(pair)))
}
`,

  tests: [
    {
      name: "formats and swaps pairs",
      expected: "(hello, world)\n(world, hello)\n",
    },
  ],
};
