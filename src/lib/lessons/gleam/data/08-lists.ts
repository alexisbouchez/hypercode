import type { Lesson } from "../../types";

export const lists: Lesson = {
  id: "lists",
  title: "Lists",
  chapterId: "collections",
  content: `## Lists in Gleam

Lists are ordered collections of values of the same type:

\`\`\`gleam
let numbers = [1, 2, 3, 4, 5]
let names = ["Alice", "Bob", "Charlie"]
let empty: List(Int) = []
\`\`\`

Lists in Gleam are linked lists -- they are efficient for prepending and pattern matching, but not for random access.

### Prepending

You can add an element to the front of a list with \`[x, ..rest]\`:

\`\`\`gleam
let numbers = [2, 3, 4]
let more = [1, ..numbers]  // [1, 2, 3, 4]
\`\`\`

### The List Module

The \`gleam/list\` module provides many useful functions:

\`\`\`gleam
import gleam/list

list.length([1, 2, 3])                    // 3
list.reverse([1, 2, 3])                   // [3, 2, 1]
list.contains([1, 2, 3], 2)               // True
list.append([1, 2], [3, 4])               // [1, 2, 3, 4]
list.flatten([[1, 2], [3, 4]])             // [1, 2, 3, 4]
list.first([1, 2, 3])                     // Ok(1)
list.rest([1, 2, 3])                      // Ok([2, 3])
list.sort([3, 1, 2], int.compare)         // [1, 2, 3]
list.unique([1, 2, 2, 3, 3])              // [1, 2, 3]
list.zip([1, 2], ["a", "b"])              // [#(1, "a"), #(2, "b")]
\`\`\`

### Recursive Processing

Since Gleam has no loops, you process lists with recursion or library functions:

\`\`\`gleam
fn sum(numbers: List(Int)) -> Int {
  case numbers {
    [] -> 0
    [first, ..rest] -> first + sum(rest)
  }
}
\`\`\`

### Range-Like Behavior

Gleam does not have a range syntax. You can use \`list.range\`:

\`\`\`gleam
list.range(1, 5)  // [1, 2, 3, 4, 5]
\`\`\`

### Your Task

Write a function called \`sum\` that takes a list of integers and returns their sum using recursion. Print the sum of \`[1, 2, 3, 4, 5]\` and the sum of an empty list.`,

  starterCode: `import gleam/io
import gleam/int

fn sum(numbers: List(Int)) -> Int {
\t// Calculate the sum using recursion
\t0
}

pub fn main() {
\tio.println(int.to_string(sum([1, 2, 3, 4, 5])))
\tio.println(int.to_string(sum([])))
}
`,

  solution: `import gleam/io
import gleam/int

fn sum(numbers: List(Int)) -> Int {
\tcase numbers {
\t\t[] -> 0
\t\t[first, ..rest] -> first + sum(rest)
\t}
}

pub fn main() {
\tio.println(int.to_string(sum([1, 2, 3, 4, 5])))
\tio.println(int.to_string(sum([])))
}
`,

  tests: [
    {
      name: "sums lists correctly",
      expected: "15\n0\n",
    },
  ],
};
