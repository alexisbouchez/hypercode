import type { Lesson } from "../../types";

export const higherOrderFunctions: Lesson = {
  id: "higher-order-functions",
  title: "Higher-Order Functions",
  chapterId: "functional-patterns",
  content: `## Higher-Order Functions

A higher-order function is a function that takes other functions as arguments or returns functions. Gleam uses them extensively instead of loops.

### Anonymous Functions

You create anonymous functions with the \`fn\` keyword:

\`\`\`gleam
let double = fn(x: Int) -> Int { x * 2 }
double(5)  // 10
\`\`\`

### list.map

Transforms every element in a list:

\`\`\`gleam
list.map([1, 2, 3], fn(x) { x * 2 })       // [2, 4, 6]
list.map(["a", "b"], string.uppercase)       // ["A", "B"]
\`\`\`

### list.filter

Keeps only elements that satisfy a condition:

\`\`\`gleam
list.filter([1, 2, 3, 4, 5], fn(x) { x > 3 })    // [4, 5]
list.filter([1, 2, 3, 4], int.is_even)              // [2, 4]
\`\`\`

### list.fold

Reduces a list to a single value by applying a function to each element and an accumulator:

\`\`\`gleam
// Sum: start at 0, add each element
list.fold([1, 2, 3, 4, 5], 0, fn(acc, x) { acc + x })
// 15

// Product: start at 1, multiply each element
list.fold([1, 2, 3, 4, 5], 1, fn(acc, x) { acc * x })
// 120

// Build a string
list.fold(["a", "b", "c"], "", fn(acc, x) { acc <> x })
// "abc"
\`\`\`

The accumulator \`acc\` starts with the initial value and is updated by the function for each element.

### list.each

Runs a side effect for each element (returns \`Nil\`):

\`\`\`gleam
list.each([1, 2, 3], fn(x) {
  io.println(int.to_string(x))
})
\`\`\`

### Combining Higher-Order Functions

The real power comes from combining these functions:

\`\`\`gleam
// Sum of squares of even numbers
[1, 2, 3, 4, 5, 6]
|> list.filter(int.is_even)
|> list.map(fn(x) { x * x })
|> list.fold(0, fn(acc, x) { acc + x })
// 56
\`\`\`

### Function References

You can pass named functions directly -- no need for wrappers:

\`\`\`gleam
list.map(["hello", "world"], string.uppercase)
// Same as: list.map(["hello", "world"], fn(s) { string.uppercase(s) })
\`\`\`

### Your Task

Given a list of integers from 1 to 10, use \`list.filter\` to keep only the odd numbers, then use \`list.map\` to triple each one, and finally use \`list.fold\` to sum them all. Print the result.

The odd numbers are 1, 3, 5, 7, 9. Tripled: 3, 9, 15, 21, 27. Sum: 75.`,

  starterCode: `import gleam/io
import gleam/int
import gleam/list

pub fn main() {
\t// Filter odd, triple, then sum [1..10]
}
`,

  solution: `import gleam/io
import gleam/int
import gleam/list

pub fn main() {
\tlet result =
\t\t[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
\t\t|> list.filter(int.is_odd)
\t\t|> list.map(fn(x) { x * 3 })
\t\t|> list.fold(0, fn(acc, x) { acc + x })
\tio.println(int.to_string(result))
}
`,

  tests: [
    {
      name: "filters, maps, and folds correctly",
      expected: "75\n",
    },
  ],
};
