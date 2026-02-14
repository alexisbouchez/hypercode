import type { Lesson } from "../../types";

export const pipeOperator: Lesson = {
  id: "pipe-operator",
  title: "The Pipe Operator",
  chapterId: "functional-patterns",
  content: `## The Pipe Operator |>

The pipe operator \`|>\` passes the result of one expression as the first argument to the next function. It turns nested function calls into a readable chain.

### Without Pipes

Nested calls are read from the inside out:

\`\`\`gleam
string.join(list.map(string.split("hello world", " "), string.uppercase), "-")
\`\`\`

### With Pipes

Pipes read left to right, top to bottom:

\`\`\`gleam
"hello world"
|> string.split(" ")
|> list.map(string.uppercase)
|> string.join("-")
// "HELLO-WORLD"
\`\`\`

Each line takes the result of the previous line and passes it as the first argument.

### How It Works

\`a |> f(b, c)\` is equivalent to \`f(a, b, c)\`. The left side becomes the first argument.

\`\`\`gleam
// These are equivalent:
string.length("hello")
"hello" |> string.length

// These are equivalent:
string.replace("hello world", "world", "gleam")
"hello world" |> string.replace("world", "gleam")
\`\`\`

### Pipes with Anonymous Functions

If you need to pass the piped value to a different position, use an anonymous function:

\`\`\`gleam
10
|> fn(x) { int.to_string(x) }
|> io.println
\`\`\`

### Building Data Pipelines

Pipes are especially powerful for data transformation:

\`\`\`gleam
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
|> list.filter(int.is_even)
|> list.map(fn(x) { x * x })
|> list.fold(0, fn(acc, x) { acc + x })
// 220
\`\`\`

### Your Task

Use the pipe operator to transform the string \`"the quick brown fox"\`:
1. Split it by spaces
2. Map each word to uppercase
3. Join with \`" "\`

Print the result.`,

  starterCode: `import gleam/io
import gleam/string
import gleam/list

pub fn main() {
\t// Transform "the quick brown fox" using pipes
}
`,

  solution: `import gleam/io
import gleam/string
import gleam/list

pub fn main() {
\t"the quick brown fox"
\t|> string.split(" ")
\t|> list.map(string.uppercase)
\t|> string.join(" ")
\t|> io.println
}
`,

  tests: [
    {
      name: "pipes string transformation",
      expected: "THE QUICK BROWN FOX\n",
    },
  ],
};
