import type { Lesson } from "../../types";

export const functions: Lesson = {
  id: "functions",
  title: "Functions",
  chapterId: "foundations",
  content: `## Defining Functions

Functions in Gleam are declared with the \`fn\` keyword:

\`\`\`gleam
fn add(a: Int, b: Int) -> Int {
  a + b
}
\`\`\`

The last expression in a function body is automatically returned. There is no \`return\` keyword.

### Public Functions

By default, functions are private to their module. Use \`pub\` to make them accessible from other modules:

\`\`\`gleam
pub fn greet(name: String) -> String {
  "Hello, " <> name <> "!"
}
\`\`\`

### Labeled Arguments

Gleam supports labeled arguments, which make function calls more readable:

\`\`\`gleam
pub fn replace(
  in string: String,
  each pattern: String,
  with replacement: String,
) -> String {
  // ...
}

// Called with labels:
replace(in: "hello world", each: "world", with: "gleam")
\`\`\`

The label comes before the parameter name. At the call site, you use the label.

### Anonymous Functions

You can create functions without names:

\`\`\`gleam
let double = fn(x: Int) -> Int { x * 2 }
double(5)  // 10
\`\`\`

Anonymous functions are commonly used with higher-order functions like \`list.map\`.

### Multiple Expressions

A function body can contain multiple expressions. Each expression except the last is evaluated for its side effects:

\`\`\`gleam
pub fn main() {
  io.println("first")
  io.println("second")
  io.println("third")
}
\`\`\`

### Your Task

Write a function called \`greet\` that takes a name and returns a greeting in the format \`Hello, <name>!\`. Use it in \`main\` to greet "World" and "Gleam" on separate lines.`,

  starterCode: `import gleam/io

fn greet(name: String) -> String {
\t// Return a greeting string
\t""
}

pub fn main() {
\t// Use the greet function to print two greetings
}
`,

  solution: `import gleam/io

fn greet(name: String) -> String {
\t"Hello, " <> name <> "!"
}

pub fn main() {
\tio.println(greet("World"))
\tio.println(greet("Gleam"))
}
`,

  tests: [
    {
      name: "prints two greetings",
      expected: "Hello, World!\nHello, Gleam!\n",
    },
  ],
};
