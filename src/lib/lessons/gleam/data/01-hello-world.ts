import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "foundations",
  content: `## Your First Gleam Program

Every Gleam program starts with imports. To print text, you need the \`io\` module from the standard library:

\`\`\`gleam
import gleam/io
\`\`\`

### The Entry Point

The entry point is a public function called \`main\`:

\`\`\`gleam
pub fn main() {
  io.println("Hello, World!")
}
\`\`\`

The \`pub\` keyword makes the function public. The \`fn\` keyword declares a function. Gleam functions do not need explicit return types when the compiler can infer them.

### Printing Output

Gleam provides two main print functions in \`gleam/io\`:

| Function | Description |
|----------|-------------|
| \`io.println\` | Prints a string followed by a newline |
| \`io.print\` | Prints a string without a newline |
| \`io.debug\` | Prints any value in debug format and returns it |

Unlike many languages, \`io.println\` only accepts strings. To print numbers or other types, you must convert them to strings first (we will learn how in later lessons).

### Gleam's Design

Gleam is designed to be simple and predictable:

- **No exceptions** -- errors are values, handled with the \`Result\` type.
- **No null** -- optional values use the \`Option\` type.
- **Immutable data** -- once a value is created, it cannot be changed.
- **Expression-based** -- everything is an expression that returns a value.

### Your Task

Write a program that prints exactly \`Hello, World!\` followed by a newline.`,

  starterCode: `import gleam/io

pub fn main() {
\t// Print "Hello, World!" here
}
`,

  solution: `import gleam/io

pub fn main() {
\tio.println("Hello, World!")
}
`,

  tests: [
    {
      name: "prints Hello, World!",
      expected: "Hello, World!\n",
    },
  ],
};
