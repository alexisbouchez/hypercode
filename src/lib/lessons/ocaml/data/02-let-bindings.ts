import type { Lesson } from "../../types";

export const letBindings: Lesson = {
  id: "let-bindings",
  title: "Let Bindings",
  chapterId: "basics",
  content: `## Let Bindings

In OCaml, you bind values with \`let\`:

\`\`\`ocaml
let x = 42
let name = "Alice"
\`\`\`

By default, bindings are **immutable** — you cannot reassign them.

### Printing Values

Use \`print_endline\` for strings, \`print_int\` for integers:

\`\`\`ocaml
let greeting = "Hello"
let age = 30
print_endline greeting
print_int age
print_newline ()
\`\`\`

### String Concatenation

Use \`^\` to concatenate strings:

\`\`\`ocaml
let full = "Hello" ^ ", " ^ "World!"
print_endline full
\`\`\`

### Your Task

Create a binding \`language\` with value \`"OCaml"\` and print \`"I love OCaml"\` using string concatenation.`,

  starterCode: `let language = "OCaml"
let message = "I love " ^ language
print_endline message
`,

  solution: `let language = "OCaml"
let message = "I love " ^ language
print_endline message
`,

  tests: [
    {
      name: "prints I love OCaml",
      expected: "I love OCaml\n",
    },
  ],
};
