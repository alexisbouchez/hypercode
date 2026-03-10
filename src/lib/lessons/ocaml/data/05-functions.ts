import type { Lesson } from "../../types";

export const functions: Lesson = {
  id: "functions",
  title: "Functions",
  chapterId: "functions",
  content: `## Defining Functions

In OCaml, functions are defined with \`let\`:

\`\`\`ocaml
let greet name = "Hello, " ^ name ^ "!"

let () = print_endline (greet "Alice")
\`\`\`

Functions with multiple parameters:

\`\`\`ocaml
let add x y = x + y

let () = print_int (add 3 4)
\`\`\`

### Anonymous Functions

Use \`fun\` to create anonymous functions:

\`\`\`ocaml
let double = fun x -> x * 2
\`\`\`

### If Expressions

\`if\` is an expression in OCaml — it returns a value:

\`\`\`ocaml
let abs_val n = if n >= 0 then n else -n
\`\`\`

### Your Task

Write a function \`square\` that returns the square of its argument, and a function \`cube\` that returns the cube. Print \`square 5\` and \`cube 3\`.`,

  starterCode: `let square x = x * x
let cube x = x * x * x

print_int (square 5)
print_newline ()
print_int (cube 3)
print_newline ()
`,

  solution: `let square x = x * x
let cube x = x * x * x

print_int (square 5)
print_newline ()
print_int (cube 3)
print_newline ()
`,

  tests: [
    {
      name: "square 5 = 25",
      expected: "25\n",
      code: `{{FUNC}}
print_int (square 5)
print_newline ()
`,
    },
    {
      name: "cube 3 = 27",
      expected: "27\n",
      code: `{{FUNC}}
print_int (cube 3)
print_newline ()
`,
    },
    {
      name: "square 0 = 0",
      expected: "0\n",
      code: `{{FUNC}}
print_int (square 0)
print_newline ()
`,
    },
  ],
};
