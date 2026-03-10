import type { Lesson } from "../../types";

export const recursion: Lesson = {
  id: "recursion",
  title: "Recursion",
  chapterId: "functions",
  content: `## Recursion

In OCaml, recursive functions must be declared with \`let rec\`:

\`\`\`ocaml
let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let () = print_int (factorial 5)   (* 120 *)
\`\`\`

### Fibonacci

\`\`\`ocaml
let rec fib n =
  if n <= 1 then n
  else fib (n - 1) + fib (n - 2)

let () = print_int (fib 10)   (* 55 *)
\`\`\`

### Your Task

Write a recursive function \`power base exp\` that computes \`base\` raised to \`exp\`. Assume \`exp >= 0\`.`,

  starterCode: `let rec power base exp =
  if exp = 0 then 1
  else base * power base (exp - 1)

print_int (power 2 10)
print_newline ()
print_int (power 3 4)
print_newline ()
`,

  solution: `let rec power base exp =
  if exp = 0 then 1
  else base * power base (exp - 1)

print_int (power 2 10)
print_newline ()
print_int (power 3 4)
print_newline ()
`,

  tests: [
    {
      name: "2^10 = 1024",
      expected: "1024\n",
      code: `{{FUNC}}
print_int (power 2 10)
print_newline ()
`,
    },
    {
      name: "3^4 = 81",
      expected: "81\n",
      code: `{{FUNC}}
print_int (power 3 4)
print_newline ()
`,
    },
    {
      name: "5^0 = 1",
      expected: "1\n",
      code: `{{FUNC}}
print_int (power 5 0)
print_newline ()
`,
    },
  ],
};
