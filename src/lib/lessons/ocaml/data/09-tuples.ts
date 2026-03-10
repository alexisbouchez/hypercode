import type { Lesson } from "../../types";

export const tuples: Lesson = {
  id: "tuples",
  title: "Tuples",
  chapterId: "data",
  content: `## Tuples

Tuples group a fixed number of values of possibly different types:

\`\`\`ocaml
let pair = (1, "hello")
let triple = (1, 2.0, "three")
\`\`\`

### Accessing Elements

For pairs, use \`fst\` and \`snd\`:

\`\`\`ocaml
let p = (10, 20)
let a = fst p   (* 10 *)
let b = snd p   (* 20 *)
\`\`\`

### Destructuring

You can pattern-match tuples directly:

\`\`\`ocaml
let (x, y) = (3, 4)
(* x = 3, y = 4 *)
\`\`\`

### Your Task

Write a function \`swap\` that takes a pair \`(a, b)\` and returns \`(b, a)\`. Print the swapped pair's elements.`,

  starterCode: `let swap p = (snd p, fst p)

let result = swap (1, 2)
print_int (fst result)
print_newline ()
print_int (snd result)
print_newline ()
`,

  solution: `let swap p = (snd p, fst p)

let result = swap (1, 2)
print_int (fst result)
print_newline ()
print_int (snd result)
print_newline ()
`,

  tests: [
    {
      name: "swap (1, 2) -> fst = 2",
      expected: "2\n",
      code: `{{FUNC}}
let result = swap (1, 2)
print_int (fst result)
print_newline ()
`,
    },
    {
      name: "swap (1, 2) -> snd = 1",
      expected: "1\n",
      code: `{{FUNC}}
let result = swap (1, 2)
print_int (snd result)
print_newline ()
`,
    },
    {
      name: "swap (10, 20) -> fst = 20",
      expected: "20\n",
      code: `{{FUNC}}
let result = swap (10, 20)
print_int (fst result)
print_newline ()
`,
    },
  ],
};
