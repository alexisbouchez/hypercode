import type { Lesson } from "../../types";

export const arithmetic: Lesson = {
  id: "arithmetic",
  title: "Arithmetic",
  chapterId: "basics",
  content: `## Arithmetic Operations

OCaml uses separate operators for integer and float arithmetic:

### Integer Arithmetic

\`\`\`ocaml
let a = 10 + 3    (* 13 *)
let b = 10 - 3    (* 7 *)
let c = 10 * 3    (* 30 *)
let d = 10 / 3    (* 3 — integer division *)
let e = 10 mod 3  (* 1 — remainder *)
\`\`\`

### Useful Functions

\`\`\`ocaml
let x = abs (-5)    (* 5 *)
let y = max 10 20   (* 20 *)
let z = min 10 20   (* 10 *)
let s = succ 5      (* 6 — successor *)
let p = pred 5      (* 4 — predecessor *)
\`\`\`

### Your Task

Compute the result of \`(7 + 3) * (10 - 4)\` and print it.`,

  starterCode: `let result = (7 + 3) * (10 - 4)
print_int result
print_newline ()
`,

  solution: `let result = (7 + 3) * (10 - 4)
print_int result
print_newline ()
`,

  tests: [
    {
      name: "prints 60",
      expected: "60\n",
    },
  ],
};
