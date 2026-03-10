import type { Lesson } from "../../types";

export const higherOrder: Lesson = {
  id: "higher-order",
  title: "Higher-Order Functions",
  chapterId: "functions",
  content: `## Higher-Order Functions

Functions in OCaml are first-class — they can be passed as arguments and returned from other functions.

### Passing Functions

\`\`\`ocaml
let apply f x = f x
let double x = x * 2

let () = print_int (apply double 5)   (* 10 *)
\`\`\`

### Anonymous Functions

\`\`\`ocaml
let result = apply (fun x -> x + 10) 5   (* 15 *)
\`\`\`

### Composition

\`\`\`ocaml
let compose f g x = f (g x)

let add1 x = x + 1
let double x = x * 2

let add1_then_double = compose double add1
let () = print_int (add1_then_double 3)   (* 8 *)
\`\`\`

### Your Task

Write a function \`apply_twice f x\` that applies \`f\` to \`x\` twice (i.e., \`f (f x)\`). Test it with a doubling function.`,

  starterCode: `let apply_twice f x = f (f x)

let double x = x * 2

print_int (apply_twice double 3)
print_newline ()
print_int (apply_twice double 5)
print_newline ()
`,

  solution: `let apply_twice f x = f (f x)

let double x = x * 2

print_int (apply_twice double 3)
print_newline ()
print_int (apply_twice double 5)
print_newline ()
`,

  tests: [
    {
      name: "apply_twice double 3 = 12",
      expected: "12\n",
      code: `{{FUNC}}
print_int (apply_twice double 3)
print_newline ()
`,
    },
    {
      name: "apply_twice double 5 = 20",
      expected: "20\n",
      code: `{{FUNC}}
print_int (apply_twice double 5)
print_newline ()
`,
    },
    {
      name: "apply_twice succ 0 = 2",
      expected: "2\n",
      code: `{{FUNC}}
print_int (apply_twice succ 0)
print_newline ()
`,
    },
  ],
};
