import type { Lesson } from "../../types";

export const variants: Lesson = {
  id: "variants",
  title: "Algebraic Types",
  chapterId: "types",
  content: `## Variant Types (Algebraic Data Types)

OCaml lets you define custom types with \`type\`:

\`\`\`ocaml
type color = Red | Green | Blue
\`\`\`

Each variant is a constructor. You match on them:

\`\`\`ocaml
let color_to_string c =
  match c with
  | Red -> "red"
  | Green -> "green"
  | Blue -> "blue"
\`\`\`

### Variants with Data

Constructors can carry data:

\`\`\`ocaml
type shape =
  | Circle of float
  | Rectangle of float * float
\`\`\`

Since our transpiler erases types, we will work with simple integer-tagged variants for this lesson.

### Your Task

Write a function \`classify\` that takes an integer and returns \`"negative"\`, \`"zero"\`, or \`"positive"\`.`,

  starterCode: `let classify n =
  if n < 0 then "negative"
  else if n = 0 then "zero"
  else "positive"

print_endline (classify (-5))
print_endline (classify 0)
print_endline (classify 42)
`,

  solution: `let classify n =
  if n < 0 then "negative"
  else if n = 0 then "zero"
  else "positive"

print_endline (classify (-5))
print_endline (classify 0)
print_endline (classify 42)
`,

  tests: [
    {
      name: "classify -5 = negative",
      expected: "negative\n",
      code: `{{FUNC}}
print_endline (classify (-5))
`,
    },
    {
      name: "classify 0 = zero",
      expected: "zero\n",
      code: `{{FUNC}}
print_endline (classify 0)
`,
    },
    {
      name: "classify 42 = positive",
      expected: "positive\n",
      code: `{{FUNC}}
print_endline (classify 42)
`,
    },
  ],
};
