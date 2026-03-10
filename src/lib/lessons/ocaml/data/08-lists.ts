import type { Lesson } from "../../types";

export const lists: Lesson = {
  id: "lists",
  title: "Lists",
  chapterId: "data",
  content: `## Lists

OCaml lists are immutable, singly-linked, and homogeneous. Create them with square brackets and semicolons:

\`\`\`ocaml
let nums = [1; 2; 3; 4; 5]
\`\`\`

### Basic Operations

\`\`\`ocaml
let first = List.hd nums         (* 1 *)
let rest = List.tl nums          (* [2; 3; 4; 5] *)
let len = List.length nums       (* 5 *)
let third = List.nth nums 2      (* 3 *)
\`\`\`

### Cons Operator (\`::\`)

Prepend an element to a list:

\`\`\`ocaml
let more = 0 :: nums   (* [0; 1; 2; 3; 4; 5] *)
\`\`\`

### Append (\`@\`)

\`\`\`ocaml
let combined = [1; 2] @ [3; 4]   (* [1; 2; 3; 4] *)
\`\`\`

### Your Task

Write a recursive function \`sum_list\` that computes the sum of a list of integers.`,

  starterCode: `let rec sum_list lst =
  if List.length lst = 0 then 0
  else List.hd lst + sum_list (List.tl lst)

print_int (sum_list [1; 2; 3; 4; 5])
print_newline ()
print_int (sum_list [10; 20; 30])
print_newline ()
`,

  solution: `let rec sum_list lst =
  if List.length lst = 0 then 0
  else List.hd lst + sum_list (List.tl lst)

print_int (sum_list [1; 2; 3; 4; 5])
print_newline ()
print_int (sum_list [10; 20; 30])
print_newline ()
`,

  tests: [
    {
      name: "sum [1;2;3;4;5] = 15",
      expected: "15\n",
      code: `{{FUNC}}
print_int (sum_list [1; 2; 3; 4; 5])
print_newline ()
`,
    },
    {
      name: "sum [10;20;30] = 60",
      expected: "60\n",
      code: `{{FUNC}}
print_int (sum_list [10; 20; 30])
print_newline ()
`,
    },
    {
      name: "sum [] = 0",
      expected: "0\n",
      code: `{{FUNC}}
print_int (sum_list [])
print_newline ()
`,
    },
  ],
};
