import type { Lesson } from "../../types";

export const optionType: Lesson = {
  id: "option-type",
  title: "Option Type",
  chapterId: "types",
  content: `## The Option Type

OCaml uses \`option\` to represent values that might be absent — no null pointers!

\`\`\`ocaml
type 'a option = None | Some of 'a
\`\`\`

A value is either \`Some x\` (present) or \`None\` (absent).

### Pattern Matching on Options

\`\`\`ocaml
let describe opt =
  match opt with
  | None -> "nothing"
  | Some x -> "got: " ^ string_of_int x
\`\`\`

### Safe Operations

Options are perfect for operations that might fail. You can use \`match\` to safely extract values:

\`\`\`ocaml
let get_or_default opt d =
  match opt with
  | None -> d
  | Some x -> x
\`\`\`

### Your Task

Write a function \`find_positive\` that takes a list and returns the first positive number as a string, or \`"none"\` if no positive number exists. Use a helper \`show_option\` to convert the result.`,

  starterCode: `let rec find_first_positive lst =
  match lst with
  | [] -> "none"
  | x :: rest -> if x > 0 then string_of_int x else find_first_positive rest

print_endline (find_first_positive [10; 20; 30])
print_endline (find_first_positive [])
print_endline (find_first_positive [-1; -2; 5])
`,

  solution: `let rec find_first_positive lst =
  match lst with
  | [] -> "none"
  | x :: rest -> if x > 0 then string_of_int x else find_first_positive rest

print_endline (find_first_positive [10; 20; 30])
print_endline (find_first_positive [])
print_endline (find_first_positive [-1; -2; 5])
`,

  tests: [
    {
      name: "find in non-empty list",
      expected: "10\n",
      code: `{{FUNC}}
print_endline (find_first_positive [10; 20; 30])
`,
    },
    {
      name: "find in empty list",
      expected: "none\n",
      code: `{{FUNC}}
print_endline (find_first_positive [])
`,
    },
    {
      name: "find skips negatives",
      expected: "5\n",
      code: `{{FUNC}}
print_endline (find_first_positive [-1; -2; 5])
`,
    },
  ],
};
