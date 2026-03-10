import type { Lesson } from "../../types";

export const patternMatching: Lesson = {
  id: "pattern-matching",
  title: "Pattern Matching",
  chapterId: "data",
  content: `## Pattern Matching

Pattern matching is one of OCaml's most powerful features. Use \`match ... with\`:

\`\`\`ocaml
let describe n =
  match n with
  | 0 -> "zero"
  | 1 -> "one"
  | _ -> "other"
\`\`\`

### Matching Lists

\`\`\`ocaml
let rec length lst =
  match lst with
  | [] -> 0
  | _ :: rest -> 1 + length rest
\`\`\`

### The Wildcard \`_\`

The underscore matches anything and serves as a catch-all:

\`\`\`ocaml
let is_zero n =
  match n with
  | 0 -> true
  | _ -> false
\`\`\`

### Your Task

Write a function \`describe_list\` that returns \`"empty"\` for an empty list, \`"singleton"\` for a one-element list, and \`"long"\` for anything else.`,

  starterCode: `let describe_list lst =
  match lst with
  | [] -> "empty"
  | _ :: [] -> "singleton"
  | _ -> "long"

print_endline (describe_list [])
print_endline (describe_list [1])
print_endline (describe_list [1; 2; 3])
`,

  solution: `let describe_list lst =
  match lst with
  | [] -> "empty"
  | _ :: [] -> "singleton"
  | _ -> "long"

print_endline (describe_list [])
print_endline (describe_list [1])
print_endline (describe_list [1; 2; 3])
`,

  tests: [
    {
      name: "empty list",
      expected: "empty\n",
      code: `{{FUNC}}
print_endline (describe_list [])
`,
    },
    {
      name: "singleton list",
      expected: "singleton\n",
      code: `{{FUNC}}
print_endline (describe_list [42])
`,
    },
    {
      name: "long list",
      expected: "long\n",
      code: `{{FUNC}}
print_endline (describe_list [1; 2; 3])
`,
    },
  ],
};
