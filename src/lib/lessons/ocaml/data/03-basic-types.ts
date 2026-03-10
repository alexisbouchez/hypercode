import type { Lesson } from "../../types";

export const basicTypes: Lesson = {
  id: "basic-types",
  title: "Basic Types",
  chapterId: "basics",
  content: `## Basic Types

OCaml has several built-in types:

| Type | Example |
|------|---------|
| \`int\` | \`42\`, \`-7\`, \`0\` |
| \`float\` | \`3.14\`, \`1.0\` |
| \`string\` | \`"hello"\` |
| \`bool\` | \`true\`, \`false\` |
| \`char\` | \`'a'\`, \`'Z'\` |
| \`unit\` | \`()\` |

### Type Conversions

OCaml does **not** implicitly convert types. You must be explicit:

\`\`\`ocaml
let n = 42
let s = string_of_int n      (* "42" *)
let f = float_of_int n       (* 42.0 *)
let m = int_of_string "100"  (* 100 *)
\`\`\`

### Boolean Operations

\`\`\`ocaml
let a = true && false   (* false *)
let b = true || false   (* true *)
let c = not true        (* false *)
\`\`\`

### Your Task

Convert the integer \`255\` to a string and print it concatenated with \`" is max byte"\`.`,

  starterCode: `let n = 255
let msg = string_of_int n ^ " is max byte"
print_endline msg
`,

  solution: `let n = 255
let msg = string_of_int n ^ " is max byte"
print_endline msg
`,

  tests: [
    {
      name: "prints 255 is max byte",
      expected: "255 is max byte\n",
    },
  ],
};
