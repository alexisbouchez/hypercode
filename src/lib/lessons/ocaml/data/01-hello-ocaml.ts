import type { Lesson } from "../../types";

export const helloOCaml: Lesson = {
  id: "hello-ocaml",
  title: "Hello, OCaml!",
  chapterId: "basics",
  content: `## Your First OCaml Program

In OCaml, \`print_endline\` prints a string followed by a newline:

\`\`\`ocaml
print_endline "Hello, World!"
\`\`\`

You can also print integers and floats with dedicated functions:

\`\`\`ocaml
print_int 42
print_newline ()
print_float 3.14
print_newline ()
\`\`\`

### Comments

\`\`\`ocaml
(* This is a comment *)

(* Comments can
   span multiple lines *)
\`\`\`

### Your Task

Print exactly \`Hello, World!\`.`,

  starterCode: `print_endline "Hello, World!"
`,

  solution: `print_endline "Hello, World!"
`,

  tests: [
    {
      name: "prints Hello, World!",
      expected: "Hello, World!\n",
    },
  ],
};
