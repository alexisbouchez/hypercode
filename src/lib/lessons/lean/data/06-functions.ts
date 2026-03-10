import type { Lesson } from "../../types";

export const functions: Lesson = {
  id: "functions",
  title: "Defining Functions",
  chapterId: "functions",
  content: `## Functions in Lean

Functions are defined with \`def\`, just like constants — but with parameters:

\`\`\`lean
def double (n : Nat) : Nat := n * 2
\`\`\`

- \`double\` is the function name
- \`(n : Nat)\` is a parameter with its type
- \`: Nat\` is the return type
- \`n * 2\` is the body

Call the function by writing its name followed by the argument (no parentheses needed):

\`\`\`lean
#eval double 5    -- 10
#eval double 21   -- 42
\`\`\`

## Dependent Types Preview

In Lean, function types can be **dependent** — the return type can refer to the input value. For example, a function could return a different type based on which argument it receives. The general function type \`(x : A) → B x\` says "given an \`x\` of type \`A\`, return something of type \`B x\`" where \`B\` may depend on \`x\`. When \`B\` does not depend on \`x\`, this simplifies to the ordinary function type \`A → B\`. This is what distinguishes Lean from languages like Haskell or OCaml.

## Your Turn

Define a function \`triple\` that multiplies its argument by 3.
`,
  starterCode: `def triple (n : Nat) : Nat := n * 3

#eval triple 5
#eval triple 10
#eval triple 0
`,
  solution: `def triple (n : Nat) : Nat := n * 3

#eval triple 5
#eval triple 10
#eval triple 0
`,
  tests: [
    {
      name: "triple 5 = 15",
      code: "{{FUNC}}\n#eval triple 5",
      expected: "15\n",
    },
    {
      name: "triple 10 = 30",
      code: "{{FUNC}}\n#eval triple 10",
      expected: "30\n",
    },
    {
      name: "triple 0 = 0",
      code: "{{FUNC}}\n#eval triple 0",
      expected: "0\n",
    },
  ],
};
