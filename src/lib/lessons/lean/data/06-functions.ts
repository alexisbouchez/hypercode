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
