import type { Lesson } from "../../types";

export const multipleParams: Lesson = {
  id: "multiple-params",
  title: "Multiple Parameters",
  chapterId: "functions",
  content: `## Functions with Multiple Parameters

Add more parameters by listing them in parentheses:

\`\`\`lean
def add (a : Nat) (b : Nat) : Nat := a + b

#eval add 3 5    -- 8
\`\`\`

Parameters of the same type can be grouped:

\`\`\`lean
def multiply (x y : Nat) : Nat := x * y

#eval multiply 6 7    -- 42
\`\`\`

When calling a function, separate arguments with spaces:

\`\`\`lean
#eval add 10 20    -- 30
\`\`\`

## Your Turn

Define a function \`power\` that computes \`base ^ exp\` (base raised to the exponent).

Use the \`^\` operator for exponentiation.
`,
  starterCode: `def power (base : Nat) (exp : Nat) : Nat := base ^ exp

#eval power 2 10
#eval power 3 4
#eval power 5 0
`,
  solution: `def power (base : Nat) (exp : Nat) : Nat := base ^ exp

#eval power 2 10
#eval power 3 4
#eval power 5 0
`,
  tests: [
    {
      name: "power 2 10 = 1024",
      code: "{{FUNC}}\n#eval power 2 10",
      expected: "1024\n",
    },
    {
      name: "power 3 4 = 81",
      code: "{{FUNC}}\n#eval power 3 4",
      expected: "81\n",
    },
    {
      name: "power 5 0 = 1",
      code: "{{FUNC}}\n#eval power 5 0",
      expected: "1\n",
    },
  ],
};
