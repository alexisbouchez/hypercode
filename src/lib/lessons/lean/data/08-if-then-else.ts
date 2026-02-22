import type { Lesson } from "../../types";

export const ifThenElse: Lesson = {
  id: "if-then-else",
  title: "If-Then-Else",
  chapterId: "functions",
  content: `## Conditional Expressions

In Lean, \`if\`/\`then\`/\`else\` is an expression (not a statement), so it always returns a value:

\`\`\`lean
def max (a b : Nat) : Nat :=
  if a >= b then a else b

#eval max 10 20    -- 20
#eval max 30 5     -- 30
\`\`\`

Both branches must have the same type.

You can nest if-then-else:

\`\`\`lean
def classify (n : Nat) : String :=
  if n == 0 then "zero"
  else if n < 10 then "small"
  else "large"
\`\`\`

## Your Turn

Define a function \`isEven\` that returns \`true\` if a number is even, \`false\` otherwise.

Hint: A number is even if \`n % 2 == 0\`.
`,
  starterCode: `def isEven (n : Nat) : Bool :=
  if n % 2 == 0 then true else false

#eval isEven 4
#eval isEven 7
#eval isEven 0
`,
  solution: `def isEven (n : Nat) : Bool :=
  if n % 2 == 0 then true else false

#eval isEven 4
#eval isEven 7
#eval isEven 0
`,
  tests: [
    {
      name: "isEven 4 = true",
      code: "{{FUNC}}\n#eval isEven 4",
      expected: "true\n",
    },
    {
      name: "isEven 7 = false",
      code: "{{FUNC}}\n#eval isEven 7",
      expected: "false\n",
    },
    {
      name: "isEven 0 = true",
      code: "{{FUNC}}\n#eval isEven 0",
      expected: "true\n",
    },
  ],
};
