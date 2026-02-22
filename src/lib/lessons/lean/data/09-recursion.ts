import type { Lesson } from "../../types";

export const recursion: Lesson = {
  id: "recursion",
  title: "Recursion",
  chapterId: "functions",
  content: `## Recursive Functions

In Lean, functions can call themselves recursively. The key is having a base case that terminates the recursion:

\`\`\`lean
def factorial (n : Nat) : Nat :=
  if n == 0 then 1 else n * factorial (n - 1)

#eval factorial 5    -- 120
#eval factorial 0    -- 1
\`\`\`

The Fibonacci sequence is another classic recursive function:

\`\`\`lean
def fibonacci (n : Nat) : Nat :=
  if n <= 1 then n
  else fibonacci (n - 1) + fibonacci (n - 2)
\`\`\`

Lean verifies that recursive functions terminate (for \`Nat\`, each recursive call must use a smaller number).

## Your Turn

Define a recursive function \`sumTo\` that computes the sum \`1 + 2 + ... + n\`.

Base case: \`sumTo 0 = 0\`
Recursive case: \`sumTo n = n + sumTo (n - 1)\`
`,
  starterCode: `def sumTo (n : Nat) : Nat :=
  if n == 0 then 0 else n + sumTo (n - 1)

#eval sumTo 5
#eval sumTo 10
#eval sumTo 0
`,
  solution: `def sumTo (n : Nat) : Nat :=
  if n == 0 then 0 else n + sumTo (n - 1)

#eval sumTo 5
#eval sumTo 10
#eval sumTo 0
`,
  tests: [
    {
      name: "sumTo 5 = 15",
      code: "{{FUNC}}\n#eval sumTo 5",
      expected: "15\n",
    },
    {
      name: "sumTo 10 = 55",
      code: "{{FUNC}}\n#eval sumTo 10",
      expected: "55\n",
    },
    {
      name: "sumTo 0 = 0",
      code: "{{FUNC}}\n#eval sumTo 0",
      expected: "0\n",
    },
  ],
};
