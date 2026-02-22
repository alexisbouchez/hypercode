import type { Lesson } from "../../types";

export const numbers: Lesson = {
  id: "numbers",
  title: "Numbers",
  chapterId: "foundations",
  content: `## Numbers in Lean

Lean has several numeric types. The most common is \`Nat\` — natural numbers (0, 1, 2, ...). Lean also has \`Int\` for integers, \`Float\` for decimals.

\`#eval\` displays the value of a numeric expression:

\`\`\`lean
#eval 2 + 3      -- 5
#eval 10 * 4     -- 40
#eval 20 - 7     -- 13
#eval 15 / 3     -- 5 (integer division)
#eval 7 % 3      -- 1 (remainder)
\`\`\`

Nat division always rounds down: \`7 / 2 = 3\`, not \`3.5\`.

## Your Turn

Evaluate the following expressions:
1. \`100 - 37\`
2. \`6 * 7\`
3. \`17 % 5\`
`,
  starterCode: `-- Evaluate each expression with #eval
#eval 100 - 37
#eval 6 * 7
#eval 17 % 5
`,
  solution: `#eval 100 - 37
#eval 6 * 7
#eval 17 % 5
`,
  tests: [
    {
      name: "100 - 37 = 63",
      expected: "63\n42\n2\n",
    },
  ],
};
