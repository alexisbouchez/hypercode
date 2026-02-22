import type { Lesson } from "../../types";

export const listRecursion: Lesson = {
  id: "list-recursion",
  title: "Recursion on Lists",
  chapterId: "lists",
  content: `## Recursive Functions on Lists

Lists have two forms — empty (\`[]\`) and non-empty (\`head :: tail\`). You can match on these to write recursive functions:

\`\`\`lean
def mySum : List Nat → Nat
  | [] => 0
  | x :: xs => x + mySum xs

#eval mySum [1, 2, 3, 4, 5]    -- 15
\`\`\`

Reading this:
- If the list is empty (\`[]\`), the sum is \`0\`
- Otherwise, the list is \`x :: xs\` — head \`x\` and tail \`xs\` — so the sum is \`x + mySum xs\`

Similarly for counting elements:

\`\`\`lean
def myLength : List Nat → Nat
  | [] => 0
  | _ :: xs => 1 + myLength xs
\`\`\`

## Your Turn

Define \`myProduct\` that multiplies all elements in a list.
- Empty list → \`1\` (the multiplicative identity)
- Non-empty → first element times the product of the rest
`,
  starterCode: `def myProduct : List Nat → Nat
  | [] => 1
  | x :: xs => x * myProduct xs

#eval myProduct [1, 2, 3, 4, 5]
#eval myProduct [2, 3, 4]
#eval myProduct []
`,
  solution: `def myProduct : List Nat → Nat
  | [] => 1
  | x :: xs => x * myProduct xs

#eval myProduct [1, 2, 3, 4, 5]
#eval myProduct [2, 3, 4]
#eval myProduct []
`,
  tests: [
    {
      name: "myProduct [1,2,3,4,5] = 120",
      code: "{{FUNC}}\n#eval myProduct [1, 2, 3, 4, 5]",
      expected: "120\n",
    },
    {
      name: "myProduct [2,3,4] = 24",
      code: "{{FUNC}}\n#eval myProduct [2, 3, 4]",
      expected: "24\n",
    },
    {
      name: "myProduct [] = 1",
      code: "{{FUNC}}\n#eval myProduct []",
      expected: "1\n",
    },
  ],
};
