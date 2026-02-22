import type { Lesson } from "../../types";

export const lists: Lesson = {
  id: "lists",
  title: "Lists",
  chapterId: "lists",
  content: `## Lists in Lean

Lists are written with square brackets and comma-separated elements:

\`\`\`lean
def nums := [1, 2, 3, 4, 5]
\`\`\`

Useful operations:

\`\`\`lean
#eval [1, 2, 3].length       -- 3
#eval [1, 2] ++ [3, 4, 5]    -- [1, 2, 3, 4, 5]
#eval [1, 2, 3].reverse      -- [3, 2, 1]
\`\`\`

You can also prepend an element using \`::\` (the cons operator):

\`\`\`lean
#eval 0 :: [1, 2, 3]    -- [0, 1, 2, 3]
\`\`\`

## Your Turn

Evaluate:
1. The length of \`[10, 20, 30, 40]\`
2. The concatenation of \`[1, 2, 3]\` and \`[4, 5, 6]\`
3. The reverse of \`[1, 2, 3, 4, 5]\`
`,
  starterCode: `#eval [10, 20, 30, 40].length
#eval [1, 2, 3] ++ [4, 5, 6]
#eval [1, 2, 3, 4, 5].reverse
`,
  solution: `#eval [10, 20, 30, 40].length
#eval [1, 2, 3] ++ [4, 5, 6]
#eval [1, 2, 3, 4, 5].reverse
`,
  tests: [
    {
      name: "list operations",
      expected: "4\n[1, 2, 3, 4, 5, 6]\n[5, 4, 3, 2, 1]\n",
    },
  ],
};
