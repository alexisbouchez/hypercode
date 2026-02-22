import type { Lesson } from "../../types";

export const mapFilter: Lesson = {
  id: "map-filter",
  title: "Map and Filter",
  chapterId: "functional",
  content: `## Higher-Order Functions: Map and Filter

Higher-order functions take other functions as arguments. Two of the most important are \`map\` and \`filter\`.

**\`map\`** applies a function to every element of a list:

\`\`\`lean
#eval [1, 2, 3, 4, 5].map (fun x => x * 2)
-- [2, 4, 6, 8, 10]
\`\`\`

**\`filter\`** keeps only elements that satisfy a predicate:

\`\`\`lean
#eval [1, 2, 3, 4, 5, 6].filter (fun x => x % 2 == 0)
-- [2, 4, 6]
\`\`\`

You can chain them together:

\`\`\`lean
#eval [1, 2, 3, 4, 5, 6].filter (fun x => x % 2 == 0)
       |>.map (fun x => x * x)
-- [4, 16, 36]
\`\`\`

## Your Turn

1. Use \`map\` to square every element of \`[1, 2, 3, 4, 5]\`
2. Use \`filter\` to keep only numbers greater than 3 from \`[1, 2, 3, 4, 5, 6]\`
`,
  starterCode: `#eval [1, 2, 3, 4, 5].map (fun x => x * x)
#eval [1, 2, 3, 4, 5, 6].filter (fun x => x > 3)
`,
  solution: `#eval [1, 2, 3, 4, 5].map (fun x => x * x)
#eval [1, 2, 3, 4, 5, 6].filter (fun x => x > 3)
`,
  tests: [
    {
      name: "map squares",
      code: "#eval [1, 2, 3, 4, 5].map (fun x => x * x)",
      expected: "[1, 4, 9, 16, 25]\n",
    },
    {
      name: "filter > 3",
      code: "#eval [1, 2, 3, 4, 5, 6].filter (fun x => x > 3)",
      expected: "[4, 5, 6]\n",
    },
  ],
};
