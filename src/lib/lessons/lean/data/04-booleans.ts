import type { Lesson } from "../../types";

export const booleans: Lesson = {
  id: "booleans",
  title: "Booleans",
  chapterId: "foundations",
  content: `## Booleans in Lean

Lean's \`Bool\` type has two values: \`true\` and \`false\` (lowercase, unlike Haskell).

Logical operators:
- \`&&\` — and
- \`||\` — or
- \`!\` — not (prefix)

Comparison operators return \`Bool\`:
- \`==\`, \`!=\` — equality
- \`<\`, \`>\`, \`<=\`, \`>=\` — comparisons

\`\`\`lean
#eval true && false    -- false
#eval true || false    -- true
#eval !true            -- false
#eval 5 > 3            -- true
#eval 10 == 10         -- true
\`\`\`

## Your Turn

Evaluate:
1. \`true && true\`
2. \`false || true\`
3. \`!false\`
4. \`7 < 3\`
`,
  starterCode: `#eval true && true
#eval false || true
#eval !false
#eval 7 < 3
`,
  solution: `#eval true && true
#eval false || true
#eval !false
#eval 7 < 3
`,
  tests: [
    {
      name: "boolean expressions",
      expected: "true\ntrue\ntrue\nfalse\n",
    },
  ],
};
