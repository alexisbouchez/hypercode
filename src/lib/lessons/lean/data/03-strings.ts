import type { Lesson } from "../../types";

export const strings: Lesson = {
  id: "strings",
  title: "Strings",
  chapterId: "foundations",
  content: `## Strings in Lean

Strings in Lean are written with double quotes. You can concatenate them with \`++\`:

\`\`\`lean
#eval "Hello" ++ ", World!"   -- "Hello, World!"
\`\`\`

Note: \`#eval\` on a \`String\` shows it with quotes (that is Lean's display format for strings). Use \`IO.println\` to print without quotes.

You can get the length of a string using \`.length\`:

\`\`\`lean
#eval "Lean".length    -- 4
\`\`\`

## Your Turn

1. Concatenate \`"Lean "\` and \`"is fun"\` and evaluate it
2. Evaluate the length of \`"functional"\`
`,
  starterCode: `-- Concatenate two strings
#eval "Lean " ++ "is fun"
-- Get the length
#eval "functional".length
`,
  solution: `#eval "Lean " ++ "is fun"
#eval "functional".length
`,
  tests: [
    {
      name: 'concat and length',
      expected: '"Lean is fun"\n10\n',
    },
  ],
};
