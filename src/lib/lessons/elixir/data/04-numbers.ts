import type { Lesson } from "../../types";

export const numbers: Lesson = {
  id: "numbers",
  title: "Numbers",
  chapterId: "foundations",
  content: `## Arithmetic

Elixir supports standard arithmetic operators:

\`\`\`elixir
IO.puts(10 + 3)   # 13
IO.puts(10 - 3)   # 7
IO.puts(10 * 3)   # 30
\`\`\`

## Integer Division and Remainder

Use \`div/2\` for integer division and \`rem/2\` for remainder:

\`\`\`elixir
IO.puts(div(10, 3))   # 3
IO.puts(rem(10, 3))   # 1
\`\`\`

## Floats

Use \`Float.round/2\` to round to a given number of decimal places:

\`\`\`elixir
IO.puts(Float.round(3.14159, 2))   # 3.14
\`\`\`

## Your Turn

Print:
1. \`10 + 3\` = 13
2. \`10 - 3\` = 7
3. \`10 * 3\` = 30
4. \`div(10, 3)\` = 3
5. \`rem(10, 3)\` = 1
6. \`Float.round(3.14159, 2)\` = 3.14
`,
  starterCode: `IO.puts(10 + 3)
IO.puts(10 - 3)
IO.puts(10 * 3)
IO.puts(div(10, 3))
IO.puts(rem(10, 3))
IO.puts(Float.round(3.14159, 2))
`,
  solution: `IO.puts(10 + 3)
IO.puts(10 - 3)
IO.puts(10 * 3)
IO.puts(div(10, 3))
IO.puts(rem(10, 3))
IO.puts(Float.round(3.14159, 2))
`,
  tests: [
    {
      name: "arithmetic, div, rem, float round",
      expected: "13\n7\n30\n3\n1\n3.14\n",
    },
  ],
};
