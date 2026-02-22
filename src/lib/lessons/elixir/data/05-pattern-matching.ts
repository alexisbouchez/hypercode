import type { Lesson } from "../../types";

export const patternMatching: Lesson = {
  id: "pattern-matching",
  title: "Pattern Matching",
  chapterId: "control-flow",
  content: `## The Match Operator

In Elixir, \`=\` is the **match operator**, not just assignment. It binds variables on the left to values on the right.

\`\`\`elixir
x = 10
IO.puts(x)   # 10
\`\`\`

## Tuple Destructuring

You can match tuples to extract values:

\`\`\`elixir
{a, b} = {10, 20}
IO.puts(a)   # 10
IO.puts(b)   # 20
\`\`\`

## List Destructuring

Match list head and tail with \`[head | tail]\`:

\`\`\`elixir
[first | rest] = [1, 2, 3, 4]
IO.puts(first)          # 1
IO.puts(length(rest))   # 3
\`\`\`

## Your Turn

1. Match \`{a, b} = {10, 20}\` and print both values
2. Match \`[first | rest] = [1, 2, 3, 4]\` and print \`first\` and \`length(rest)\`
`,
  starterCode: `{a, b} = {10, 20}
IO.puts(a)
IO.puts(b)
[first | rest] = [1, 2, 3, 4]
IO.puts(first)
IO.puts(length(rest))
`,
  solution: `{a, b} = {10, 20}
IO.puts(a)
IO.puts(b)
[first | rest] = [1, 2, 3, 4]
IO.puts(first)
IO.puts(length(rest))
`,
  tests: [
    {
      name: "tuple and list destructuring",
      expected: "10\n20\n1\n3\n",
    },
  ],
};
