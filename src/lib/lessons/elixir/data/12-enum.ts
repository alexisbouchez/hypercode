import type { Lesson } from "../../types";

export const enum_: Lesson = {
  id: "enum",
  title: "Enum",
  chapterId: "collections",
  content: `## Enum

The \`Enum\` module provides functions for working with collections:

## map

\`Enum.map\` applies a function to each element:

\`\`\`elixir
doubled = Enum.map([1, 2, 3, 4, 5], fn x -> x * 2 end)
IO.puts(Enum.join(doubled, ", "))   # 2, 4, 6, 8, 10
\`\`\`

## filter

\`Enum.filter\` keeps elements matching a condition:

\`\`\`elixir
evens = Enum.filter([1, 2, 3, 4, 5], fn x -> rem(x, 2) == 0 end)
IO.puts(Enum.join(evens, ", "))   # 2, 4
\`\`\`

## reduce

\`Enum.reduce\` folds a list into a single value:

\`\`\`elixir
total = Enum.reduce([1, 2, 3, 4, 5], 0, fn x, acc -> acc + x end)
IO.puts(total)   # 15
\`\`\`

## Your Turn

Given \`nums = [1, 2, 3, 4, 5]\`:
1. Map to doubles → \`"2, 4, 6, 8, 10"\`
2. Filter evens → \`"2, 4"\`
3. Reduce to sum → \`15\`
`,
  starterCode: `nums = [1, 2, 3, 4, 5]
doubled = Enum.map(nums, fn x -> x * 2 end)
IO.puts(Enum.join(doubled, ", "))

evens = Enum.filter(nums, fn x -> rem(x, 2) == 0 end)
IO.puts(Enum.join(evens, ", "))

total = Enum.reduce(nums, 0, fn x, acc -> acc + x end)
IO.puts(total)
`,
  solution: `nums = [1, 2, 3, 4, 5]
doubled = Enum.map(nums, fn x -> x * 2 end)
IO.puts(Enum.join(doubled, ", "))

evens = Enum.filter(nums, fn x -> rem(x, 2) == 0 end)
IO.puts(Enum.join(evens, ", "))

total = Enum.reduce(nums, 0, fn x, acc -> acc + x end)
IO.puts(total)
`,
  tests: [
    {
      name: "Enum.map, filter, reduce, join",
      expected: "2, 4, 6, 8, 10\n2, 4\n15\n",
    },
  ],
};
