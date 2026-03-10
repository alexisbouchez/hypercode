import type { Lesson } from "../../types";

export const lists: Lesson = {
  id: "lists",
  title: "Lists",
  chapterId: "collections",
  content: `## Lists

Lists are ordered collections in Elixir:

\`\`\`elixir
nums = [1, 2, 3, 4]
IO.puts(length(nums))   # 4
\`\`\`

## hd and tl

\`hd\` returns the head (first element), \`tl\` returns the tail (rest):

\`\`\`elixir
IO.puts(hd([1, 2, 3]))   # 1
IO.puts(length(tl([1, 2, 3])))   # 2
\`\`\`

## Concatenation with ++

\`\`\`elixir
a = [1, 2, 3]
b = [4, 5, 6, 7]
IO.puts(Enum.sum(a ++ b))   # 28
\`\`\`

## Your Turn

Given \`nums = [1, 2, 3, 4, 5]\`:
1. Print \`length(nums)\` → 5
2. Print \`hd(nums)\` → 1
3. Print \`length(tl(nums))\` → 4
4. Print \`Enum.sum(nums ++ [6, 7])\` → 28
`,
  starterCode: `nums = [1, 2, 3, 4, 5]
IO.puts(length(nums))
IO.puts(hd(nums))
IO.puts(length(tl(nums)))
extended = nums ++ [6, 7]
IO.puts(Enum.sum(extended))
`,
  solution: `nums = [1, 2, 3, 4, 5]
IO.puts(length(nums))
IO.puts(hd(nums))
IO.puts(length(tl(nums)))
extended = nums ++ [6, 7]
IO.puts(Enum.sum(extended))
`,
  tests: [
    {
      name: "length, head, tail, and concatenation with sum",
      expected: "5\n1\n4\n28\n",
    },
    {
      name: "single-element list head and tail",
      code: `one = [42]
IO.puts(hd(one))
IO.puts(length(tl(one)))
`,
      expected: "42\n0\n",
    },
    {
      name: "concatenating empty list",
      code: `a = [1, 2, 3]
b = a ++ []
IO.puts(length(b))
IO.puts(Enum.sum(b))
`,
      expected: "3\n6\n",
    },
  ],
};
