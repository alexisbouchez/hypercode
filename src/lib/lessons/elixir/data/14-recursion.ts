import type { Lesson } from "../../types";

export const recursion: Lesson = {
  id: "recursion",
  title: "Recursion",
  chapterId: "functional",
  content: `## Recursion

Elixir uses recursion instead of traditional loops. A recursive function calls itself with a smaller problem.

\`\`\`elixir
defmodule Math do
  def factorial(0), do: 1
  def factorial(n), do: n * factorial(n - 1)
end

IO.puts(Math.factorial(5))   # 120
\`\`\`

The first clause matches \`0\` (base case), the second matches any other value.

## Sum of a List

\`\`\`elixir
defmodule MyList do
  def sum([]), do: 0
  def sum([head | tail]), do: head + sum(tail)
end

IO.puts(MyList.sum([1, 2, 3, 4, 5]))   # 15
\`\`\`

## Your Turn

Implement:
1. \`Math.factorial(5)\` → 120
2. \`MyList.sum([1, 2, 3, 4, 5])\` → 15
`,
  starterCode: `defmodule Math do
  def factorial(0), do: 1
  def factorial(n), do: n * factorial(n - 1)
end

defmodule MyList do
  def sum([]), do: 0
  def sum([head | tail]), do: head + sum(tail)
end

IO.puts(Math.factorial(5))
IO.puts(MyList.sum([1, 2, 3, 4, 5]))
`,
  solution: `defmodule Math do
  def factorial(0), do: 1
  def factorial(n), do: n * factorial(n - 1)
end

defmodule MyList do
  def sum([]), do: 0
  def sum([head | tail]), do: head + sum(tail)
end

IO.puts(Math.factorial(5))
IO.puts(MyList.sum([1, 2, 3, 4, 5]))
`,
  tests: [
    {
      name: "factorial and recursive sum",
      expected: "120\n15\n",
    },
    {
      name: "factorial of 0 and 1 (base cases)",
      code: `{{FUNC}}
IO.puts(Math.factorial(0))
IO.puts(Math.factorial(1))
IO.puts(Math.factorial(3))
`,
      expected: "1\n1\n6\n",
    },
    {
      name: "sum of empty and single-element list",
      code: `{{FUNC}}
IO.puts(MyList.sum([]))
IO.puts(MyList.sum([100]))
IO.puts(MyList.sum([10, 20, 30]))
`,
      expected: "0\n100\n60\n",
    },
  ],
};
