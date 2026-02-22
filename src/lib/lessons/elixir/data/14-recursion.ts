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
  ],
};
