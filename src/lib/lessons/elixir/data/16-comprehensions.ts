import type { Lesson } from "../../types";

export const comprehensions: Lesson = {
  id: "comprehensions",
  title: "Comprehensions & Streams",
  chapterId: "functional",
  content: `## Comprehensions

Elixir's \`for\` comprehension lets you transform and filter collections concisely:

\`\`\`elixir
# Basic comprehension — doubles each element
doubled = for x <- [1, 2, 3, 4], do: x * 2
IO.inspect(doubled)   # [2, 4, 6, 8]
\`\`\`

### Generators and Filters

A comprehension can have multiple **generators** (the \`x <- list\` part) and **filters** (boolean expressions):

\`\`\`elixir
# Filter: only even numbers
evens = for x <- 1..10, rem(x, 2) == 0, do: x
IO.inspect(evens)   # [2, 4, 6, 8, 10]
\`\`\`

Multiple generators produce a cartesian product:

\`\`\`elixir
pairs = for x <- [1, 2], y <- [:a, :b], do: {x, y}
# [{1, :a}, {1, :b}, {2, :a}, {2, :b}]
\`\`\`

## Streams

While \`Enum\` functions are **eager** (they process the entire collection immediately), the \`Stream\` module provides **lazy** evaluation — elements are computed only when needed:

\`\`\`elixir
result = [1, 2, 3, 4, 5]
  |> Stream.map(fn x -> x * 2 end)
  |> Stream.filter(fn x -> x > 4 end)
  |> Enum.to_list()
IO.inspect(result)   # [6, 8, 10]
\`\`\`

Streams are useful when working with large or infinite data — they avoid creating intermediate lists.

\`\`\`elixir
# Stream.take pulls only what's needed
first_five = Stream.iterate(1, fn x -> x + 1 end) |> Enum.take(5)
IO.inspect(first_five)   # [1, 2, 3, 4, 5]
\`\`\`

## Your Turn

1. Use a \`for\` comprehension to square only the odd numbers from \`[1, 2, 3, 4, 5]\`
2. Use \`Stream.map\` and \`Stream.filter\` with \`Enum.to_list\` to triple numbers and keep those > 6
3. Use \`Stream.iterate\` with \`Enum.take\` to get the first 5 powers of 2 starting from 1
`,
  starterCode: `# 1. Comprehension: square odd numbers from [1, 2, 3, 4, 5]
odds_squared = for x <- [1, 2, 3, 4, 5], rem(x, 2) == 1, do: x * x
IO.inspect(odds_squared)

# 2. Stream pipeline: triple, then keep > 6
result = [1, 2, 3, 4, 5]
  |> Stream.map(fn x -> x * 3 end)
  |> Stream.filter(fn x -> x > 6 end)
  |> Enum.to_list()
IO.inspect(result)

# 3. Stream.iterate: first 5 powers of 2
powers = Stream.iterate(1, fn x -> x * 2 end) |> Enum.take(5)
IO.inspect(powers)
`,
  solution: `# 1. Comprehension: square odd numbers from [1, 2, 3, 4, 5]
odds_squared = for x <- [1, 2, 3, 4, 5], rem(x, 2) == 1, do: x * x
IO.inspect(odds_squared)

# 2. Stream pipeline: triple, then keep > 6
result = [1, 2, 3, 4, 5]
  |> Stream.map(fn x -> x * 3 end)
  |> Stream.filter(fn x -> x > 6 end)
  |> Enum.to_list()
IO.inspect(result)

# 3. Stream.iterate: first 5 powers of 2
powers = Stream.iterate(1, fn x -> x * 2 end) |> Enum.take(5)
IO.inspect(powers)
`,
  tests: [
    {
      name: "for comprehension with filter",
      expected: "[1, 9, 25]\n[9, 12, 15]\n[1, 2, 4, 8, 16]\n",
    },
    {
      name: "for comprehension with multiple generators",
      code: `pairs = for x <- [1, 2], y <- [10, 20], do: x + y
IO.inspect(pairs)
`,
      expected: "[11, 21, 12, 22]\n",
    },
    {
      name: "Stream.map and Enum.take from infinite stream",
      code: `squares = Stream.iterate(1, fn x -> x + 1 end)
  |> Stream.map(fn x -> x * x end)
  |> Enum.take(4)
IO.inspect(squares)
`,
      expected: "[1, 4, 9, 16]\n",
    },
  ],
};
