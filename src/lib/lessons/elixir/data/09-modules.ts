import type { Lesson } from "../../types";

export const modules: Lesson = {
  id: "modules",
  title: "Modules & Functions",
  chapterId: "functions",
  content: `## defmodule

Elixir code is organized into modules. Use \`defmodule\` to define one:

\`\`\`elixir
defmodule Math do
  def square(x) do
    x * x
  end
end

IO.puts(Math.square(4))   # 16
\`\`\`

## Named Functions

\`def\` defines a public function inside a module. You call it with \`Module.function()\`.

Short functions can use the inline \`do:\` syntax:

\`\`\`elixir
defmodule Math do
  def cube(x), do: x * x * x
end

IO.puts(Math.cube(3))   # 27
\`\`\`

## Your Turn

Define a \`Math\` module with:
1. \`square(x)\` — returns x squared
2. \`cube(x)\` — returns x cubed

Print \`Math.square(4)\` → 16 and \`Math.cube(3)\` → 27
`,
  starterCode: `defmodule Math do
  def square(x) do
    x * x
  end

  def cube(x), do: x * x * x
end

IO.puts(Math.square(4))
IO.puts(Math.cube(3))
`,
  solution: `defmodule Math do
  def square(x) do
    x * x
  end

  def cube(x), do: x * x * x
end

IO.puts(Math.square(4))
IO.puts(Math.cube(3))
`,
  tests: [
    {
      name: "defmodule with square and cube",
      expected: "16\n27\n",
    },
  ],
};
