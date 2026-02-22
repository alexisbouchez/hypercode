import type { Lesson } from "../../types";

export const multihead: Lesson = {
  id: "multihead",
  title: "Multi-Head Functions",
  chapterId: "functional",
  content: `## Pattern Matching in Function Heads

Elixir allows multiple clauses for the same function, each matching different patterns:

\`\`\`elixir
defmodule Handler do
  def handle({:ok, value}), do: "Success: \#{value}"
  def handle({:error, reason}), do: "Error: \#{reason}"
  def handle(_), do: "Unknown"
end
\`\`\`

Elixir tries each clause top to bottom until one matches.

## Using It

\`\`\`elixir
IO.puts(Handler.handle({:ok, 42}))          # Success: 42
IO.puts(Handler.handle({:error, "not found"}))  # Error: not found
IO.puts(Handler.handle(:something_else))    # Unknown
\`\`\`

This pattern is fundamental to Elixir — it replaces if/else chains with declarative matching.

## Your Turn

Implement the \`Handler\` module above and print all three results.
`,
  starterCode: `defmodule Handler do
  def handle({:ok, value}), do: "Success: \#{value}"
  def handle({:error, reason}), do: "Error: \#{reason}"
  def handle(_), do: "Unknown"
end

IO.puts(Handler.handle({:ok, 42}))
IO.puts(Handler.handle({:error, "not found"}))
IO.puts(Handler.handle(:something_else))
`,
  solution: `defmodule Handler do
  def handle({:ok, value}), do: "Success: \#{value}"
  def handle({:error, reason}), do: "Error: \#{reason}"
  def handle(_), do: "Unknown"
end

IO.puts(Handler.handle({:ok, 42}))
IO.puts(Handler.handle({:error, "not found"}))
IO.puts(Handler.handle(:something_else))
`,
  tests: [
    {
      name: "multi-head function pattern matching",
      expected: "Success: 42\nError: not found\nUnknown\n",
    },
  ],
};
