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
    {
      name: "handle ok with string value",
      code: `{{FUNC}}
IO.puts(Handler.handle({:ok, "hello"}))
IO.puts(Handler.handle({:ok, 0}))
`,
      expected: "Success: hello\nSuccess: 0\n",
    },
    {
      name: "handle error with different reasons",
      code: `{{FUNC}}
IO.puts(Handler.handle({:error, "timeout"}))
IO.puts(Handler.handle({:error, 404}))
IO.puts(Handler.handle(:unknown))
`,
      expected: "Error: timeout\nError: 404\nUnknown\n",
    },
  ],
};
