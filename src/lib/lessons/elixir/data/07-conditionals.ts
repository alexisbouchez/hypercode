import type { Lesson } from "../../types";

export const conditionals: Lesson = {
  id: "conditionals",
  title: "Conditionals",
  chapterId: "control-flow",
  content: `## if/else

Elixir's \`if\` is an expression that returns a value:

\`\`\`elixir
x = 15
label = if x > 10 do
  "big"
else
  "small"
end
IO.puts(label)   # big
\`\`\`

## cond

\`cond\` checks multiple conditions in order, like a chain of if/else if:

\`\`\`elixir
score = 82
grade = cond do
  score >= 90 -> "A"
  score >= 80 -> "B"
  score >= 70 -> "C"
  true -> "F"
end
IO.puts(grade)   # B
\`\`\`

The final \`true ->\` is the catch-all clause.

## Your Turn

1. If \`x = 15\`, use \`if\` to print \`"big"\` (x > 10) or \`"small"\`
2. If \`score = 82\`, use \`cond\` to print the letter grade (\`"B"\`)
`,
  starterCode: `x = 15
label = if x > 10 do
  "big"
else
  "small"
end
IO.puts(label)

score = 82
grade = cond do
  score >= 90 -> "A"
  score >= 80 -> "B"
  score >= 70 -> "C"
  true -> "F"
end
IO.puts(grade)
`,
  solution: `x = 15
label = if x > 10 do
  "big"
else
  "small"
end
IO.puts(label)

score = 82
grade = cond do
  score >= 90 -> "A"
  score >= 80 -> "B"
  score >= 70 -> "C"
  true -> "F"
end
IO.puts(grade)
`,
  tests: [
    {
      name: "if/else and cond",
      expected: "big\nB\n",
    },
  ],
};
