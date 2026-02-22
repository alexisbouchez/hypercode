import type { Lesson } from "../../types";

export const variables: Lesson = {
  id: "variables",
  title: "Variables & Atoms",
  chapterId: "foundations",
  content: `## Variables

In Elixir, you bind values to names using \`=\`. Variables start with a lowercase letter.

\`\`\`elixir
name = "Alice"
age = 30
active = true
IO.puts(name)
IO.puts(age)
IO.puts(active)
\`\`\`

## Atoms

Atoms are constants whose value is their own name. They start with a colon.

\`\`\`elixir
status = :ok
IO.puts(status)
\`\`\`

Atoms like \`:ok\`, \`:error\`, and \`:not_found\` are used extensively in Elixir for tagging results.

## Your Turn

Create variables:
- \`name\` = \`"Alice"\`
- \`age\` = \`30\`
- \`active\` = \`true\`
- \`status\` = \`:ok\`

Print each one with \`IO.puts\`.
`,
  starterCode: `name = "Alice"
age = 30
active = true
status = :ok
IO.puts(name)
IO.puts(age)
IO.puts(active)
IO.puts(status)
`,
  solution: `name = "Alice"
age = 30
active = true
status = :ok
IO.puts(name)
IO.puts(age)
IO.puts(active)
IO.puts(status)
`,
  tests: [
    {
      name: "prints name, age, active, status",
      expected: "Alice\n30\ntrue\nok\n",
    },
  ],
};
