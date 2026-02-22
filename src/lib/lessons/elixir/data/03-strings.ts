import type { Lesson } from "../../types";

export const strings: Lesson = {
  id: "strings",
  title: "Strings",
  chapterId: "foundations",
  content: `## String Interpolation

Use \`\#{expr}\` inside double-quoted strings to embed expressions.

\`\`\`elixir
lang = "Elixir"
IO.puts("Hello, \#{lang}!")
\`\`\`

## String Functions

The \`String\` module provides common operations:

\`\`\`elixir
IO.puts(String.length("hello"))    # 5
IO.puts(String.upcase("hello"))    # HELLO
IO.puts(String.reverse("hello"))   # olleh
\`\`\`

## Your Turn

Given \`lang = "Elixir"\`:
1. Print \`"Hello, Elixir!"\` using interpolation
2. Print the length of \`"Elixir"\` (6)
3. Print \`"ELIXIR"\` using \`String.upcase\`
4. Print \`"rixilE"\` using \`String.reverse\`
`,
  starterCode: `lang = "Elixir"
IO.puts("Hello, \#{lang}!")
IO.puts(String.length(lang))
IO.puts(String.upcase(lang))
IO.puts(String.reverse(lang))
`,
  solution: `lang = "Elixir"
IO.puts("Hello, \#{lang}!")
IO.puts(String.length(lang))
IO.puts(String.upcase(lang))
IO.puts(String.reverse(lang))
`,
  tests: [
    {
      name: "string interpolation and operations",
      expected: "Hello, Elixir!\n6\nELIXIR\nrixilE\n",
    },
  ],
};
