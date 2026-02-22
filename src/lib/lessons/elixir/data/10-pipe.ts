import type { Lesson } from "../../types";

export const pipe: Lesson = {
  id: "pipe",
  title: "Pipe Operator",
  chapterId: "functions",
  content: `## |>

The pipe operator \`|>\` passes the result of one expression as the first argument to the next. It makes data transformation pipelines readable:

Without pipe:
\`\`\`elixir
String.upcase(String.reverse("hello world"))
\`\`\`

With pipe:
\`\`\`elixir
"hello world"
|> String.reverse()
|> String.upcase()
\`\`\`

Both produce \`"DLROW OLLEH"\`.

## Your Turn

Use the pipe operator to:
1. Start with \`"hello world"\`
2. Reverse it with \`String.reverse\`
3. Uppercase it with \`String.upcase\`
4. Print the result with \`IO.puts\`
`,
  starterCode: `"hello world"
|> String.reverse()
|> String.upcase()
|> IO.puts()
`,
  solution: `"hello world"
|> String.reverse()
|> String.upcase()
|> IO.puts()
`,
  tests: [
    {
      name: "pipe through reverse and upcase",
      expected: "DLROW OLLEH\n",
    },
  ],
};
