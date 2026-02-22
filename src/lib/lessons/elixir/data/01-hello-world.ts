import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "foundations",
  content: `## Your First Elixir Program

Elixir uses \`IO.puts\` to print output with a newline.

\`\`\`elixir
IO.puts("Hello, World!")
\`\`\`

- \`IO\` is the built-in input/output module
- \`puts\` stands for "put string" — it prints and adds a newline
- Strings are enclosed in double quotes

## Your Turn

Print \`Hello, Elixir!\` to the output.
`,
  starterCode: `IO.puts("Hello, Elixir!")
`,
  solution: `IO.puts("Hello, Elixir!")
`,
  tests: [
    {
      name: 'prints "Hello, Elixir!"',
      expected: "Hello, Elixir!\n",
    },
  ],
};
