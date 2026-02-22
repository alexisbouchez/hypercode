import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "basics",
  content: `## Your First Swift Program

In Swift, \`print()\` outputs text to the console, followed by a newline:

\`\`\`swift
print("Hello, World!")
\`\`\`

You can print multiple values by separating them with commas — Swift will join them with spaces:

\`\`\`swift
print("The answer is", 42)
// The answer is 42
\`\`\`

### Comments

Single-line comments start with \`//\`, multi-line use \`/* ... */\`:

\`\`\`swift
// This is a comment
print("code") // inline comment
\`\`\`

### Your Task

Print exactly \`Hello, World!\`.`,

  starterCode: `print("Hello, World!")
`,

  solution: `print("Hello, World!")
`,

  tests: [
    {
      name: "prints Hello, World!",
      expected: "Hello, World!\n",
    },
  ],
};
