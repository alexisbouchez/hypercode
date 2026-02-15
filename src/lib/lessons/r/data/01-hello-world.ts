import type { Lesson } from "../../types";

export const helloWorld: Lesson = {
  id: "hello-world",
  title: "Hello, World!",
  chapterId: "foundations",
  content: `## Your First R Program

The simplest way to print output in R is the \`cat()\` function:

\`\`\`r
cat("Hello, World!\\n")
\`\`\`

The \`\\n\` at the end adds a newline character. Unlike \`print()\`, \`cat()\` outputs raw text without quotes or line numbers.

### Comments

Comments in R start with \`#\`:

\`\`\`r
# This is a comment
cat("Hello\\n")  # This is also a comment
\`\`\`

### \`cat()\` vs \`print()\`

R has two main output functions:

| Function | Description |
|----------|-------------|
| \`cat()\` | Outputs raw text, no quotes, no newline unless you add \`\\n\` |
| \`print()\` | Outputs a formatted representation with \`[1]\` prefix and quotes for strings |

Throughout this course, we use \`cat()\` for predictable output formatting.

### Your Task

Write a program that prints exactly \`Hello, World!\` followed by a newline.`,

  starterCode: `# Print "Hello, World!" here
`,

  solution: `cat("Hello, World!\\n")
`,

  tests: [
    {
      name: "prints Hello, World!",
      expected: "Hello, World!\n",
    },
  ],
};
