import type { Lesson } from "../../types";

export const echo: Lesson = {
  id: "echo",
  title: "Hello, Shell!",
  chapterId: "the-shell",
  content: `## Your First Shell Command

The shell is a text interface to your operating system. You type commands, press Enter, and the shell executes them. It is the most direct way to interact with a Linux system.

### The \`echo\` Command

\`echo\` prints text to the screen. It is the simplest way to produce output:

\`\`\`bash
echo "Hello, Linux!"
\`\`\`

Output:
\`\`\`
Hello, Linux!
\`\`\`

Quotes group words together. Without quotes, the shell might interpret special characters in unexpected ways. Always use quotes around text that contains spaces or special characters.

### Running Multiple Commands

You can run multiple commands by putting each on its own line:

\`\`\`bash
echo "First line"
echo "Second line"
\`\`\`

Output:
\`\`\`
First line
Second line
\`\`\`

### Your Task

Print \`Hello, Linux!\` using the \`echo\` command.`,

  starterCode: `# Print "Hello, Linux!" using echo
`,

  solution: `echo "Hello, Linux!"
`,

  tests: [
    {
      name: "prints Hello, Linux!",
      expected: "Hello, Linux!\n",
    },
  ],
};
