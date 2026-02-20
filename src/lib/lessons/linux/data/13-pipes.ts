import type { Lesson } from "../../types";

export const pipes: Lesson = {
  id: "pipes",
  title: "Pipes",
  chapterId: "text-processing",
  content: `## Connecting Commands Together

The **pipe** operator \`|\` takes the output of one command and feeds it as input to the next. This is one of the most powerful ideas in Linux: small tools that do one thing well, combined into powerful pipelines.

### Basic Syntax

\`\`\`bash
command1 | command2
\`\`\`

The standard output (stdout) of \`command1\` becomes the standard input (stdin) of \`command2\`.

### Example

\`\`\`bash
cat notes.txt | grep "daily"
\`\`\`

This does two things:
1. \`cat notes.txt\` reads the file and prints it
2. \`grep "daily"\` reads that output and filters for lines containing "daily"

If \`notes.txt\` contains:
\`\`\`
Learn Linux
Practice daily
Have fun
\`\`\`

Output:
\`\`\`
Practice daily
\`\`\`

### Longer Pipelines

Pipes can be chained indefinitely:
\`\`\`bash
cat notes.txt | grep "L" | head -n 1
\`\`\`

1. \`cat\` reads the file
2. \`grep "L"\` keeps lines with "L"
3. \`head -n 1\` keeps only the first

Output:
\`\`\`
Learn Linux
\`\`\`

### The Philosophy

The Unix philosophy: "Write programs that do one thing and do it well. Write programs to work together." Pipes are the glue.

Instead of one giant program that filters, counts, and sorts, you have \`grep\`, \`wc\`, and \`sort\` — each tiny, each composable.

### Your Task

Use a pipe to pass the contents of \`notes.txt\` through \`grep\` to find lines containing \`daily\`.`,

  starterCode: `# Pipe the contents of notes.txt through grep to find "daily"
`,

  solution: `cat notes.txt | grep "daily"
`,

  tests: [
    {
      name: "finds lines with daily",
      expected: "Practice daily\n",
    },
  ],
};
