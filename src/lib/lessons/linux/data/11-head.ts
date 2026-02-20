import type { Lesson } from "../../types";

export const head: Lesson = {
  id: "head",
  title: "First Lines",
  chapterId: "text-processing",
  content: `## Viewing the Start of a File

When a file is large, you often want to see just the beginning or end — not the whole thing. \`head\` and \`tail\` are built for this.

### The \`head\` Command

\`head\` prints the first lines of a file. By default it shows 10 lines:

\`\`\`bash
head notes.txt
\`\`\`

### Specifying the Number of Lines

Use \`-n\` to control how many lines are shown:

\`\`\`bash
head -n 2 notes.txt
\`\`\`

If \`notes.txt\` contains:
\`\`\`
Learn Linux
Practice daily
Have fun
\`\`\`

Then \`head -n 2 notes.txt\` outputs:
\`\`\`
Learn Linux
Practice daily
\`\`\`

### The \`tail\` Command

\`tail\` is the mirror image: it shows the last N lines:

\`\`\`bash
tail -n 1 notes.txt
\`\`\`

Output:
\`\`\`
Have fun
\`\`\`

### Following a File in Real Time

One powerful use of \`tail\` is watching log files as they grow:
\`\`\`bash
tail -f /var/log/app.log
\`\`\`

The \`-f\` flag (follow) keeps the command running and prints new lines as they are appended.

### Your Task

Print the first 2 lines of \`notes.txt\` using \`head -n 2\`.`,

  starterCode: `# Print the first 2 lines of notes.txt
`,

  solution: `head -n 2 notes.txt
`,

  tests: [
    {
      name: "prints first 2 lines",
      expected: "Learn Linux\nPractice daily\n",
    },
  ],
};
