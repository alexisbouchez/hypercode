import type { Lesson } from "../../types";

export const wc: Lesson = {
  id: "wc",
  title: "Counting Lines",
  chapterId: "text-processing",
  content: `## Word Count

\`wc\` (**word count**) counts lines, words, and characters in a file or stdin.

### Basic Usage

\`\`\`bash
wc notes.txt
\`\`\`

Output:
\`\`\`
3 6 34 notes.txt
\`\`\`

The three numbers are: **lines**, **words**, **characters** (bytes).

### Counting Only Lines

The \`-l\` flag counts only lines:

\`\`\`bash
wc -l notes.txt
\`\`\`

Output:
\`\`\`
3 notes.txt
\`\`\`

The number is the count of newline characters. A standard text file with 3 lines has 3 newlines.

### Other Flags

| Flag | Meaning |
|------|---------|
| \`-l\` | Count lines |
| \`-w\` | Count words |
| \`-c\` | Count characters (bytes) |

### With Pipes

\`wc\` is very useful at the end of a pipeline:

\`\`\`bash
grep "Linux" notes.txt | wc -l
\`\`\`

Output:
\`\`\`
1
\`\`\`

This counts how many lines matched the pattern.

\`\`\`bash
ls | wc -l
\`\`\`

Counts the number of items in the current directory.

### Your Task

Count the number of lines in \`notes.txt\` using \`wc -l\`.`,

  starterCode: `# Count the lines in notes.txt
`,

  solution: `wc -l notes.txt
`,

  tests: [
    {
      name: "counts lines in notes.txt",
      expected: "3 notes.txt\n",
    },
  ],
};
