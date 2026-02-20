import type { Lesson } from "../../types";

export const grep: Lesson = {
  id: "grep",
  title: "Searching Text",
  chapterId: "text-processing",
  content: `## Finding Patterns in Files

\`grep\` (**global regular expression print**) searches for lines that match a pattern and prints them.

### Basic Usage

\`\`\`bash
grep "pattern" filename
\`\`\`

Example:
\`\`\`bash
grep "Linux" notes.txt
\`\`\`

If \`notes.txt\` contains:
\`\`\`
Learn Linux
Practice daily
Have fun
\`\`\`

Output:
\`\`\`
Learn Linux
\`\`\`

Only lines containing "Linux" are printed.

### Common Flags

| Flag | Meaning |
|------|---------|
| \`-i\` | Case-insensitive search |
| \`-v\` | Invert match — print lines that do NOT match |
| \`-n\` | Show line numbers |
| \`-r\` | Recursive — search all files in a directory |
| \`-l\` | Show only filenames that contain matches |

### Examples

Search case-insensitively:
\`\`\`bash
grep -i "linux" notes.txt
\`\`\`

Print lines that do NOT contain "Linux":
\`\`\`bash
grep -v "Linux" notes.txt
\`\`\`

Output:
\`\`\`
Practice daily
Have fun
\`\`\`

### Regular Expressions

The "pattern" can be a regular expression:
\`\`\`bash
grep "^P" notes.txt    # lines starting with P
grep "y$" notes.txt    # lines ending with y
\`\`\`

### Your Task

Search \`notes.txt\` for lines containing \`Linux\`.`,

  starterCode: `# Search notes.txt for lines containing "Linux"
`,

  solution: `grep "Linux" notes.txt
`,

  tests: [
    {
      name: "finds lines with Linux",
      expected: "Learn Linux\n",
    },
  ],
};
