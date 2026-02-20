import type { Lesson } from "../../types";

export const ls: Lesson = {
  id: "ls",
  title: "Listing Files",
  chapterId: "the-shell",
  content: `## Seeing What's There

Before you can work with files, you need to know what exists. The \`ls\` command lists the contents of a directory.

### Basic Usage

\`\`\`bash
ls
\`\`\`

This lists the files and directories in your current working directory, one per line (when output is not a terminal).

### Common Flags

Flags (also called options) modify a command's behavior. They start with a dash:

| Flag | Meaning |
|------|---------|
| \`-l\` | Long format — shows permissions, owner, size, and date |
| \`-a\` | Show all files, including hidden ones (names starting with \`.\`) |
| \`-la\` | Combine both flags |

Example with \`-l\`:
\`\`\`bash
ls -l
\`\`\`

Output:
\`\`\`
drwxr-xr-x 1 user user      0 Jan  1 00:00 docs
-rw-r--r-- 1 user user     14 Jan  1 00:00 hello.txt
-rw-r--r-- 1 user user     34 Jan  1 00:00 notes.txt
\`\`\`

The first character shows the type: \`d\` for directory, \`-\` for file. The next nine characters are permissions.

### Listing a Specific Path

You can give \`ls\` a path to list:

\`\`\`bash
ls /etc
\`\`\`

### Your Task

Run \`ls\` to list the contents of the current directory.`,

  starterCode: `# List the contents of the current directory
`,

  solution: `ls
`,

  tests: [
    {
      name: "lists directory contents",
      expected: "docs\nhello.txt\nnotes.txt\n",
    },
  ],
};
