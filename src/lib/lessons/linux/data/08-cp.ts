import type { Lesson } from "../../types";

export const cp: Lesson = {
  id: "cp",
  title: "Copying Files",
  chapterId: "working-with-files",
  content: `## Duplicating Files

\`cp\` (**copy**) copies files from one location to another. The original file is unchanged.

### Basic Syntax

\`\`\`bash
cp source destination
\`\`\`

### Copying a File to a New Name

\`\`\`bash
cp hello.txt hello-copy.txt
\`\`\`

This creates \`hello-copy.txt\` with the same content as \`hello.txt\`.

### Copying into a Directory

If the destination is an existing directory, the file is copied into it:
\`\`\`bash
cp hello.txt docs/
\`\`\`

This creates \`docs/hello.txt\`.

### Copying Directories

By default, \`cp\` refuses to copy directories. Use \`-r\` (recursive) to copy a directory and all its contents:

\`\`\`bash
cp -r docs/ docs-backup/
\`\`\`

### Useful Flags

| Flag | Meaning |
|------|---------|
| \`-r\` | Recursive — copy directories |
| \`-v\` | Verbose — show what's being copied |
| \`-i\` | Interactive — ask before overwriting |

### Your Task

Copy \`hello.txt\` to a new file called \`hello-copy.txt\`, then list the directory to confirm both files exist.`,

  starterCode: `# Copy hello.txt to hello-copy.txt, then list the directory
`,

  solution: `cp hello.txt hello-copy.txt
ls
`,

  tests: [
    {
      name: "copies hello.txt",
      expected: "docs\nhello-copy.txt\nhello.txt\nnotes.txt\n",
    },
  ],
};
