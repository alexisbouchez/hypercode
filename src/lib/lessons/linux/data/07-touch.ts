import type { Lesson } from "../../types";

export const touch: Lesson = {
  id: "touch",
  title: "Creating Files",
  chapterId: "working-with-files",
  content: `## Creating Empty Files

\`touch\` creates a new empty file if it doesn't exist. If the file already exists, it updates its last-modified timestamp without changing the content.

### Basic Usage

\`\`\`bash
touch newfile.txt
\`\`\`

This creates an empty file called \`newfile.txt\` in the current directory.

### Creating Multiple Files

\`\`\`bash
touch file1.txt file2.txt file3.txt
\`\`\`

### Why Use touch?

\`touch\` is useful for:
- **Quickly creating placeholder files** — you can create the file structure of a project before writing any content
- **Updating timestamps** — some tools check modification times to decide whether to rerun a build step
- **Testing** — quickly create files to test scripts that process them

### Verifying Creation

\`\`\`bash
touch newfile.txt
ls
\`\`\`

Output:
\`\`\`
docs
hello.txt
newfile.txt
notes.txt
\`\`\`

To actually put content in a file, you use \`echo\` with output redirection (covered later), or a text editor like \`nano\` or \`vim\`.

### Your Task

Create a new empty file called \`newfile.txt\`, then list the directory to confirm it was created.`,

  starterCode: `# Create a file called "newfile.txt" then list the directory
`,

  solution: `touch newfile.txt
ls
`,

  tests: [
    {
      name: "creates newfile.txt",
      expected: "docs\nhello.txt\nnewfile.txt\nnotes.txt\n",
    },
  ],
};
