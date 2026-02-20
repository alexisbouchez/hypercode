import type { Lesson } from "../../types";

export const mkdir: Lesson = {
  id: "mkdir",
  title: "Creating Directories",
  chapterId: "working-with-files",
  content: `## Making New Directories

\`mkdir\` (**make directory**) creates one or more new directories.

### Basic Usage

\`\`\`bash
mkdir projects
\`\`\`

This creates a new directory called \`projects\` inside the current directory.

### Creating Multiple Directories

\`\`\`bash
mkdir src tests docs
\`\`\`

### Creating Nested Directories

By default, \`mkdir\` fails if a parent directory doesn't exist:
\`\`\`bash
mkdir a/b/c   # fails if a/ doesn't exist
\`\`\`

Use \`-p\` to create all parents as needed:
\`\`\`bash
mkdir -p a/b/c   # creates a/, a/b/, and a/b/c/ as needed
\`\`\`

The \`-p\` flag also suppresses errors if the directory already exists — useful in scripts.

### Verifying Creation

After creating a directory, use \`ls\` to confirm it appeared:
\`\`\`bash
mkdir projects
ls
\`\`\`

Output:
\`\`\`
docs
hello.txt
notes.txt
projects
\`\`\`

### Your Task

Create a directory called \`projects\`, then list the current directory to confirm it was created.`,

  starterCode: `# Create a directory called "projects" then list the directory
`,

  solution: `mkdir projects
ls
`,

  tests: [
    {
      name: "creates projects directory",
      expected: "docs\nhello.txt\nnotes.txt\nprojects\n",
    },
  ],
};
