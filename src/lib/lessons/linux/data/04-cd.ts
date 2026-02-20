import type { Lesson } from "../../types";

export const cd: Lesson = {
  id: "cd",
  title: "Changing Directories",
  chapterId: "the-shell",
  content: `## Moving Around the Filesystem

\`cd\` stands for **change directory**. It moves your working directory to a new location.

### Basic Usage

\`\`\`bash
cd docs
\`\`\`

This moves into the \`docs\` subdirectory of the current directory. After this, \`pwd\` would show \`/home/user/docs\`.

### Relative vs Absolute Paths

A **relative path** is interpreted from your current location:
\`\`\`bash
cd docs          # go into docs/ from wherever you are
cd ../..         # go up two levels
\`\`\`

An **absolute path** starts from root:
\`\`\`bash
cd /home/user    # always goes to /home/user, regardless of where you are
cd /etc          # always goes to /etc
\`\`\`

### Special Shortcuts

| Shortcut | Meaning |
|----------|---------|
| \`cd\` | Go to your home directory |
| \`cd ~\` | Go to your home directory |
| \`cd ..\` | Go up one level (to the parent directory) |
| \`cd -\` | Go to the previous directory |

### Your Task

Change into the \`docs\` directory and then print the working directory with \`pwd\`.`,

  starterCode: `# Change into the docs directory, then print the working directory
`,

  solution: `cd docs
pwd
`,

  tests: [
    {
      name: "navigates to docs directory",
      expected: "/home/user/docs\n",
    },
  ],
};
