import type { Lesson } from "../../types";

export const pwd: Lesson = {
  id: "pwd",
  title: "Where Am I?",
  chapterId: "the-shell",
  content: `## The Working Directory

When you open a shell, you are always in a particular directory. This is called the **working directory** or **current directory**. Every command you run is relative to this location.

### The \`pwd\` Command

\`pwd\` stands for **print working directory**. It shows you the full path of where you currently are:

\`\`\`bash
pwd
\`\`\`

Output:
\`\`\`
/home/user
\`\`\`

### Understanding Paths

Linux uses a single root directory \`/\`. Everything hangs off this root:

\`\`\`
/                   ← root
├── home/           ← user home directories
│   └── user/       ← your home directory
├── etc/            ← configuration files
├── tmp/            ← temporary files
└── var/            ← variable data (logs, etc.)
\`\`\`

The path \`/home/user\` means: start at root (\`/\`), go into \`home\`, go into \`user\`. The \`/\` at the beginning makes it an **absolute path** — it starts from the root, not from your current location.

When you first open a shell, you start in your **home directory**: \`/home/user\`.

### Your Task

Run \`pwd\` to print the current working directory.`,

  starterCode: `# Print the current directory
`,

  solution: `pwd
`,

  tests: [
    {
      name: "prints current directory",
      expected: "/home/user\n",
    },
  ],
};
