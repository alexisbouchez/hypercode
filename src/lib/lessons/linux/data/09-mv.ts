import type { Lesson } from "../../types";

export const mv: Lesson = {
  id: "mv",
  title: "Moving and Renaming",
  chapterId: "working-with-files",
  content: `## Moving and Renaming Files

\`mv\` (**move**) moves a file to a new location. If the destination is in the same directory, it renames the file.

### Renaming a File

\`\`\`bash
mv hello.txt greet.txt
\`\`\`

This renames \`hello.txt\` to \`greet.txt\`. The original \`hello.txt\` no longer exists.

### Moving to a Different Directory

\`\`\`bash
mv notes.txt docs/
\`\`\`

This moves \`notes.txt\` into the \`docs\` directory.

### Moving and Renaming Simultaneously

\`\`\`bash
mv hello.txt docs/greeting.txt
\`\`\`

This moves \`hello.txt\` into \`docs/\` and renames it to \`greeting.txt\` at the same time.

### Moving Multiple Files

\`\`\`bash
mv file1.txt file2.txt target-dir/
\`\`\`

When the last argument is a directory, all preceding arguments are moved into it.

### Key Difference from cp

\`mv\` does not make a copy — the original is gone. This is especially relevant when moving across filesystems (then \`mv\` copies and deletes).

### Your Task

Rename \`hello.txt\` to \`greet.txt\`, then list the directory to confirm the rename.`,

  starterCode: `# Rename hello.txt to greet.txt, then list the directory
`,

  solution: `mv hello.txt greet.txt
ls
`,

  tests: [
    {
      name: "renames hello.txt to greet.txt",
      expected: "docs\ngreet.txt\nnotes.txt\n",
    },
  ],
};
