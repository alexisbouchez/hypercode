import type { Lesson } from "../../types";

export const rm: Lesson = {
  id: "rm",
  title: "Removing Files",
  chapterId: "working-with-files",
  content: `## Deleting Files

\`rm\` (**remove**) permanently deletes files. Unlike a desktop recycle bin, there is no undo.

### Basic Usage

\`\`\`bash
rm notes.txt
\`\`\`

### Removing Multiple Files

\`\`\`bash
rm file1.txt file2.txt file3.txt
\`\`\`

### Removing Directories

\`rm\` refuses to remove directories by default:
\`\`\`bash
rm docs/     # error: cannot remove 'docs/': Is a directory
\`\`\`

Use \`-r\` (recursive) to remove a directory and everything inside it:
\`\`\`bash
rm -r docs/
\`\`\`

### The \`-f\` Flag

\`-f\` (**force**) suppresses error messages about non-existent files and never prompts for confirmation:
\`\`\`bash
rm -f nonexistent.txt    # no error even if the file doesn't exist
rm -rf old-directory/    # force-remove a directory, no prompts
\`\`\`

> **Warning:** \`rm -rf\` is one of the most dangerous commands in Linux. It will delete everything in the specified path with no confirmation. Always double-check the path before running it.

### Safer Alternative

Use \`-i\` to ask for confirmation before each deletion:
\`\`\`bash
rm -i *.txt    # asks "remove file?" for each .txt file
\`\`\`

### Your Task

Remove \`notes.txt\`, then list the directory to confirm it was deleted.`,

  starterCode: `# Remove notes.txt, then list the directory
`,

  solution: `rm notes.txt
ls
`,

  tests: [
    {
      name: "removes notes.txt",
      expected: "docs\nhello.txt\n",
    },
  ],
};
