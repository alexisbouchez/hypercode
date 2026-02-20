import type { Lesson } from "../../types";

export const cat: Lesson = {
  id: "cat",
  title: "Reading Files",
  chapterId: "the-shell",
  content: `## Displaying File Contents

\`cat\` (short for **concatenate**) reads one or more files and prints their contents to the screen.

### Basic Usage

\`\`\`bash
cat hello.txt
\`\`\`

If \`hello.txt\` contains \`Hello, Linux!\`, the output is:
\`\`\`
Hello, Linux!
\`\`\`

### Reading Multiple Files

\`cat\` can read multiple files at once, printing them one after another (hence "concatenate"):

\`\`\`bash
cat hello.txt notes.txt
\`\`\`

### Absolute Paths

You can always give an absolute path:
\`\`\`bash
cat /etc/hosts
\`\`\`

Output:
\`\`\`
127.0.0.1 localhost
::1 localhost
\`\`\`

The \`/etc/hosts\` file maps hostnames to IP addresses. It is one of the most commonly viewed system files.

### Other File Viewing Commands

For large files, \`cat\` dumps everything at once. Alternatives:
- \`head file\` — print just the first 10 lines
- \`tail file\` — print just the last 10 lines
- \`less file\` — interactive pager (scroll up and down)

### Your Task

Read the contents of \`hello.txt\` using \`cat\`.`,

  starterCode: `# Display the contents of hello.txt
`,

  solution: `cat hello.txt
`,

  tests: [
    {
      name: "reads hello.txt",
      expected: "Hello, Linux!\n",
    },
  ],
};
