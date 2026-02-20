import type { Lesson } from "../../types";

export const conditionals: Lesson = {
  id: "conditionals",
  title: "Conditionals",
  chapterId: "shell-scripting",
  content: `## Making Decisions

Shell scripts can take different actions based on conditions. The \`if\` statement is the primary tool for this.

### Basic \`if\` / \`else\`

\`\`\`bash
if [ condition ]; then
  echo "condition is true"
else
  echo "condition is false"
fi
\`\`\`

The \`[\` command evaluates the condition and returns true or false. \`fi\` ends the block (it is \`if\` spelled backward).

### File Tests

The most common conditions check whether files exist:

| Test | True when |
|------|-----------|
| \`-f FILE\` | FILE exists and is a regular file |
| \`-d FILE\` | FILE exists and is a directory |
| \`-e FILE\` | FILE exists (file or directory) |

Example:
\`\`\`bash
if [ -f hello.txt ]; then
  echo "File exists"
else
  echo "Not found"
fi
\`\`\`

Since \`hello.txt\` exists, the output is:
\`\`\`
File exists
\`\`\`

### String Tests

| Test | True when |
|------|-----------|
| \`"$A" = "$B"\` | strings are equal |
| \`"$A" != "$B"\` | strings differ |
| \`-z "$A"\` | string is empty |
| \`-n "$A"\` | string is non-empty |

### Number Tests

| Test | True when |
|------|-----------|
| \`$A -eq $B\` | equal |
| \`$A -ne $B\` | not equal |
| \`$A -lt $B\` | less than |
| \`$A -gt $B\` | greater than |

### Your Task

Write an \`if\` statement that checks whether \`hello.txt\` exists. If it does, print \`File exists\`. Otherwise, print \`Not found\`.`,

  starterCode: `# Check if hello.txt exists and print the appropriate message
`,

  solution: `if [ -f hello.txt ]; then
  echo "File exists"
else
  echo "Not found"
fi
`,

  tests: [
    {
      name: "detects file existence",
      expected: "File exists\n",
    },
  ],
};
