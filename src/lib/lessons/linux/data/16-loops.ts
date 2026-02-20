import type { Lesson } from "../../types";

export const loops: Lesson = {
  id: "loops",
  title: "Loops",
  chapterId: "shell-scripting",
  content: `## Repeating Commands

Shell loops let you run commands multiple times — over a list of values, over files, or until a condition changes.

### The \`for\` Loop

The most common loop iterates over a list:

\`\`\`bash
for i in 1 2 3; do
  echo "Number $i"
done
\`\`\`

Output:
\`\`\`
Number 1
Number 2
Number 3
\`\`\`

Structure:
- \`for VARIABLE in LIST; do\` — starts the loop
- \`COMMANDS\` — body (can be multiple lines)
- \`done\` — ends the loop

The variable (\`i\`) takes each value from the list in turn.

### Iterating Over Files

\`\`\`bash
for file in *.txt; do
  echo "Found: $file"
done
\`\`\`

The \`*.txt\` glob expands to all \`.txt\` files in the current directory.

### Iterating Over a Range

\`\`\`bash
for i in $(seq 1 5); do
  echo "$i"
done
\`\`\`

Or with brace expansion (bash):
\`\`\`bash
for i in {1..5}; do
  echo "$i"
done
\`\`\`

### The \`while\` Loop

\`while\` runs as long as a condition is true:

\`\`\`bash
COUNT=1
while [ $COUNT -le 3 ]; do
  echo "Count: $COUNT"
  COUNT=$((COUNT + 1))
done
\`\`\`

### Your Task

Write a \`for\` loop that prints \`Number 1\`, \`Number 2\`, and \`Number 3\` on separate lines.`,

  starterCode: `# Loop over 1 2 3 and print "Number N" for each
`,

  solution: `for i in 1 2 3; do
  echo "Number $i"
done
`,

  tests: [
    {
      name: "prints Number 1, 2, 3",
      expected: "Number 1\nNumber 2\nNumber 3\n",
    },
  ],
};
