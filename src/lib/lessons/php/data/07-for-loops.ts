import type { Lesson } from "../../types";

export const forLoops: Lesson = {
  id: "for-loops",
  title: "For Loops",
  chapterId: "control-flow",
  content: `## For Loops

PHP's \`for\` loop works like C:

\`\`\`php
for ($i = 0; $i < 5; $i++) {
    echo "$i\\n";
}
\`\`\`

### Foreach

\`foreach\` iterates over arrays:

\`\`\`php
$fruits = ["apple", "banana", "cherry"];
foreach ($fruits as $fruit) {
    echo "$fruit\\n";
}
\`\`\`

You can also get the key:

\`\`\`php
foreach ($fruits as $index => $fruit) {
    echo "$index: $fruit\\n";
}
\`\`\`

### Your Task

Print the first 5 even numbers (2, 4, 6, 8, 10) using a \`for\` loop.`,

  starterCode: `<?php
for ($i = 1; $i <= 5; $i++) {
    echo ($i * 2) . "\\n";
}
`,

  solution: `<?php
for ($i = 1; $i <= 5; $i++) {
    echo ($i * 2) . "\\n";
}
`,

  tests: [
    {
      name: "prints first 5 even numbers",
      expected: "2\n4\n6\n8\n10\n",
    },
  ],
};
