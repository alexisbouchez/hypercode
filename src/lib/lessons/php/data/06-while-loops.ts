import type { Lesson } from "../../types";

export const whileLoops: Lesson = {
  id: "while-loops",
  title: "While Loops",
  chapterId: "control-flow",
  content: `## While Loops

A \`while\` loop repeats as long as the condition is true:

\`\`\`php
$i = 1;
while ($i <= 3) {
    echo "$i\\n";
    $i++;
}
\`\`\`

### Do-While

A \`do-while\` loop runs at least once:

\`\`\`php
$i = 1;
do {
    echo "$i\\n";
    $i++;
} while ($i <= 3);
\`\`\`

### Your Task

Print the numbers 1 through 5, one per line, using a \`while\` loop.`,

  starterCode: `<?php
$i = 1;
while ($i <= 5) {
    echo "$i\\n";
    $i++;
}
`,

  solution: `<?php
$i = 1;
while ($i <= 5) {
    echo "$i\\n";
    $i++;
}
`,

  tests: [
    {
      name: "prints 1 through 5",
      expected: "1\n2\n3\n4\n5\n",
    },
  ],
};
