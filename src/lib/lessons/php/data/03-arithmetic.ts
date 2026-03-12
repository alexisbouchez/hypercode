import type { Lesson } from "../../types";

export const arithmetic: Lesson = {
  id: "arithmetic",
  title: "Arithmetic",
  chapterId: "basics",
  content: `## Arithmetic Operators

PHP supports the usual arithmetic operators:

\`\`\`php
$a = 10;
$b = 3;
echo $a + $b . "\\n";  // 13
echo $a - $b . "\\n";  // 7
echo $a * $b . "\\n";  // 30
echo $a / $b . "\\n";  // 3.3333333333333
echo $a % $b . "\\n";  // 1
echo $a ** $b . "\\n"; // 1000
\`\`\`

The \`.\` operator concatenates strings. When you concatenate a number with a string, PHP converts the number automatically.

### Your Task

Given \`$x = 17\` and \`$y = 5\`, print:
- Their sum
- Their integer division result (use \`intdiv()\`)
- The remainder`,

  starterCode: `<?php
$x = 17;
$y = 5;
echo ($x + $y) . "\\n";
echo intdiv($x, $y) . "\\n";
echo ($x % $y) . "\\n";
`,

  solution: `<?php
$x = 17;
$y = 5;
echo ($x + $y) . "\\n";
echo intdiv($x, $y) . "\\n";
echo ($x % $y) . "\\n";
`,

  tests: [
    {
      name: "prints 22, 3, 2",
      expected: "22\n3\n2\n",
    },
  ],
};
