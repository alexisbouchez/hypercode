import type { Lesson } from "../../types";

export const ifElse: Lesson = {
  id: "if-else",
  title: "If / Else",
  chapterId: "control-flow",
  content: `## Conditional Statements

PHP uses \`if\`, \`elseif\`, and \`else\`:

\`\`\`php
$age = 18;
if ($age >= 18) {
    echo "Adult\\n";
} elseif ($age >= 13) {
    echo "Teenager\\n";
} else {
    echo "Child\\n";
}
\`\`\`

### Comparison Operators

| Operator | Meaning |
|----------|---------|
| \`==\`  | Equal (loose) |
| \`===\` | Identical (strict) |
| \`!=\`  | Not equal |
| \`<\`   | Less than |
| \`>\`   | Greater than |
| \`<=\`  | Less or equal |
| \`>=\`  | Greater or equal |

### Your Task

Given \`$score = 85\`, print the grade:
- 90+ → \`A\`
- 80+ → \`B\`
- 70+ → \`C\`
- else → \`F\``,

  starterCode: `<?php
$score = 85;
if ($score >= 90) {
    echo "A\\n";
} elseif ($score >= 80) {
    echo "B\\n";
} elseif ($score >= 70) {
    echo "C\\n";
} else {
    echo "F\\n";
}
`,

  solution: `<?php
$score = 85;
if ($score >= 90) {
    echo "A\\n";
} elseif ($score >= 80) {
    echo "B\\n";
} elseif ($score >= 70) {
    echo "C\\n";
} else {
    echo "F\\n";
}
`,

  tests: [
    {
      name: "prints B for score 85",
      expected: "B\n",
    },
  ],
};
