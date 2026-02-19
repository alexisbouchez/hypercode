import type { Lesson } from "../../types";

export const ifElse: Lesson = {
  id: "if-else",
  title: "If and Else",
  chapterId: "control-flow",
  content: `## If and Else

HolyC's \`if\`/\`else\` syntax is identical to C. The condition must be in parentheses, and the body can be a single statement or a block in braces.

### Basic If

\`\`\`holyc
I64 x = 10;
if (x > 5) {
  Print("x is greater than 5\\n");
}
\`\`\`

### If-Else

\`\`\`holyc
I64 score = 72;
if (score >= 90) {
  Print("A\\n");
} else {
  Print("Not A\\n");
}
\`\`\`

### If-Else If-Else

\`\`\`holyc
I64 score = 85;
if (score >= 90) {
  Print("A\\n");
} else if (score >= 80) {
  Print("B\\n");
} else if (score >= 70) {
  Print("C\\n");
} else {
  Print("F\\n");
}
\`\`\`

### Comparison Operators

| Operator | Meaning |
|----------|---------|
| \`==\` | Equal |
| \`!=\` | Not equal |
| \`<\` | Less than |
| \`>\` | Greater than |
| \`<=\` | Less than or equal |
| \`>=\` | Greater than or equal |

### Logical Operators

- \`&&\` — logical AND
- \`||\` — logical OR
- \`!\` — logical NOT

\`\`\`holyc
I64 age = 25;
Bool has_id = TRUE;
if (age >= 18 && has_id) {
  Print("Access granted\\n");
}
\`\`\`

### Your Task

Given \`I64 temp = 35\`:
- If \`temp > 30\`, print \`Hot\`
- If \`temp > 20\`, print \`Warm\`
- Otherwise print \`Cool\``,

  starterCode: `I64 temp = 35;
// Print Hot, Warm, or Cool based on temp
`,

  solution: `I64 temp = 35;
if (temp > 30) {
  Print("Hot\\n");
} else if (temp > 20) {
  Print("Warm\\n");
} else {
  Print("Cool\\n");
}
`,

  tests: [
    {
      name: "prints Hot for 35",
      expected: "Hot\n",
    },
  ],
};
