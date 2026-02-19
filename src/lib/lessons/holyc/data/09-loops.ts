import type { Lesson } from "../../types";

export const loops: Lesson = {
  id: "loops",
  title: "Loops",
  chapterId: "control-flow",
  content: `## Loops

HolyC supports \`for\` and \`while\` loops — identical to C. There is one important difference: **HolyC has no \`continue\` keyword**.

### For Loop

\`\`\`holyc
for (I64 i = 0; i < 5; i++) {
  Print("%d\\n", i);
}
\`\`\`

Output:
\`\`\`
0
1
2
3
4
\`\`\`

### While Loop

\`\`\`holyc
I64 n = 1;
while (n <= 4) {
  Print("%d\\n", n);
  n++;
}
\`\`\`

### No Continue — Use Goto

HolyC intentionally omits \`continue\`. To skip the rest of a loop body for an iteration, use \`goto\` to jump to a label at the end of the body:

\`\`\`holyc
for (I64 i = 0; i < 6; i++) {
  if (i == 3) goto skip;
  Print("%d\\n", i);
  skip:;
}
\`\`\`

Output:
\`\`\`
0
1
2
4
5
\`\`\`

The label \`skip:\` sits just before the closing brace. The semicolon after the label is required because a label must be followed by a statement (an empty statement \`;\` works).

This design forces explicit control flow — Terry Davis believed it made code easier to reason about.

### Your Task

Print the sum of all integers from 1 to 10.

Expected output: \`55\``,

  starterCode: `I64 sum = 0;
// Use a for loop to sum 1..10, then print the result
`,

  solution: `I64 sum = 0;
for (I64 i = 1; i <= 10; i++) {
  sum = sum + i;
}
Print("%d\\n", sum);
`,

  tests: [
    {
      name: "prints sum 1 to 10",
      expected: "55\n",
    },
  ],
};
