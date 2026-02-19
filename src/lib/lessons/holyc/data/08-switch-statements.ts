import type { Lesson } from "../../types";

export const switchStatements: Lesson = {
  id: "switch-statements",
  title: "Switch Statements",
  chapterId: "control-flow",
  content: `## Switch Statements

HolyC's \`switch\` works like C's, with one powerful addition: **range cases**.

### Basic Switch

\`\`\`holyc
I64 day = 3;
switch (day) {
  case 1: Print("Monday\\n");    break;
  case 2: Print("Tuesday\\n");   break;
  case 3: Print("Wednesday\\n"); break;
  case 4: Print("Thursday\\n");  break;
  case 5: Print("Friday\\n");    break;
  default: Print("Weekend\\n");  break;
}
\`\`\`

### Range Cases

HolyC extends \`switch\` with a range syntax using \`...\`:

\`\`\`holyc
I64 score = 85;
switch (score) {
  case 90...100: Print("A\\n"); break;
  case 80...89:  Print("B\\n"); break;
  case 70...79:  Print("C\\n"); break;
  case 60...69:  Print("D\\n"); break;
  default:       Print("F\\n"); break;
}
\`\`\`

The range \`case 80...89:\` matches any value from 80 to 89, inclusive. This eliminates the need for a chain of \`if-else if\` comparisons.

### Fall-Through

Without a \`break\`, execution falls through to the next case (same as C):

\`\`\`holyc
I64 x = 1;
switch (x) {
  case 1:
  case 2:
    Print("one or two\\n");
    break;
  case 3:
    Print("three\\n");
    break;
}
\`\`\`

### Your Task

Given \`I64 month = 7\`, use a \`switch\` with range cases to print the season:
- Months 3–5: \`Spring\`
- Months 6–8: \`Summer\`
- Months 9–11: \`Autumn\`
- Months 12, 1, 2: \`Winter\``,

  starterCode: `I64 month = 7;
// Use switch with range cases to print the season
`,

  solution: `I64 month = 7;
switch (month) {
  case 3...5:  Print("Spring\\n"); break;
  case 6...8:  Print("Summer\\n"); break;
  case 9...11: Print("Autumn\\n"); break;
  default:     Print("Winter\\n"); break;
}
`,

  tests: [
    {
      name: "prints Summer for month 7",
      expected: "Summer\n",
    },
  ],
};
