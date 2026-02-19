import type { Lesson } from "../../types";

export const printAndOutput: Lesson = {
  id: "print-and-output",
  title: "Print and Output",
  chapterId: "the-temple",
  content: `## Print and Output

HolyC's \`Print\` function is a powerful, printf-compatible built-in. You use it for all text output.

### Format Specifiers

\`Print\` supports the standard C format specifiers:

| Specifier | Type | Example |
|-----------|------|---------|
| \`%d\` | Signed integer | \`Print("%d\\n", 42);\` → \`42\` |
| \`%u\` | Unsigned integer | \`Print("%u\\n", 255);\` → \`255\` |
| \`%f\` | Floating point | \`Print("%f\\n", 3.14);\` → \`3.140000\` |
| \`%s\` | String | \`Print("%s\\n", "hi");\` → \`hi\` |
| \`%c\` | Character | \`Print("%c\\n", 65);\` → \`A\` |
| \`%x\` | Hexadecimal | \`Print("%x\\n", 255);\` → \`ff\` |

### Multiple Values

You can print multiple values in one call:

\`\`\`holyc
Print("x=%d y=%d\\n", 10, 20);
\`\`\`

This prints: \`x=10 y=20\`

### String Literals

HolyC string literals use double quotes. Escape sequences work as in C:

- \`\\n\` — newline
- \`\\t\` — tab
- \`\\\\\` — backslash
- \`\\"\` — double quote

\`\`\`holyc
Print("Line 1\\nLine 2\\n");
Print("Tab:\\there\\n");
\`\`\`

### Your Task

Print three lines:
1. \`Name: HolyC\`
2. \`Version: 1\`
3. \`Year: 1999\`

Each on its own line.`,

  starterCode: `// Print three lines about HolyC
`,

  solution: `Print("Name: HolyC\\n");
Print("Version: %d\\n", 1);
Print("Year: %d\\n", 1999);
`,

  tests: [
    {
      name: "prints three lines",
      expected: "Name: HolyC\nVersion: 1\nYear: 1999\n",
    },
  ],
};
