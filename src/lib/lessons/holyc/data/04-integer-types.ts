import type { Lesson } from "../../types";

export const integerTypes: Lesson = {
  id: "integer-types",
  title: "Integer Types",
  chapterId: "types-and-variables",
  content: `## Integer Types

HolyC has explicit integer types that encode both signedness and size. Every type name is capitalized.

### Signed Integer Types

| Type | Size | Range |
|------|------|-------|
| \`I8\` | 8-bit | −128 to 127 |
| \`I16\` | 16-bit | −32,768 to 32,767 |
| \`I32\` | 32-bit | −2,147,483,648 to 2,147,483,647 |
| \`I64\` | 64-bit | −9.2 × 10¹⁸ to 9.2 × 10¹⁸ |

### Unsigned Integer Types

| Type | Size | Range |
|------|------|-------|
| \`U8\` | 8-bit | 0 to 255 |
| \`U16\` | 16-bit | 0 to 65,535 |
| \`U32\` | 32-bit | 0 to 4,294,967,295 |
| \`U64\` | 64-bit | 0 to 1.8 × 10¹⁹ |

### Declaring Variables

\`\`\`holyc
I64 population = 8000000000;
I32 year = 2024;
I8 small = 127;
U8 byte_val = 255;
\`\`\`

### Arithmetic

All standard arithmetic operators work as in C: \`+\`, \`-\`, \`*\`, \`/\`, \`%\`.

HolyC also has the exponentiation operator \`**\`:

\`\`\`holyc
I64 squared = 5 ** 2;  // 25
I64 cubed   = 3 ** 3;  // 27
\`\`\`

### Your Task

Declare an \`I64\` variable \`a = 12\` and an \`I64\` variable \`b = 5\`. Print the result of \`a * b\` on one line.

Expected output: \`60\``,

  starterCode: `I64 a = 12;
I64 b = 5;
// Print a * b
`,

  solution: `I64 a = 12;
I64 b = 5;
Print("%d\\n", a * b);
`,

  tests: [
    {
      name: "prints 60",
      expected: "60\n",
    },
  ],
};
