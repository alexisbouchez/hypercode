import type { Lesson } from "../../types";

export const floatingPoint: Lesson = {
  id: "floating-point",
  title: "Floating Point",
  chapterId: "types-and-variables",
  content: `## Floating Point

HolyC has one floating-point type: \`F64\`, a 64-bit double-precision float. This is equivalent to C's \`double\`.

### Declaring Floats

\`\`\`holyc
F64 pi = 3.14159;
F64 e  = 2.71828;
F64 temperature = -17.5;
\`\`\`

### Arithmetic

Float arithmetic works exactly as in C. Integer literals are automatically promoted:

\`\`\`holyc
F64 x = 1.5;
F64 y = x * 2.0;   // 3.0
F64 z = x + 0.5;   // 2.0
\`\`\`

### Printing Floats

Use \`%f\` for standard decimal output, or \`%.Nf\` to control decimal places:

\`\`\`holyc
F64 pi = 3.14159;
Print("%f\\n", pi);     // 3.141590
Print("%.2f\\n", pi);  // 3.14
Print("%.4f\\n", pi);  // 3.1416
\`\`\`

### Mixing Integers and Floats

You can assign an integer to a float variable; HolyC converts it automatically:

\`\`\`holyc
F64 half = 1.0 / 2.0;   // 0.5   (float division)
I64 whole = 1 / 2;      // 0     (integer division truncates)
\`\`\`

### Your Task

Compute the area of a circle with radius \`7.0\` using \`pi = 3.14159\`.

Formula: \`area = pi * r * r\`

Print the result with 2 decimal places: \`153.94\``,

  starterCode: `F64 pi = 3.14159;
F64 r = 7.0;
// Compute and print the area with 2 decimal places
`,

  solution: `F64 pi = 3.14159;
F64 r = 7.0;
F64 area = pi * r * r;
Print("%.2f\\n", area);
`,

  tests: [
    {
      name: "prints circle area",
      expected: "153.94\n",
    },
  ],
};
