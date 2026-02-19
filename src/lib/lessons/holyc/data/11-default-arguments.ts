import type { Lesson } from "../../types";

export const defaultArguments: Lesson = {
  id: "default-arguments",
  title: "Default Arguments",
  chapterId: "functions",
  content: `## Default Arguments

HolyC supports **default argument values** — a feature that C does not have. You can specify a default for any parameter by writing \`= value\` after the type:

\`\`\`holyc
I64 Multiply(I64 a, I64 b=2) {
  return a * b;
}

Print("%d\\n", Multiply(5));     // 10  (b defaults to 2)
Print("%d\\n", Multiply(5, 3));  // 15  (b is 3)
\`\`\`

### Any Position

Unlike C++, HolyC allows default arguments at **any position**, not just the end:

\`\`\`holyc
I64 Clamp(I64 value, I64 lo=0, I64 hi=100) {
  if (value < lo) return lo;
  if (value > hi) return hi;
  return value;
}

Print("%d\\n", Clamp(150));        // 100
Print("%d\\n", Clamp(-5));         // 0
Print("%d\\n", Clamp(50, 0, 100)); // 50
\`\`\`

### Practical Use

Default arguments reduce boilerplate for functions that are commonly called with the same values:

\`\`\`holyc
U0 PrintHeader(U8 *title, U8 *border="===") {
  Print("%s\\n", border);
  Print("%s\\n", title);
  Print("%s\\n", border);
}

PrintHeader("Welcome");          // uses === borders
PrintHeader("Error", "!!!");     // uses !!! borders
\`\`\`

### Your Task

Write a function \`I64 Power(I64 base, I64 exp=2)\` that raises \`base\` to the power \`exp\` using a loop.

Call it as \`Power(4)\` (should return \`16\`) and \`Power(2, 8)\` (should return \`256\`).`,

  starterCode: `// Define Power(base, exp=2) using a loop

// Print Power(4) and Power(2, 8)
`,

  solution: `I64 Power(I64 base, I64 exp=2) {
  I64 result = 1;
  for (I64 i = 0; i < exp; i++) {
    result = result * base;
  }
  return result;
}

Print("%d\\n", Power(4));
Print("%d\\n", Power(2, 8));
`,

  tests: [
    {
      name: "Power(4) returns 16",
      code: "{{FUNC}}\nPrint(\"%d\\n\", Power(4));",
      expected: "16\n",
    },
    {
      name: "Power(2, 8) returns 256",
      code: "{{FUNC}}\nPrint(\"%d\\n\", Power(2, 8));",
      expected: "256\n",
    },
  ],
};
