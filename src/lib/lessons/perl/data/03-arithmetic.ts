import type { Lesson } from "../../types";

export const arithmetic: Lesson = {
  id: "arithmetic",
  title: "Arithmetic",
  chapterId: "basics",
  content: `## Arithmetic Operations

Perl supports the standard arithmetic operators:

| Operator | Meaning |
|----------|---------|
| \`+\` | Addition |
| \`-\` | Subtraction |
| \`*\` | Multiplication |
| \`/\` | Division |
| \`%\` | Modulo |
| \`**\` | Exponentiation |

\`\`\`perl
my $sum = 10 + 3;     # 13
my $diff = 10 - 3;    # 7
my $prod = 10 * 3;    # 30
my $quot = 10 / 3;    # 3.33333...
my $mod = 10 % 3;     # 1
my $pow = 2 ** 8;     # 256
\`\`\`

### Compound Assignment

\`\`\`perl
my $x = 10;
$x += 5;   # $x is now 15
$x -= 3;   # $x is now 12
$x *= 2;   # $x is now 24
\`\`\`

### Your Task

Calculate \`2 ** 10\` (two to the power of ten) and print the result.`,

  starterCode: `my $result = 2 ** 10;
say $result;
`,

  solution: `my $result = 2 ** 10;
say $result;
`,

  tests: [
    {
      name: "prints 1024",
      expected: "1024\n",
    },
    {
      name: "modulo operator",
      expected: "1\n",
      code: `my $x = 10 % 3;
say $x;`,
    },
    {
      name: "compound assignment",
      expected: "25\n",
      code: `my $x = 10;
$x += 15;
say $x;`,
    },
  ],
};
