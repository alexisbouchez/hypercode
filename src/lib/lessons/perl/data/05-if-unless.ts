import type { Lesson } from "../../types";

export const ifUnless: Lesson = {
  id: "if-unless",
  title: "If / Unless",
  chapterId: "control-flow",
  content: `## Conditional Statements

### if / elsif / else

\`\`\`perl
my $x = 10;
if ($x > 0) {
    say "positive";
} elsif ($x == 0) {
    say "zero";
} else {
    say "negative";
}
\`\`\`

### Comparison Operators

Perl has separate operators for numbers and strings:

| Numeric | String | Meaning |
|---------|--------|---------|
| \`==\` | \`eq\` | Equal |
| \`!=\` | \`ne\` | Not equal |
| \`<\` | \`lt\` | Less than |
| \`>\` | \`gt\` | Greater than |
| \`<=\` | \`le\` | Less or equal |
| \`>=\` | \`ge\` | Greater or equal |

### unless

\`unless\` is the opposite of \`if\` -- it executes when the condition is false:

\`\`\`perl
my $age = 20;
unless ($age < 18) {
    say "adult";
}
\`\`\`

### Your Task

Write an if/elsif/else that classifies a number: print \`positive\`, \`zero\`, or \`negative\`.`,

  starterCode: `my $x = 42;
if ($x > 0) {
    say "positive";
} elsif ($x == 0) {
    say "zero";
} else {
    say "negative";
}
`,

  solution: `my $x = 42;
if ($x > 0) {
    say "positive";
} elsif ($x == 0) {
    say "zero";
} else {
    say "negative";
}
`,

  tests: [
    {
      name: "positive number",
      expected: "positive\n",
    },
    {
      name: "zero",
      expected: "zero\n",
      code: `my $x = 0;
if ($x > 0) {
    say "positive";
} elsif ($x == 0) {
    say "zero";
} else {
    say "negative";
}`,
    },
    {
      name: "negative number",
      expected: "negative\n",
      code: `my $x = -5;
if ($x > 0) {
    say "positive";
} elsif ($x == 0) {
    say "zero";
} else {
    say "negative";
}`,
    },
    {
      name: "unless works",
      expected: "adult\n",
      code: `my $age = 20;
unless ($age < 18) {
    say "adult";
}`,
    },
  ],
};
