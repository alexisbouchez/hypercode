import type { Lesson } from "../../types";

export const forForeach: Lesson = {
  id: "for-foreach",
  title: "For / Foreach",
  chapterId: "control-flow",
  content: `## For Loops

Perl's \`for\` loop can iterate over a range:

\`\`\`perl
for my $i (1..5) {
    say $i;
}
\`\`\`

The \`1..5\` range operator produces the list \`(1, 2, 3, 4, 5)\`.

### Foreach

\`foreach\` iterates over a list or array:

\`\`\`perl
my @colors = ("red", "green", "blue");
foreach my $color (@colors) {
    say $color;
}
\`\`\`

In Perl, \`for\` and \`foreach\` are interchangeable:

\`\`\`perl
for my $color (@colors) {
    say $color;
}
\`\`\`

### Your Task

Print the squares of numbers 1 through 5 (1, 4, 9, 16, 25), each on its own line.`,

  starterCode: `for my $i (1..5) {
    my $square = $i * $i;
    say $square;
}
`,

  solution: `for my $i (1..5) {
    my $square = $i * $i;
    say $square;
}
`,

  tests: [
    {
      name: "prints squares 1-5",
      expected: "1\n4\n9\n16\n25\n",
    },
    {
      name: "foreach over array",
      expected: "red\ngreen\nblue\n",
      code: `my @colors = ("red", "green", "blue");
foreach my $color (@colors) {
    say $color;
}`,
    },
    {
      name: "range iteration",
      expected: "3\n4\n5\n6\n7\n",
      code: `for my $i (3..7) {
    say $i;
}`,
    },
  ],
};
