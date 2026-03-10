import type { Lesson } from "../../types";

export const whileUntil: Lesson = {
  id: "while-until",
  title: "While / Until",
  chapterId: "control-flow",
  content: `## While Loops

A \`while\` loop runs as long as its condition is true:

\`\`\`perl
my $i = 1;
while ($i <= 5) {
    say $i;
    $i++;
}
\`\`\`

### Until Loops

\`until\` is the opposite of \`while\` -- it runs until the condition becomes true:

\`\`\`perl
my $count = 0;
until ($count == 3) {
    say $count;
    $count++;
}
\`\`\`

### Increment / Decrement

\`\`\`perl
$x++;   # increment by 1
$x--;   # decrement by 1
$x += 5;  # add 5
\`\`\`

### Your Task

Print the numbers 1 through 5, each on its own line, using a \`while\` loop.`,

  starterCode: `my $i = 1;
while ($i <= 5) {
    say $i;
    $i++;
}
`,

  solution: `my $i = 1;
while ($i <= 5) {
    say $i;
    $i++;
}
`,

  tests: [
    {
      name: "prints 1 through 5",
      expected: "1\n2\n3\n4\n5\n",
    },
    {
      name: "until loop",
      expected: "0\n1\n2\n",
      code: `my $count = 0;
until ($count == 3) {
    say $count;
    $count++;
}`,
    },
    {
      name: "decrement loop",
      expected: "3\n2\n1\n",
      code: `my $i = 3;
while ($i > 0) {
    say $i;
    $i--;
}`,
    },
  ],
};
