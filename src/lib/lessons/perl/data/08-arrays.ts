import type { Lesson } from "../../types";

export const arrays: Lesson = {
  id: "arrays",
  title: "Arrays",
  chapterId: "data",
  content: `## Arrays

Arrays in Perl are ordered lists of scalars, prefixed with \`@\`:

\`\`\`perl
my @fruits = ("apple", "banana", "cherry");
\`\`\`

### Accessing Elements

Use \`$\` (scalar) to access a single element by index (0-based):

\`\`\`perl
say $fruits[0];   # apple
say $fruits[1];   # banana
say $fruits[-1];  # cherry (last element)
\`\`\`

### Array Length

\`\`\`perl
my $len = scalar(@fruits);   # 3
\`\`\`

### Modifying Arrays

\`\`\`perl
push @fruits, "date";       # add to end
my $last = pop @fruits;     # remove from end
unshift @fruits, "avocado"; # add to front
my $first = shift @fruits;  # remove from front
\`\`\`

### Your Task

Create an array of three numbers, push a fourth, then print each element on its own line using a foreach loop.`,

  starterCode: `my @nums = (10, 20, 30);
push @nums, 40;
foreach my $n (@nums) {
    say $n;
}
`,

  solution: `my @nums = (10, 20, 30);
push @nums, 40;
foreach my $n (@nums) {
    say $n;
}
`,

  tests: [
    {
      name: "prints four numbers",
      expected: "10\n20\n30\n40\n",
    },
    {
      name: "array access by index",
      expected: "banana\n",
      code: `my @fruits = ("apple", "banana", "cherry");
say $fruits[1];`,
    },
    {
      name: "scalar gives length",
      expected: "3\n",
      code: `my @arr = (1, 2, 3);
say scalar(@arr);`,
    },
    {
      name: "pop removes last",
      expected: "3\n1\n2\n",
      code: `my @arr = (1, 2, 3);
my $last = pop @arr;
say $last;
foreach my $n (@arr) {
    say $n;
}`,
    },
  ],
};
