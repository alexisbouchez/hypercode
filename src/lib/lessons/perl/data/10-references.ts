import type { Lesson } from "../../types";

export const references: Lesson = {
  id: "references",
  title: "Array & Hash Operations",
  chapterId: "data",
  content: `## Working with Arrays and Hashes

### Joining Arrays

\`join\` creates a string from an array:

\`\`\`perl
my @words = ("hello", "world");
say join(" ", @words);   # hello world
say join(", ", @words);  # hello, world
\`\`\`

### Splitting Strings

\`split\` creates an array from a string:

\`\`\`perl
my @parts = split(",", "a,b,c");
say $parts[0];   # a
say $parts[1];   # b
\`\`\`

### Sorting and Reversing

\`\`\`perl
my @nums = (3, 1, 4, 1, 5);
my @sorted = sort @nums;
my @rev = reverse @nums;
\`\`\`

### Keys and Values

Extract keys or values from a hash:

\`\`\`perl
my %h = (a => 1, b => 2);
my @k = keys %h;
my @v = values %h;
\`\`\`

### Your Task

Split the string \`"one,two,three"\` by comma and print each word on its own line.`,

  starterCode: `my @words = split(",", "one,two,three");
foreach my $w (@words) {
    say $w;
}
`,

  solution: `my @words = split(",", "one,two,three");
foreach my $w (@words) {
    say $w;
}
`,

  tests: [
    {
      name: "splits and prints words",
      expected: "one\ntwo\nthree\n",
    },
    {
      name: "join with space",
      expected: "hello world\n",
      code: `my @words = ("hello", "world");
say join(" ", @words);`,
    },
    {
      name: "join with comma",
      expected: "a, b, c\n",
      code: `my @words = ("a", "b", "c");
say join(", ", @words);`,
    },
  ],
};
