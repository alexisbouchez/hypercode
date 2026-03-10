import type { Lesson } from "../../types";

export const fileSimulation: Lesson = {
  id: "file-simulation",
  title: "Data Processing",
  chapterId: "advanced",
  content: `## Processing Structured Data

In real Perl programs, you often read data from files and process it line by line. We will simulate this by working with multi-line strings.

### Processing CSV Data

\`\`\`perl
my @lines = ("Alice,30", "Bob,25", "Carol,35");
foreach my $line (@lines) {
    my @fields = split(",", $line);
    say "$fields[0] is $fields[1] years old";
}
\`\`\`

### Building a Report

Combine arrays and hashes to transform data:

\`\`\`perl
my @data = ("apple:3", "banana:5", "cherry:2");
my $total = 0;
foreach my $item (@data) {
    my @parts = split(":", $item);
    $total += $parts[1];
}
say "Total: $total";
\`\`\`

### Accumulating Results

\`\`\`perl
sub sum_array {
    my @nums = @_;
    my $total = 0;
    foreach my $n (@nums) {
        $total += $n;
    }
    return $total;
}
\`\`\`

### Your Task

Process an array of \`"name:score"\` strings. Print each person's name and score, then print the average score.`,

  starterCode: `my @data = ("Alice:90", "Bob:80", "Carol:70");
my $total = 0;
my $count = 0;
foreach my $entry (@data) {
    my @parts = split(":", $entry);
    say "$parts[0] scored $parts[1]";
    $total += $parts[1];
    $count++;
}
my $avg = $total / $count;
say "Average: $avg";
`,

  solution: `my @data = ("Alice:90", "Bob:80", "Carol:70");
my $total = 0;
my $count = 0;
foreach my $entry (@data) {
    my @parts = split(":", $entry);
    say "$parts[0] scored $parts[1]";
    $total += $parts[1];
    $count++;
}
my $avg = $total / $count;
say "Average: $avg";
`,

  tests: [
    {
      name: "prints scores and average",
      expected: "Alice scored 90\nBob scored 80\nCarol scored 70\nAverage: 80\n",
    },
    {
      name: "CSV split",
      expected: "Alice is 30 years old\nBob is 25 years old\n",
      code: `my @lines = ("Alice,30", "Bob,25");
foreach my $line (@lines) {
    my @fields = split(",", $line);
    say "$fields[0] is $fields[1] years old";
}`,
    },
    {
      name: "sum accumulation",
      expected: "Total: 10\n",
      code: `my @data = ("a:3", "b:5", "c:2");
my $total = 0;
foreach my $item (@data) {
    my @parts = split(":", $item);
    $total += $parts[1];
}
say "Total: $total";`,
    },
  ],
};
