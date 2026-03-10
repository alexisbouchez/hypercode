import type { Lesson } from "../../types";

export const regularExpressions: Lesson = {
  id: "regular-expressions",
  title: "Regular Expressions",
  chapterId: "functions",
  content: `## Regular Expressions

Perl is famous for its powerful regular expression support. We will use \`split\` with regex patterns and build pattern-matching subroutines.

### Split with Regex

\`\`\`perl
my @words = split(/\\s+/, "hello   world   perl");
# ("hello", "world", "perl")
\`\`\`

### Building a Validator

You can write subroutines that check patterns manually:

\`\`\`perl
sub is_digit_string {
    my ($str) = @_;
    my @chars = split("", $str);
    foreach my $ch (@chars) {
        if ($ch lt "0" or $ch gt "9") {
            return 0;
        }
    }
    return 1;
}
\`\`\`

### Grep and Map

\`grep\` filters a list, \`map\` transforms it:

\`\`\`perl
my @nums = (1, 2, 3, 4, 5, 6);
my @evens = grep { $_ % 2 == 0 } @nums;   # (2, 4, 6)
my @doubled = map { $_ * 2 } @nums;         # (2, 4, 6, 8, 10, 12)
\`\`\`

### Your Task

Use \`grep\` to filter an array to only keep numbers greater than 3, then print each on its own line.`,

  starterCode: `my @nums = (1, 2, 3, 4, 5, 6);
my @big = grep { $_ > 3 } @nums;
foreach my $n (@big) {
    say $n;
}
`,

  solution: `my @nums = (1, 2, 3, 4, 5, 6);
my @big = grep { $_ > 3 } @nums;
foreach my $n (@big) {
    say $n;
}
`,

  tests: [
    {
      name: "filters numbers > 3",
      expected: "4\n5\n6\n",
    },
    {
      name: "map doubles values",
      expected: "2\n4\n6\n",
      code: `my @nums = (1, 2, 3);
my @doubled = map { $_ * 2 } @nums;
foreach my $n (@doubled) {
    say $n;
}`,
    },
    {
      name: "split on whitespace",
      expected: "hello\nworld\n",
      code: `my @words = split(" ", "hello world");
foreach my $w (@words) {
    say $w;
}`,
    },
  ],
};
