import type { Lesson } from "../../types";

export const puttingItTogether: Lesson = {
  id: "putting-it-together",
  title: "Putting It All Together",
  chapterId: "advanced",
  content: `## Putting It All Together

Let's combine everything you have learned: scalars, arrays, hashes, loops, subroutines, and string operations.

### Example: Word Frequency Counter

\`\`\`perl
sub count_words {
    my ($text) = @_;
    my @words = split(" ", $text);
    my %freq;
    foreach my $w (@words) {
        my $lower = lc($w);
        if (exists $freq{$lower}) {
            $freq{$lower} += 1;
        } else {
            $freq{$lower} = 1;
        }
    }
    return %freq;
}
\`\`\`

### Example: FizzBuzz

The classic interview problem -- combining loops, conditionals, and modulo:

\`\`\`perl
for my $i (1..15) {
    if ($i % 15 == 0) {
        say "FizzBuzz";
    } elsif ($i % 3 == 0) {
        say "Fizz";
    } elsif ($i % 5 == 0) {
        say "Buzz";
    } else {
        say $i;
    }
}
\`\`\`

### Your Task

Write a subroutine \`fizzbuzz\` that takes a number \`$n\` and prints FizzBuzz from 1 to \`$n\`. Call it with 15.`,

  starterCode: `sub fizzbuzz {
    my ($n) = @_;
    for my $i (1..$n) {
        if ($i % 15 == 0) {
            say "FizzBuzz";
        } elsif ($i % 3 == 0) {
            say "Fizz";
        } elsif ($i % 5 == 0) {
            say "Buzz";
        } else {
            say $i;
        }
    }
}

fizzbuzz(15);
`,

  solution: `sub fizzbuzz {
    my ($n) = @_;
    for my $i (1..$n) {
        if ($i % 15 == 0) {
            say "FizzBuzz";
        } elsif ($i % 3 == 0) {
            say "Fizz";
        } elsif ($i % 5 == 0) {
            say "Buzz";
        } else {
            say $i;
        }
    }
}

fizzbuzz(15);
`,

  tests: [
    {
      name: "fizzbuzz 15",
      expected: "1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz\n",
    },
    {
      name: "fizzbuzz 5",
      expected: "1\n2\nFizz\n4\nBuzz\n",
      code: `{{FUNC}}
fizzbuzz(5);`,
    },
    {
      name: "fizzbuzz 3",
      expected: "1\n2\nFizz\n",
      code: `{{FUNC}}
fizzbuzz(3);`,
    },
  ],
};
