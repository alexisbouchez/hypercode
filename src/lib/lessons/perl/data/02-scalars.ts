import type { Lesson } from "../../types";

export const scalars: Lesson = {
  id: "scalars",
  title: "Scalar Variables",
  chapterId: "basics",
  content: `## Scalar Variables

In Perl, scalar variables hold a single value -- a number, a string, or a reference. They are prefixed with \`$\`:

\`\`\`perl
my $name = "Alice";
my $age = 30;
my $pi = 3.14159;
\`\`\`

The \`my\` keyword declares a new variable with lexical scope.

### Numbers

Perl handles integers and floating-point numbers seamlessly:

\`\`\`perl
my $x = 10;
my $y = 3.5;
my $z = $x + $y;   # 13.5
\`\`\`

### Strings

Strings can be single-quoted (literal) or double-quoted (with interpolation):

\`\`\`perl
my $greeting = 'Hello';        # no interpolation
my $message = "Hi, $name!";   # interpolates $name
\`\`\`

### Your Task

Declare a variable \`$language\` set to \`"Perl"\` and print \`I am learning Perl\` using string interpolation.`,

  starterCode: `my $language = "Perl";
say "I am learning $language";
`,

  solution: `my $language = "Perl";
say "I am learning $language";
`,

  tests: [
    {
      name: "prints I am learning Perl",
      expected: "I am learning Perl\n",
    },
    {
      name: "scalar integer",
      expected: "42\n",
      code: `my $x = 42;
say $x;`,
    },
    {
      name: "scalar float",
      expected: "3.14\n",
      code: `my $pi = 3.14;
say $pi;`,
    },
  ],
};
