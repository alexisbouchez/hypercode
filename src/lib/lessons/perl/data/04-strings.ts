import type { Lesson } from "../../types";

export const strings: Lesson = {
  id: "strings",
  title: "Strings",
  chapterId: "basics",
  content: `## Working with Strings

Perl has two kinds of string literals:

### Double-Quoted Strings

Double-quoted strings interpolate variables and escape sequences:

\`\`\`perl
my $name = "World";
say "Hello, $name!";     # Hello, World!
say "Tab:\\there";        # Tab:	here
say "Newline:\\n";        # prints a newline
\`\`\`

### Single-Quoted Strings

Single-quoted strings are literal -- no interpolation:

\`\`\`perl
say 'Hello, $name!';     # Hello, $name!  (literally)
\`\`\`

### String Concatenation

The dot \`.\` operator concatenates strings:

\`\`\`perl
my $full = "Hello" . ", " . "World!";
say $full;   # Hello, World!
\`\`\`

### String Repetition

The \`x\` operator repeats a string:

\`\`\`perl
say "ha" x 3;   # hahaha
\`\`\`

### String Functions

\`\`\`perl
say uc("hello");        # HELLO
say lc("HELLO");        # hello
say length("Perl");     # 4
\`\`\`

### Your Task

Create two variables \`$first\` and \`$last\`, concatenate them with a space, and print the full name.`,

  starterCode: `my $first = "Larry";
my $last = "Wall";
my $full = $first . " " . $last;
say $full;
`,

  solution: `my $first = "Larry";
my $last = "Wall";
my $full = $first . " " . $last;
say $full;
`,

  tests: [
    {
      name: "prints Larry Wall",
      expected: "Larry Wall\n",
    },
    {
      name: "string repetition",
      expected: "hahaha\n",
      code: `say "ha" x 3;`,
    },
    {
      name: "uc function",
      expected: "HELLO\n",
      code: `say uc("hello");`,
    },
    {
      name: "length function",
      expected: "4\n",
      code: `say length("Perl");`,
    },
  ],
};
