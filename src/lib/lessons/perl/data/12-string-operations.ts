import type { Lesson } from "../../types";

export const stringOperations: Lesson = {
  id: "string-operations",
  title: "String Operations",
  chapterId: "functions",
  content: `## String Functions

Perl has many built-in functions for manipulating strings.

### Case Conversion

\`\`\`perl
say uc("hello");    # HELLO
say lc("HELLO");    # hello
\`\`\`

### Substring

\`substr\` extracts part of a string:

\`\`\`perl
my $str = "Hello, World!";
say substr($str, 0, 5);    # Hello
say substr($str, 7);       # World!
\`\`\`

### String Length

\`\`\`perl
say length("Perl");   # 4
\`\`\`

### Finding Substrings

\`index\` returns the position of a substring (-1 if not found):

\`\`\`perl
my $pos = index("Hello, World!", "World");
say $pos;   # 7
\`\`\`

### Sprintf

\`sprintf\` formats strings (like C's sprintf):

\`\`\`perl
my $msg = sprintf("Name: %s, Age: %d", "Alice", 30);
say $msg;
\`\`\`

### Your Task

Write a subroutine \`shout\` that takes a string and returns it in uppercase. Print the result of calling \`shout\` with \`"hello"\`.`,

  starterCode: `sub shout {
    my ($str) = @_;
    return uc($str);
}

say shout("hello");
`,

  solution: `sub shout {
    my ($str) = @_;
    return uc($str);
}

say shout("hello");
`,

  tests: [
    {
      name: "shout hello",
      expected: "HELLO\n",
    },
    {
      name: "shout world",
      expected: "WORLD\n",
      code: `{{FUNC}}
say shout("world");`,
    },
    {
      name: "substr extraction",
      expected: "Hello\n",
      code: `my $str = "Hello, World!";
say substr($str, 0, 5);`,
    },
    {
      name: "index finds position",
      expected: "7\n",
      code: `say index("Hello, World!", "World");`,
    },
  ],
};
