import type { Lesson } from "../../types";

export const subroutines: Lesson = {
  id: "subroutines",
  title: "Subroutines",
  chapterId: "functions",
  content: `## Subroutines

Functions in Perl are called subroutines. Define them with \`sub\`:

\`\`\`perl
sub greet {
    my ($name) = @_;
    say "Hello, $name!";
}

greet("Alice");   # Hello, Alice!
\`\`\`

The special array \`@_\` contains the arguments passed to the subroutine.

### Returning Values

The last expression evaluated is the return value, or use \`return\` explicitly:

\`\`\`perl
sub add {
    my ($a, $b) = @_;
    return $a + $b;
}

say add(3, 4);   # 7
\`\`\`

### Multiple Parameters

\`\`\`perl
sub max {
    my ($a, $b) = @_;
    if ($a > $b) {
        return $a;
    }
    return $b;
}
\`\`\`

### Your Task

Write a subroutine \`square\` that takes a number and returns its square. Print the square of 7.`,

  starterCode: `sub square {
    my ($n) = @_;
    return $n * $n;
}

say square(7);
`,

  solution: `sub square {
    my ($n) = @_;
    return $n * $n;
}

say square(7);
`,

  tests: [
    {
      name: "square of 7 is 49",
      expected: "49\n",
    },
    {
      name: "square of 3 is 9",
      expected: "9\n",
      code: `{{FUNC}}
say square(3);`,
    },
    {
      name: "square of 0 is 0",
      expected: "0\n",
      code: `{{FUNC}}
say square(0);`,
    },
    {
      name: "square of 10 is 100",
      expected: "100\n",
      code: `{{FUNC}}
say square(10);`,
    },
  ],
};
