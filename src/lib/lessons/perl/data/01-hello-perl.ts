import type { Lesson } from "../../types";

export const helloPerl: Lesson = {
  id: "hello-perl",
  title: "Hello, Perl!",
  chapterId: "basics",
  content: `## Your First Perl Program

In Perl, \`print\` outputs text to the screen. You need to add \`\\n\` for a newline:

\`\`\`perl
print "Hello, World!\\n";
\`\`\`

The \`say\` function works like \`print\` but automatically adds a newline:

\`\`\`perl
say "Hello, World!";
\`\`\`

Every statement in Perl ends with a semicolon \`;\`.

### Comments

\`\`\`perl
# This is a comment
\`\`\`

### Your Task

Print exactly \`Hello, Perl!\` using \`say\`.`,

  starterCode: `say "Hello, Perl!";
`,

  solution: `say "Hello, Perl!";
`,

  tests: [
    {
      name: "prints Hello, Perl!",
      expected: "Hello, Perl!\n",
    },
    {
      name: "prints greeting with print",
      expected: "Hello, World!\n",
      code: `{{FUNC}}
print "Hello, World!\\n";`,
    },
    {
      name: "prints a number",
      expected: "42\n",
      code: `{{FUNC}}
say 42;`,
    },
  ],
};
