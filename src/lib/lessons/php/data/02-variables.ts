import type { Lesson } from "../../types";

export const variables: Lesson = {
  id: "variables",
  title: "Variables",
  chapterId: "basics",
  content: `## Variables

In PHP, all variables start with a \`$\` sign:

\`\`\`php
$name = "Alice";
$age = 30;
$pi = 3.14;
$active = true;
\`\`\`

PHP is dynamically typed — you don't declare types.

### String Interpolation

Double-quoted strings interpolate variables:

\`\`\`php
$name = "Alice";
echo "Hello, $name\\n"; // Hello, Alice
\`\`\`

Single-quoted strings do **not** interpolate:

\`\`\`php
echo 'Hello, $name\\n'; // Hello, $name\\n
\`\`\`

### Your Task

Create a variable \`$language\` set to \`"PHP"\` and print \`I love PHP\` using string interpolation.`,

  starterCode: `<?php
$language = "PHP";
echo "I love $language\\n";
`,

  solution: `<?php
$language = "PHP";
echo "I love $language\\n";
`,

  tests: [
    {
      name: "prints I love PHP",
      expected: "I love PHP\n",
    },
  ],
};
