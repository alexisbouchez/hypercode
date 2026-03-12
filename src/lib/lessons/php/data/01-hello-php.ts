import type { Lesson } from "../../types";

export const helloPhp: Lesson = {
  id: "hello-php",
  title: "Hello, PHP!",
  chapterId: "basics",
  content: `## Your First PHP Program

PHP uses \`echo\` to output text:

\`\`\`php
<?php
echo "Hello, World!\\n";
\`\`\`

Every PHP statement ends with a semicolon \`;\`.

### Comments

\`\`\`php
// Single-line comment
# Also a single-line comment
/* Multi-line comment */
\`\`\`

### Your Task

Print exactly \`Hello, PHP!\` using \`echo\`.`,

  starterCode: `<?php
echo "Hello, PHP!\\n";
`,

  solution: `<?php
echo "Hello, PHP!\\n";
`,

  tests: [
    {
      name: "prints Hello, PHP!",
      expected: "Hello, PHP!\n",
    },
  ],
};
