import type { Lesson } from "../../types";

export const typeJuggling: Lesson = {
  id: "type-juggling",
  title: "Type Juggling",
  chapterId: "advanced",
  content: `## Type Juggling

PHP automatically converts types when needed:

\`\`\`php
echo "5" + 3 . "\\n";     // 8 (string "5" becomes int)
echo "5" . 3 . "\\n";     // 53 (int 3 becomes string)
echo true + true . "\\n"; // 2 (booleans become ints)
\`\`\`

### Type Checking

\`\`\`php
gettype(42);        // "integer"
gettype("hello");   // "string"
gettype(3.14);      // "double"
gettype(true);      // "boolean"
gettype([1, 2]);    // "array"

is_int(42);         // true
is_string("hi");    // true
is_array([]);       // true
\`\`\`

### Type Casting

\`\`\`php
$x = (int)"42";     // 42
$y = (string)42;    // "42"
$z = (float)"3.14"; // 3.14
$b = (bool)1;       // true
\`\`\`

### Your Task

Print the type and value for each:
1. \`"42" + 8\`
2. \`"hello" . 5\`
3. The type of \`[1, 2, 3]\``,

  starterCode: `<?php
echo "42" + 8 . "\\n";
echo "hello" . 5 . "\\n";
echo gettype([1, 2, 3]) . "\\n";
`,

  solution: `<?php
echo "42" + 8 . "\\n";
echo "hello" . 5 . "\\n";
echo gettype([1, 2, 3]) . "\\n";
`,

  tests: [
    {
      name: "prints type juggling results",
      expected: "50\nhello5\narray\n",
    },
  ],
};
