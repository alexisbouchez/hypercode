import type { Lesson } from "../../types";

export const strings: Lesson = {
  id: "strings",
  title: "Strings",
  chapterId: "basics",
  content: `## String Operations

PHP has powerful string functions:

\`\`\`php
$str = "Hello, World!";
echo strlen($str) . "\\n";        // 13
echo strtoupper($str) . "\\n";    // HELLO, WORLD!
echo strtolower($str) . "\\n";    // hello, world!
echo substr($str, 0, 5) . "\\n";  // Hello
echo str_replace("World", "PHP", $str) . "\\n"; // Hello, PHP!
\`\`\`

### Concatenation

Use \`.\` to concatenate strings:

\`\`\`php
$first = "Hello";
$second = "World";
echo $first . ", " . $second . "!\\n";
\`\`\`

### Your Task

Given \`$word = "hypercode"\`, print:
- Its length
- The word in uppercase
- The first 5 characters`,

  starterCode: `<?php
$word = "hypercode";
echo strlen($word) . "\\n";
echo strtoupper($word) . "\\n";
echo substr($word, 0, 5) . "\\n";
`,

  solution: `<?php
$word = "hypercode";
echo strlen($word) . "\\n";
echo strtoupper($word) . "\\n";
echo substr($word, 0, 5) . "\\n";
`,

  tests: [
    {
      name: "prints 9, HYPERCODE, hyper",
      expected: "9\nHYPERCODE\nhyper\n",
    },
  ],
};
