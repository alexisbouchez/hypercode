import type { Lesson } from "../../types";

export const stringFunctions: Lesson = {
  id: "string-functions",
  title: "String Functions",
  chapterId: "functions",
  content: `## String Functions

PHP has an extensive string function library:

\`\`\`php
echo str_repeat("ab", 3) . "\\n";       // ababab
echo str_pad("5", 3, "0", STR_PAD_LEFT) . "\\n"; // 005
echo trim("  hello  ") . "\\n";          // hello
echo strrev("hello") . "\\n";            // olleh
echo str_contains("hello world", "world"); // 1
echo strpos("hello", "ll") . "\\n";      // 2
\`\`\`

### Your Task

Write a function \`is_palindrome\` that checks if a string reads the same forward and backward (case-insensitive).`,

  starterCode: `<?php
function is_palindrome($str) {
    // Your code here
}

echo is_palindrome("racecar") ? "true" : "false";
echo "\\n";
echo is_palindrome("Hello") ? "true" : "false";
echo "\\n";
echo is_palindrome("Madam") ? "true" : "false";
echo "\\n";
`,

  solution: `<?php
function is_palindrome($str) {
    $lower = strtolower($str);
    return $lower === strrev($lower);
}

echo is_palindrome("racecar") ? "true" : "false";
echo "\\n";
echo is_palindrome("Hello") ? "true" : "false";
echo "\\n";
echo is_palindrome("Madam") ? "true" : "false";
echo "\\n";
`,

  tests: [
    {
      name: "checks palindromes correctly",
      expected: "true\nfalse\ntrue\n",
    },
  ],
};
