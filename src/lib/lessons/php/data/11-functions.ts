import type { Lesson } from "../../types";

export const functions: Lesson = {
  id: "functions",
  title: "Functions",
  chapterId: "functions",
  content: `## Functions

Define functions with the \`function\` keyword:

\`\`\`php
function greet($name) {
    return "Hello, $name!";
}
echo greet("Alice") . "\\n";
\`\`\`

### Default Parameters

\`\`\`php
function greet($name = "World") {
    return "Hello, $name!";
}
echo greet() . "\\n"; // Hello, World!
\`\`\`

### Arrow Functions

Short closures for simple expressions:

\`\`\`php
$double = fn($x) => $x * 2;
echo $double(5) . "\\n"; // 10
\`\`\`

### Your Task

Write a function \`max_of_three\` that takes three numbers and returns the largest.`,

  starterCode: `<?php
function max_of_three($a, $b, $c) {
    // Your code here
}

echo max_of_three(3, 7, 5) . "\\n";
echo max_of_three(10, 2, 8) . "\\n";
echo max_of_three(1, 1, 1) . "\\n";
`,

  solution: `<?php
function max_of_three($a, $b, $c) {
    if ($a >= $b && $a >= $c) {
        return $a;
    } elseif ($b >= $a && $b >= $c) {
        return $b;
    } else {
        return $c;
    }
}

echo max_of_three(3, 7, 5) . "\\n";
echo max_of_three(10, 2, 8) . "\\n";
echo max_of_three(1, 1, 1) . "\\n";
`,

  tests: [
    {
      name: "returns max of three numbers",
      expected: "7\n10\n1\n",
    },
  ],
};
