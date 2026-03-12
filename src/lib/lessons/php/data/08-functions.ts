import type { Lesson } from "../../types";

export const functions: Lesson = {
  id: "functions",
  title: "Functions",
  chapterId: "functions",
  content: `## Functions

\`\`\`php
function greet($name) {
    return "Hello, $name!";
}

echo greet("Alice") . "\\n"; // Hello, Alice!
\`\`\`

### Default Parameters

\`\`\`php
function greet($name = "World") {
    return "Hello, $name!";
}

echo greet() . "\\n";       // Hello, World!
echo greet("Bob") . "\\n";  // Hello, Bob!
\`\`\`

### Your Task

Write a function \`add($a, $b)\` that returns the sum of two numbers. Print the result of \`add(3, 4)\`.`,

  starterCode: `<?php
function add($a, $b) {
    return $a + $b;
}

echo add(3, 4) . "\\n";
`,

  solution: `<?php
function add($a, $b) {
    return $a + $b;
}

echo add(3, 4) . "\\n";
`,

  tests: [
    {
      name: "add(3, 4) returns 7",
      expected: "7\n",
    },
    {
      name: "add(10, 20) returns 30",
      expected: "30\n",
      code: `<?php
{{FUNC}}
echo add(10, 20) . "\\n";`,
    },
  ],
};
