import type { Lesson } from "../../types";

export const closures: Lesson = {
  id: "closures",
  title: "Closures",
  chapterId: "functions",
  content: `## Anonymous Functions & Closures

PHP supports anonymous functions (closures):

\`\`\`php
$greet = function($name) {
    return "Hello, $name!";
};

echo $greet("Alice") . "\\n"; // Hello, Alice!
\`\`\`

### Capturing Variables with \`use\`

Closures can capture variables from the enclosing scope with \`use\`:

\`\`\`php
$prefix = "Mr.";
$greet = function($name) use ($prefix) {
    return "$prefix $name";
};

echo $greet("Smith") . "\\n"; // Mr. Smith
\`\`\`

### Arrow Functions (PHP 7.4+)

Short closures that automatically capture variables:

\`\`\`php
$multiplier = 3;
$multiply = fn($n) => $n * $multiplier;

echo $multiply(5) . "\\n"; // 15
\`\`\`

### Your Task

Write a function \`makeCounter()\` that returns a closure. Each time the closure is called, it should return the next integer starting from 1.`,

  starterCode: `<?php
function makeCounter() {
    $count = 0;
    return function() use (&$count) {
        $count++;
        return $count;
    };
}

$counter = makeCounter();
echo $counter() . "\\n";
echo $counter() . "\\n";
echo $counter() . "\\n";
`,

  solution: `<?php
function makeCounter() {
    $count = 0;
    return function() use (&$count) {
        $count++;
        return $count;
    };
}

$counter = makeCounter();
echo $counter() . "\\n";
echo $counter() . "\\n";
echo $counter() . "\\n";
`,

  tests: [
    {
      name: "counter increments",
      expected: "1\n2\n3\n",
    },
  ],
};
