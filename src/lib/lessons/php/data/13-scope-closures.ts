import type { Lesson } from "../../types";

export const scopeClosures: Lesson = {
  id: "scope-closures",
  title: "Scope & Closures",
  chapterId: "functions",
  content: `## Variable Scope

Variables inside functions are local by default:

\`\`\`php
$x = 10;
function test() {
    // $x is not accessible here
    $y = 20;
    echo $y . "\\n";
}
\`\`\`

### Closures

Closures capture variables with \`use\`:

\`\`\`php
$greeting = "Hello";
$greet = function($name) use ($greeting) {
    return "$greeting, $name!";
};
echo $greet("Alice") . "\\n"; // Hello, Alice!
\`\`\`

### Your Task

Write a function \`make_counter\` that returns a closure. Each call to the closure should return the next number starting from 1.`,

  starterCode: `<?php
function make_counter() {
    $count = 0;
    return function() use (&$count) {
        $count++;
        return $count;
    };
}

$counter = make_counter();
echo $counter() . "\\n";
echo $counter() . "\\n";
echo $counter() . "\\n";
`,

  solution: `<?php
function make_counter() {
    $count = 0;
    return function() use (&$count) {
        $count++;
        return $count;
    };
}

$counter = make_counter();
echo $counter() . "\\n";
echo $counter() . "\\n";
echo $counter() . "\\n";
`,

  tests: [
    {
      name: "counter increments correctly",
      expected: "1\n2\n3\n",
    },
  ],
};
