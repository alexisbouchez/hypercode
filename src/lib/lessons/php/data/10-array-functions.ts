import type { Lesson } from "../../types";

export const arrayFunctions: Lesson = {
  id: "array-functions",
  title: "Array Functions",
  chapterId: "data-structures",
  content: `## Array Functions

PHP has many built-in array functions:

\`\`\`php
$nums = [3, 1, 4, 1, 5, 9];

sort($nums);
echo implode(", ", $nums) . "\\n"; // 1, 1, 3, 4, 5, 9

$reversed = array_reverse($nums);
echo implode(", ", $reversed) . "\\n"; // 9, 5, 4, 3, 1, 1

$sliced = array_slice($nums, 0, 3);
echo implode(", ", $sliced) . "\\n"; // 1, 1, 3
\`\`\`

### Map, Filter, Reduce

\`\`\`php
$doubled = array_map(fn($x) => $x * 2, [1, 2, 3]);
$evens = array_filter([1, 2, 3, 4], fn($x) => $x % 2 === 0);
$sum = array_reduce([1, 2, 3], fn($carry, $x) => $carry + $x, 0);
\`\`\`

### Your Task

Given \`$nums = [5, 2, 8, 1, 9]\`:
- Sort and print the sorted array joined by spaces
- Print the sum using \`array_reduce\``,

  starterCode: `<?php
$nums = [5, 2, 8, 1, 9];
sort($nums);
echo implode(" ", $nums) . "\\n";
$sum = array_reduce($nums, fn($carry, $x) => $carry + $x, 0);
echo $sum . "\\n";
`,

  solution: `<?php
$nums = [5, 2, 8, 1, 9];
sort($nums);
echo implode(" ", $nums) . "\\n";
$sum = array_reduce($nums, fn($carry, $x) => $carry + $x, 0);
echo $sum . "\\n";
`,

  tests: [
    {
      name: "prints sorted and sum",
      expected: "1 2 5 8 9\n25\n",
    },
  ],
};
