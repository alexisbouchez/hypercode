import type { Lesson } from "../../types";

export const arrayFunctions: Lesson = {
  id: "array-functions",
  title: "Array Functions",
  chapterId: "data-structures",
  content: `## Array Functions

### array_map

Apply a function to each element:

\`\`\`php
$nums = [1, 2, 3, 4];
$doubled = array_map(function($n) { return $n * 2; }, $nums);
// [2, 4, 6, 8]
\`\`\`

### array_filter

Keep elements that pass a test:

\`\`\`php
$nums = [1, 2, 3, 4, 5, 6];
$evens = array_filter($nums, function($n) { return $n % 2 === 0; });
// [2, 4, 6]
\`\`\`

### array_reduce

Reduce an array to a single value:

\`\`\`php
$nums = [1, 2, 3, 4];
$sum = array_reduce($nums, function($carry, $n) { return $carry + $n; }, 0);
// 10
\`\`\`

### Your Task

Given \`[1, 2, 3, 4, 5]\`:
1. Use \`array_filter\` to keep only odd numbers
2. Use \`array_map\` to square them
3. Print each result on its own line`,

  starterCode: `<?php
$nums = [1, 2, 3, 4, 5];
$odds = array_filter($nums, function($n) { return $n % 2 !== 0; });
$squared = array_map(function($n) { return $n * $n; }, $odds);
foreach ($squared as $n) {
    echo "$n\\n";
}
`,

  solution: `<?php
$nums = [1, 2, 3, 4, 5];
$odds = array_filter($nums, function($n) { return $n % 2 !== 0; });
$squared = array_map(function($n) { return $n * $n; }, $odds);
foreach ($squared as $n) {
    echo "$n\\n";
}
`,

  tests: [
    {
      name: "prints squared odd numbers",
      expected: "1\n9\n25\n",
    },
  ],
};
