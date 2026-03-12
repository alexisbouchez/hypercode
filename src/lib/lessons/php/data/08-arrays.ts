import type { Lesson } from "../../types";

export const arrays: Lesson = {
  id: "arrays",
  title: "Arrays",
  chapterId: "data-structures",
  content: `## Indexed Arrays

Create arrays with \`[]\` or \`array()\`:

\`\`\`php
$colors = ["red", "green", "blue"];
echo $colors[0] . "\\n"; // red
echo count($colors) . "\\n"; // 3
\`\`\`

### Modifying Arrays

\`\`\`php
$colors[] = "yellow";        // append
array_push($colors, "pink"); // also append
array_pop($colors);          // remove last
\`\`\`

### Useful Functions

\`\`\`php
echo implode(", ", $colors) . "\\n"; // join elements
$parts = explode(",", "a,b,c");      // split string
echo in_array("red", $colors) . "\\n"; // check membership
\`\`\`

### Your Task

Create an array \`$nums\` with values \`[10, 20, 30]\`. Print:
- The first element
- The last element
- The count`,

  starterCode: `<?php
$nums = [10, 20, 30];
echo $nums[0] . "\\n";
echo $nums[count($nums) - 1] . "\\n";
echo count($nums) . "\\n";
`,

  solution: `<?php
$nums = [10, 20, 30];
echo $nums[0] . "\\n";
echo $nums[count($nums) - 1] . "\\n";
echo count($nums) . "\\n";
`,

  tests: [
    {
      name: "prints 10, 30, 3",
      expected: "10\n30\n3\n",
    },
  ],
};
