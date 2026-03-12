import type { Lesson } from "../../types";

export const arrays: Lesson = {
  id: "arrays",
  title: "Arrays",
  chapterId: "data-structures",
  content: `## Arrays

PHP arrays are ordered maps:

\`\`\`php
$fruits = ["apple", "banana", "cherry"];

echo $fruits[0] . "\\n"; // apple
echo count($fruits) . "\\n"; // 3
\`\`\`

### Modifying Arrays

\`\`\`php
$fruits[] = "date";          // append
array_push($fruits, "fig");  // also append
array_pop($fruits);          // remove last
\`\`\`

### Useful Functions

\`\`\`php
in_array("banana", $fruits); // true
array_reverse($fruits);      // reversed copy
sort($fruits);               // sort in place
implode(", ", $fruits);      // join into string
\`\`\`

### Your Task

Create an array with \`[3, 1, 4, 1, 5]\`, sort it, then print each element on its own line.`,

  starterCode: `<?php
$nums = [3, 1, 4, 1, 5];
sort($nums);
foreach ($nums as $n) {
    echo "$n\\n";
}
`,

  solution: `<?php
$nums = [3, 1, 4, 1, 5];
sort($nums);
foreach ($nums as $n) {
    echo "$n\\n";
}
`,

  tests: [
    {
      name: "prints sorted array",
      expected: "1\n1\n3\n4\n5\n",
    },
  ],
};
