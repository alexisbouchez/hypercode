import type { Lesson } from "../../types";

export const arrays: Lesson = {
	id: "arrays",
	title: "Arrays",
	chapterId: "arrays-and-strings",
	content: `## Arrays

An array is a fixed-size collection of elements of the same type, stored contiguously in memory.

### Declaration and Initialization

\`\`\`c
int nums[5] = {10, 20, 30, 40, 50};
int zeros[100] = {0};  // all elements initialized to 0
\`\`\`

### Accessing Elements

Array indices start at 0:

\`\`\`c
int first = nums[0];   // 10
int third = nums[2];   // 30
nums[4] = 99;           // modify last element
\`\`\`

### Iterating

\`\`\`c
int nums[5] = {10, 20, 30, 40, 50};
for (int i = 0; i < 5; i++) {
    printf("%d\\n", nums[i]);
}
\`\`\`

### sizeof

\`sizeof\` returns the total size in bytes. To get the number of elements:

\`\`\`c
int nums[5] = {10, 20, 30, 40, 50};
int count = sizeof(nums) / sizeof(nums[0]);  // 5
\`\`\`

### No Bounds Checking

C does **not** check array bounds. Accessing \`nums[10]\` when the array has 5 elements is undefined behavior -- it will read whatever memory happens to be there.

### Your Task

Create an array with the values \`{2, 4, 6, 8, 10}\`. Compute and print the sum of all elements.`,

	starterCode: `#include <stdio.h>

int main() {
\tint nums[5] = {2, 4, 6, 8, 10};
\t// Compute the sum and print it
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int main() {
\tint nums[5] = {2, 4, 6, 8, 10};
\tint sum = 0;
\tfor (int i = 0; i < 5; i++) {
\t\tsum = sum + nums[i];
\t}
\tprintf("%d\\n", sum);
\treturn 0;
}
`,

	tests: [
		{
			name: "prints sum of array",
			expected: "30\n",
		},
	],
};
