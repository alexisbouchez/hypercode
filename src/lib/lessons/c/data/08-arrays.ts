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

> Photon torpedo banks: indexed, loaded, and ready to launch in order. Torpedo \`tubes[0]\` through \`tubes[n-1]\`.

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

Write a function \`int sum_array(int *arr, int len)\` that returns the sum of all elements in the array. Use it to print the sum of \`{2, 4, 6, 8, 10}\`.`,

	starterCode: `#include <stdio.h>

int sum_array(int *arr, int len) {
\t// Sum all elements
\treturn 0;
}

int main() {
\tint nums[] = {2, 4, 6, 8, 10};
\tprintf("%d\\n", sum_array(nums, 5));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int sum_array(int *arr, int len) {
\tint sum = 0;
\tfor (int i = 0; i < len; i++) {
\t\tsum = sum + arr[i];
\t}
\treturn sum;
}

int main() {
\tint nums[] = {2, 4, 6, 8, 10};
\tprintf("%d\\n", sum_array(nums, 5));
\treturn 0;
}
`,

	tests: [
		{
			name: "sum of {2,4,6,8,10} = 30",
			expected: "30\n",
		},
		{
			name: "sum of {1} = 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint arr[] = {1};
\tprintf("%d\\n", sum_array(arr, 1));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "sum of {10,20,30} = 60",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint arr[] = {10, 20, 30};
\tprintf("%d\\n", sum_array(arr, 3));
\treturn 0;
}`,
			expected: "60\n",
		},
		{
			name: "sum of {-1,1} = 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint arr[] = {-1, 1};
\tprintf("%d\\n", sum_array(arr, 2));
\treturn 0;
}`,
			expected: "0\n",
		},
	],
};
