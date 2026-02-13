import type { Lesson } from "../../types";

export const callocAndRealloc: Lesson = {
	id: "calloc-and-realloc",
	title: "Calloc and Realloc",
	chapterId: "dynamic-memory",
	content: `## Calloc and Realloc

Beyond \`malloc\`, C provides two more functions for managing heap memory.

### calloc

\`calloc(count, size)\` allocates memory for \`count\` elements of \`size\` bytes each, and **initializes all bytes to zero**:

\`\`\`c
// Allocate 10 ints, all initialized to 0
int *arr = (int *)calloc(10, sizeof(int));
printf("%d\\n", arr[0]);  // 0 (guaranteed)
free(arr);
\`\`\`

Compare with \`malloc\`, which leaves memory uninitialized:

| Function | Syntax | Zeroed? |
|----------|--------|---------|
| \`malloc\` | \`malloc(size)\` | No |
| \`calloc\` | \`calloc(count, size)\` | Yes |

> Expanding the Enterprise: \`realloc\` is like adding a new deck to the ship while everyone's still aboard.

### realloc

\`realloc(ptr, new_size)\` resizes a previously allocated block. It may move the data to a new location:

\`\`\`c
int *arr = (int *)malloc(3 * sizeof(int));
arr[0] = 10;
arr[1] = 20;
arr[2] = 30;

// Grow to 5 elements
arr = (int *)realloc(arr, 5 * sizeof(int));
arr[3] = 40;
arr[4] = 50;
free(arr);
\`\`\`

### Key Rules

- \`realloc\` preserves existing data up to the minimum of old and new sizes
- \`realloc(NULL, size)\` behaves like \`malloc(size)\`
- Always assign the result of \`realloc\` -- the pointer may change

### Your Task

Write a function \`int *zeros(int n)\` that uses \`calloc\` to allocate an array of \`n\` integers (all zero). Write a function \`int *grow(int *arr, int old_n, int new_n, int fill)\` that uses \`realloc\` to grow the array and fills new slots with \`fill\`. Print all elements after growing.`,

	starterCode: `#include <stdio.h>
#include <stdlib.h>

int *zeros(int n) {
\t// Use calloc to allocate n zero-initialized ints
\treturn NULL;
}

int *grow(int *arr, int old_n, int new_n, int fill) {
\t// Realloc to new_n, fill new slots with fill
\treturn NULL;
}

int main() {
\tint *arr = zeros(3);
\tarr = grow(arr, 3, 6, 7);
\tfor (int i = 0; i < 6; i++) {
\t\tprintf("%d\\n", arr[i]);
\t}
\tfree(arr);
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <stdlib.h>

int *zeros(int n) {
\treturn (int *)calloc(n, sizeof(int));
}

int *grow(int *arr, int old_n, int new_n, int fill) {
\tarr = (int *)realloc(arr, new_n * sizeof(int));
\tfor (int i = old_n; i < new_n; i++) {
\t\tarr[i] = fill;
\t}
\treturn arr;
}

int main() {
\tint *arr = zeros(3);
\tarr = grow(arr, 3, 6, 7);
\tfor (int i = 0; i < 6; i++) {
\t\tprintf("%d\\n", arr[i]);
\t}
\tfree(arr);
\treturn 0;
}
`,

	tests: [
		{
			name: "zeros then grow with 7s",
			expected: "0\n0\n0\n7\n7\n7\n",
		},
		{
			name: "zeros(4) all zero",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tint *arr = zeros(4);
\tfor (int i = 0; i < 4; i++) {
\t\tprintf("%d\\n", arr[i]);
\t}
\tfree(arr);
\treturn 0;
}`,
			expected: "0\n0\n0\n0\n",
		},
		{
			name: "grow fills with 5",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tint *arr = zeros(2);
\tarr = grow(arr, 2, 5, 5);
\tfor (int i = 0; i < 5; i++) {
\t\tprintf("%d\\n", arr[i]);
\t}
\tfree(arr);
\treturn 0;
}`,
			expected: "0\n0\n5\n5\n5\n",
		},
		{
			name: "grow preserves existing data",
			code: `#include <stdio.h>
#include <stdlib.h>
{{FUNC}}
int main() {
\tint *arr = (int *)malloc(2 * sizeof(int));
\tarr[0] = 10;
\tarr[1] = 20;
\tarr = grow(arr, 2, 4, 99);
\tfor (int i = 0; i < 4; i++) {
\t\tprintf("%d\\n", arr[i]);
\t}
\tfree(arr);
\treturn 0;
}`,
			expected: "10\n20\n99\n99\n",
		},
	],
};
