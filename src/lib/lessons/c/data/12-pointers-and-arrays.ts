import type { Lesson } from "../../types";

export const pointersAndArrays: Lesson = {
	id: "pointers-and-arrays",
	title: "Pointers and Arrays",
	chapterId: "pointers",
	content: `## Pointers and Arrays

In C, arrays and pointers are closely related. An array name, in most contexts, **decays** to a pointer to its first element.

### Array Decay

\`\`\`c
int arr[5] = {10, 20, 30, 40, 50};
int *p = arr;   // arr decays to &arr[0]
\`\`\`

Now \`p[i]\` and \`arr[i]\` access the same memory. You can use pointer arithmetic or array indexing interchangeably:

\`\`\`c
printf("%d\\n", p[2]);     // 30
printf("%d\\n", *(p + 2)); // 30 (same thing)
\`\`\`

> Tractor beam arrays: multiple beams working in concert, each pointing to a different target. That's pointer arithmetic in action.

### Arrays as Function Parameters

When you pass an array to a function, it decays to a pointer. The function receives a pointer, not a copy:

\`\`\`c
void print_array(int *arr, int len) {
    for (int i = 0; i < len; i++) {
        printf("%d ", arr[i]);
    }
    printf("\\n");
}

int main() {
    int nums[] = {1, 2, 3};
    print_array(nums, 3);
}
\`\`\`

### Modifying Arrays Through Pointers

Since the function gets a pointer to the original array, it can modify the array:

\`\`\`c
void double_elements(int *arr, int len) {
    for (int i = 0; i < len; i++) {
        arr[i] = arr[i] * 2;
    }
}
\`\`\`

### Your Task

Write a function \`void reverse(int *arr, int len)\` that reverses an array in place. Call it on \`{1, 2, 3, 4, 5}\` and print each element separated by spaces.`,

	starterCode: `#include <stdio.h>

void reverse(int *arr, int len) {
\t// Reverse the array in place
}

int main() {
\tint nums[] = {1, 2, 3, 4, 5};
\treverse(nums, 5);
\tfor (int i = 0; i < 5; i++) {
\t\tif (i > 0) printf(" ");
\t\tprintf("%d", nums[i]);
\t}
\tprintf("\\n");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void reverse(int *arr, int len) {
\tfor (int i = 0; i < len / 2; i++) {
\t\tint tmp = arr[i];
\t\tarr[i] = arr[len - 1 - i];
\t\tarr[len - 1 - i] = tmp;
\t}
}

int main() {
\tint nums[] = {1, 2, 3, 4, 5};
\treverse(nums, 5);
\tfor (int i = 0; i < 5; i++) {
\t\tif (i > 0) printf(" ");
\t\tprintf("%d", nums[i]);
\t}
\tprintf("\\n");
\treturn 0;
}
`,

	tests: [
		{
			name: "reverses array",
			expected: "5 4 3 2 1\n",
		},
		{
			name: "reverse single element",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint arr[] = {42};
\treverse(arr, 1);
\tprintf("%d\\n", arr[0]);
\treturn 0;
}`,
			expected: "42\n",
		},
		{
			name: "reverse two elements",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint arr[] = {1, 2};
\treverse(arr, 2);
\tprintf("%d %d\\n", arr[0], arr[1]);
\treturn 0;
}`,
			expected: "2 1\n",
		},
		{
			name: "reverse even-length array",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint arr[] = {10, 20, 30, 40};
\treverse(arr, 4);
\tfor (int i = 0; i < 4; i++) {
\t\tif (i > 0) printf(" ");
\t\tprintf("%d", arr[i]);
\t}
\tprintf("\\n");
\treturn 0;
}`,
			expected: "40 30 20 10\n",
		},
	],
};
