import type { Lesson } from "../../types";

export const sorting: Lesson = {
	id: "sorting",
	title: "Sorting",
	chapterId: "advanced",
	content: `## Sorting

Sorting is one of the most fundamental algorithms in computer science. Let's implement **bubble sort**, a simple sorting algorithm.

### How Bubble Sort Works

Bubble sort repeatedly steps through the array, compares adjacent elements, and swaps them if they're in the wrong order. After each pass, the largest unsorted element "bubbles up" to its correct position.

\`\`\`
Pass 1: [5, 3, 8, 1] → [3, 5, 1, 8]   (8 is in place)
Pass 2: [3, 5, 1, 8] → [3, 1, 5, 8]   (5 is in place)
Pass 3: [3, 1, 5, 8] → [1, 3, 5, 8]   (done!)
\`\`\`

### Implementation

\`\`\`c
void bubble_sort(int *arr, int n) {
    for (int i = 0; i < n - 1; i++) {
        for (int j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                // Swap
                int temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
        }
    }
}
\`\`\`

### Swapping Elements

The swap pattern is essential in many algorithms:

\`\`\`c
int temp = a;
a = b;
b = temp;
\`\`\`

### Printing an Array

A helper to print arrays is handy for debugging:

\`\`\`c
void print_array(int *arr, int n) {
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\\n");
}
\`\`\`

### Your Task

Implement \`void bubble_sort(int *arr, int n)\` that sorts an array of \`n\` integers in ascending order. Sort and print two arrays: \`{5, 3, 8, 1, 9}\` and \`{10, 4, 7, 2}\`.`,

	starterCode: `#include <stdio.h>

void bubble_sort(int *arr, int n) {
\t// Sort arr in ascending order using bubble sort
}

void print_array(int *arr, int n) {
\tfor (int i = 0; i < n; i++) {
\t\tprintf("%d ", arr[i]);
\t}
\tprintf("\\n");
}

int main() {
\tint a[] = {5, 3, 8, 1, 9};
\tbubble_sort(a, 5);
\tprint_array(a, 5);

\tint b[] = {10, 4, 7, 2};
\tbubble_sort(b, 4);
\tprint_array(b, 4);

\treturn 0;
}
`,

	solution: `#include <stdio.h>

void bubble_sort(int *arr, int n) {
\tfor (int i = 0; i < n - 1; i++) {
\t\tfor (int j = 0; j < n - i - 1; j++) {
\t\t\tif (arr[j] > arr[j + 1]) {
\t\t\t\tint temp = arr[j];
\t\t\t\tarr[j] = arr[j + 1];
\t\t\t\tarr[j + 1] = temp;
\t\t\t}
\t\t}
\t}
}

void print_array(int *arr, int n) {
\tfor (int i = 0; i < n; i++) {
\t\tprintf("%d ", arr[i]);
\t}
\tprintf("\\n");
}

int main() {
\tint a[] = {5, 3, 8, 1, 9};
\tbubble_sort(a, 5);
\tprint_array(a, 5);

\tint b[] = {10, 4, 7, 2};
\tbubble_sort(b, 4);
\tprint_array(b, 4);

\treturn 0;
}
`,

	tests: [
		{
			name: "sorts both arrays",
			expected: "1 3 5 8 9 \n2 4 7 10 \n",
		},
		{
			name: "already sorted array",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint a[] = {1, 2, 3, 4, 5};
\tbubble_sort(a, 5);
\tprint_array(a, 5);
\treturn 0;
}`,
			expected: "1 2 3 4 5 \n",
		},
		{
			name: "reverse sorted array",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint a[] = {5, 4, 3, 2, 1};
\tbubble_sort(a, 5);
\tprint_array(a, 5);
\treturn 0;
}`,
			expected: "1 2 3 4 5 \n",
		},
		{
			name: "single element",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint a[] = {42};
\tbubble_sort(a, 1);
\tprint_array(a, 1);
\treturn 0;
}`,
			expected: "42 \n",
		},
		{
			name: "array with duplicates",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint a[] = {3, 1, 3, 1, 2};
\tbubble_sort(a, 5);
\tprint_array(a, 5);
\treturn 0;
}`,
			expected: "1 1 2 3 3 \n",
		},
	],
};
