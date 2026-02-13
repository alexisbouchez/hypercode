import type { Lesson } from "../../types";

export const functionPointers: Lesson = {
	id: "function-pointers",
	title: "Function Pointers",
	chapterId: "advanced",
	content: `## Function Pointers

In C, functions have addresses in memory just like variables. A **function pointer** stores the address of a function, letting you call functions indirectly.

### Declaring Function Pointers

The syntax mirrors the function signature, with \`(*name)\` replacing the function name:

\`\`\`c
int add(int a, int b) { return a + b; }

// Declare a pointer to a function taking two ints, returning int
int (*op)(int, int) = add;
printf("%d\\n", op(3, 4));  // 7
\`\`\`

### Why Function Pointers?

They let you write **generic code**. For example, an apply function that works with any operation:

\`\`\`c
int apply(int (*f)(int, int), int x, int y) {
    return f(x, y);
}

int mul(int a, int b) { return a * b; }

printf("%d\\n", apply(add, 2, 3));  // 5
printf("%d\\n", apply(mul, 2, 3));  // 6
\`\`\`

### Callback Pattern

Function pointers enable callbacks -- passing a function to be called later:

\`\`\`c
void for_each(int *arr, int n, void (*action)(int)) {
    for (int i = 0; i < n; i++) {
        action(arr[i]);
    }
}

void print_item(int x) {
    printf("%d\\n", x);
}

int nums[] = {10, 20, 30};
for_each(nums, 3, print_item);
\`\`\`

### Your Task

Write three functions: \`int add(int a, int b)\`, \`int sub(int a, int b)\`, and \`int mul(int a, int b)\`. Then write \`int apply(int (*f)(int, int), int a, int b)\` that calls the function pointer. Use \`apply\` to compute and print: 10+3, 10-3, 10*3.`,

	starterCode: `#include <stdio.h>

int add(int a, int b) {
\treturn 0;
}

int sub(int a, int b) {
\treturn 0;
}

int mul(int a, int b) {
\treturn 0;
}

int apply(int (*f)(int, int), int a, int b) {
\t// Call the function pointer
\treturn 0;
}

int main() {
\tprintf("%d\\n", apply(add, 10, 3));
\tprintf("%d\\n", apply(sub, 10, 3));
\tprintf("%d\\n", apply(mul, 10, 3));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int add(int a, int b) {
\treturn a + b;
}

int sub(int a, int b) {
\treturn a - b;
}

int mul(int a, int b) {
\treturn a * b;
}

int apply(int (*f)(int, int), int a, int b) {
\treturn f(a, b);
}

int main() {
\tprintf("%d\\n", apply(add, 10, 3));
\tprintf("%d\\n", apply(sub, 10, 3));
\tprintf("%d\\n", apply(mul, 10, 3));
\treturn 0;
}
`,

	tests: [
		{
			name: "apply with add, sub, mul",
			expected: "13\n7\n30\n",
		},
		{
			name: "apply(add, 5, 5) = 10",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", apply(add, 5, 5));
\treturn 0;
}`,
			expected: "10\n",
		},
		{
			name: "apply(sub, 100, 1) = 99",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", apply(sub, 100, 1));
\treturn 0;
}`,
			expected: "99\n",
		},
		{
			name: "apply(mul, 7, 8) = 56",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", apply(mul, 7, 8));
\treturn 0;
}`,
			expected: "56\n",
		},
		{
			name: "apply(add, 0, 0) = 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", apply(add, 0, 0));
\treturn 0;
}`,
			expected: "0\n",
		},
	],
};
