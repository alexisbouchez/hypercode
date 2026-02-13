import type { Lesson } from "../../types";

export const pointers: Lesson = {
	id: "pointers",
	title: "Pointers",
	chapterId: "pointers",
	content: `## Pointers

A pointer is a variable that holds the memory address of another variable. Pointers are fundamental to C -- they enable dynamic data structures, efficient parameter passing, and direct memory manipulation.

### Address-of and Dereference

- \`&x\` -- the **address-of** operator gives you the address of variable \`x\`
- \`*p\` -- the **dereference** operator reads or writes the value at the address stored in \`p\`

\`\`\`c
int x = 42;
int *p = &x;       // p holds the address of x
printf("%d\\n", *p); // 42 -- dereference p to read x's value
*p = 99;            // write 99 to the address p points to
printf("%d\\n", x);  // 99 -- x was modified through p
\`\`\`

### Pointer Types

A pointer's type determines what type it points to:

\`\`\`c
int *ip;     // pointer to int
char *cp;    // pointer to char
long *lp;    // pointer to long
\`\`\`

### Pointer Arithmetic

Adding 1 to a pointer advances it by \`sizeof(*p)\` bytes:

\`\`\`c
int arr[3] = {10, 20, 30};
int *p = arr;       // points to arr[0]
printf("%d\\n", *p);     // 10
printf("%d\\n", *(p+1)); // 20
printf("%d\\n", *(p+2)); // 30
\`\`\`

### NULL Pointer

A pointer that doesn't point to anything should be set to \`NULL\`:

\`\`\`c
int *p = NULL;  // or: int *p = 0;
\`\`\`

### Your Task

Write a function \`void swap(int *a, int *b)\` that swaps the values of two integers using pointers. Use it to swap x=10 and y=20, then print both values.`,

	starterCode: `#include <stdio.h>

void swap(int *a, int *b) {
\t// Swap the values at addresses a and b
}

int main() {
\tint x = 10;
\tint y = 20;
\tswap(&x, &y);
\tprintf("%d %d\\n", x, y);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void swap(int *a, int *b) {
\tint tmp = *a;
\t*a = *b;
\t*b = tmp;
}

int main() {
\tint x = 10;
\tint y = 20;
\tswap(&x, &y);
\tprintf("%d %d\\n", x, y);
\treturn 0;
}
`,

	tests: [
		{
			name: "swaps values",
			expected: "20 10\n",
		},
		{
			name: "swap(1, 2)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint a = 1, b = 2;
\tswap(&a, &b);
\tprintf("%d %d\\n", a, b);
\treturn 0;
}`,
			expected: "2 1\n",
		},
		{
			name: "swap(0, 0)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint a = 0, b = 0;
\tswap(&a, &b);
\tprintf("%d %d\\n", a, b);
\treturn 0;
}`,
			expected: "0 0\n",
		},
		{
			name: "swap(42, 7)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint a = 42, b = 7;
\tswap(&a, &b);
\tprintf("%d %d\\n", a, b);
\treturn 0;
}`,
			expected: "7 42\n",
		},
	],
};
