import type { Lesson } from "../../types";

export const functions: Lesson = {
	id: "functions",
	title: "Functions",
	chapterId: "functions",
	content: `## Functions

Functions in C are declared with a return type, name, and parameter list:

\`\`\`c
int add(int a, int b) {
    return a + b;
}
\`\`\`

### Function Prototypes

In C, a function must be declared before it is called. You can either define it before use, or declare a prototype:

\`\`\`c
int add(int a, int b);  // prototype (declaration)

int main() {
    printf("%d\\n", add(3, 4));  // OK -- declared above
    return 0;
}

int add(int a, int b) {  // definition
    return a + b;
}
\`\`\`

### void Functions

Functions that return nothing use \`void\`:

\`\`\`c
void greet(const char *name) {
    printf("Hello, %s!\\n", name);
}
\`\`\`

### Parameters are Passed by Value

In C, function arguments are copied. Modifying a parameter inside a function does not affect the caller's variable:

\`\`\`c
void try_modify(int x) {
    x = 99;  // only modifies the local copy
}

int main() {
    int a = 5;
    try_modify(a);
    printf("%d\\n", a);  // still 5
}
\`\`\`

### Your Task

Write a function \`int max(int a, int b)\` that returns the larger of two integers. Use it in \`main\` to print the results of \`max(3, 7)\`, \`max(10, 2)\`, and \`max(5, 5)\`, each on a separate line.`,

	starterCode: `#include <stdio.h>

// Write the max function here

int main() {
\tprintf("%d\\n", max(3, 7));
\tprintf("%d\\n", max(10, 2));
\tprintf("%d\\n", max(5, 5));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int max(int a, int b) {
\tif (a >= b) return a;
\treturn b;
}

int main() {
\tprintf("%d\\n", max(3, 7));
\tprintf("%d\\n", max(10, 2));
\tprintf("%d\\n", max(5, 5));
\treturn 0;
}
`,

	tests: [
		{
			name: "max returns larger value",
			expected: "7\n10\n5\n",
		},
	],
};
