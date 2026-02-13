import type { Lesson } from "../../types";

export const recursion: Lesson = {
	id: "recursion",
	title: "Recursion",
	chapterId: "functions",
	content: `## Recursion

A recursive function is one that calls itself. Every recursive function needs a **base case** to stop the recursion.

### Factorial

The classic example:

\`\`\`c
int factorial(int n) {
    if (n <= 1) return 1;       // base case
    return n * factorial(n - 1); // recursive case
}
\`\`\`

How \`factorial(4)\` evaluates:
\`\`\`
factorial(4) = 4 * factorial(3)
             = 4 * 3 * factorial(2)
             = 4 * 3 * 2 * factorial(1)
             = 4 * 3 * 2 * 1
             = 24
\`\`\`

### Fibonacci

Another classic recursive function:

\`\`\`c
int fib(int n) {
    if (n <= 0) return 0;
    if (n == 1) return 1;
    return fib(n - 1) + fib(n - 2);
}
\`\`\`

### Assembly View

Check the **Assembly** tab after running to see how recursive calls translate to \`BL\` (branch with link) instructions and stack frame setup with \`STP\`/\`LDP\`.

### Your Task

Write a function \`int factorial(int n)\` that computes the factorial of n. Print \`factorial(0)\`, \`factorial(1)\`, \`factorial(5)\`, and \`factorial(10)\`, each on a separate line.`,

	starterCode: `#include <stdio.h>

int factorial(int n) {
\t// Implement factorial recursively
\treturn 0;
}

int main() {
\tprintf("%d\\n", factorial(0));
\tprintf("%d\\n", factorial(1));
\tprintf("%d\\n", factorial(5));
\tprintf("%d\\n", factorial(10));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int factorial(int n) {
\tif (n <= 1) return 1;
\treturn n * factorial(n - 1);
}

int main() {
\tprintf("%d\\n", factorial(0));
\tprintf("%d\\n", factorial(1));
\tprintf("%d\\n", factorial(5));
\tprintf("%d\\n", factorial(10));
\treturn 0;
}
`,

	tests: [
		{
			name: "computes factorials",
			expected: "1\n1\n120\n3628800\n",
		},
		{
			name: "factorial(0) = 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", factorial(0));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "factorial(1) = 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", factorial(1));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "factorial(6) = 720",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", factorial(6));
\treturn 0;
}`,
			expected: "720\n",
		},
		{
			name: "factorial(8) = 40320",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", factorial(8));
\treturn 0;
}`,
			expected: "40320\n",
		},
	],
};
