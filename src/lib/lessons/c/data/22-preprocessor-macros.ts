import type { Lesson } from "../../types";

export const preprocessorMacros: Lesson = {
	id: "preprocessor-macros",
	title: "Preprocessor Macros",
	chapterId: "advanced",
	content: `## Preprocessor Macros

The C preprocessor runs before compilation, performing text substitution. Lines starting with \`#\` are preprocessor directives.

### Simple Constants

\`#define\` creates named constants -- the preprocessor replaces every occurrence with the value:

\`\`\`c
#define PI 3
#define MAX_SIZE 100

int arr[MAX_SIZE];
\`\`\`

### Macros with Parameters

Macros can take arguments, acting like inline functions:

\`\`\`c
#define SQUARE(x) ((x) * (x))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

printf("%d\\n", SQUARE(5));   // 25
printf("%d\\n", MAX(10, 20)); // 20
\`\`\`

### Why the Extra Parentheses?

Without parentheses, macros can produce unexpected results due to operator precedence:

\`\`\`c
#define BAD_SQUARE(x) x * x
printf("%d\\n", BAD_SQUARE(2 + 3));
// Expands to: 2 + 3 * 2 + 3 = 11 (not 25!)

#define GOOD_SQUARE(x) ((x) * (x))
printf("%d\\n", GOOD_SQUARE(2 + 3));
// Expands to: ((2 + 3) * (2 + 3)) = 25
\`\`\`

### Conditional Compilation

\`\`\`c
#define DEBUG 1

#if DEBUG
    printf("Debug mode\\n");
#endif
\`\`\`

### Your Task

Define the following macros:
- \`ABS(x)\` -- absolute value of x
- \`CLAMP(x, lo, hi)\` -- clamp x between lo and hi

Print \`ABS(-5)\`, \`ABS(3)\`, \`CLAMP(15, 0, 10)\`, and \`CLAMP(-3, 0, 10)\`.`,

	starterCode: `#include <stdio.h>

// Define ABS(x) macro
// Define CLAMP(x, lo, hi) macro

int main() {
\tprintf("%d\\n", ABS(-5));
\tprintf("%d\\n", ABS(3));
\tprintf("%d\\n", CLAMP(15, 0, 10));
\tprintf("%d\\n", CLAMP(-3, 0, 10));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

#define ABS(x) ((x) < 0 ? -(x) : (x))
#define CLAMP(x, lo, hi) ((x) < (lo) ? (lo) : ((x) > (hi) ? (hi) : (x)))

int main() {
\tprintf("%d\\n", ABS(-5));
\tprintf("%d\\n", ABS(3));
\tprintf("%d\\n", CLAMP(15, 0, 10));
\tprintf("%d\\n", CLAMP(-3, 0, 10));
\treturn 0;
}
`,

	tests: [
		{
			name: "ABS and CLAMP output",
			expected: "5\n3\n10\n0\n",
		},
		{
			name: "ABS(0) = 0",
			code: `#include <stdio.h>
#define ABS(x) ((x) < 0 ? -(x) : (x))
#define CLAMP(x, lo, hi) ((x) < (lo) ? (lo) : ((x) > (hi) ? (hi) : (x)))
int main() {
\tprintf("%d\\n", ABS(0));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "ABS(-100) = 100",
			code: `#include <stdio.h>
#define ABS(x) ((x) < 0 ? -(x) : (x))
#define CLAMP(x, lo, hi) ((x) < (lo) ? (lo) : ((x) > (hi) ? (hi) : (x)))
int main() {
\tprintf("%d\\n", ABS(-100));
\treturn 0;
}`,
			expected: "100\n",
		},
		{
			name: "CLAMP(5, 0, 10) = 5 (within range)",
			code: `#include <stdio.h>
#define ABS(x) ((x) < 0 ? -(x) : (x))
#define CLAMP(x, lo, hi) ((x) < (lo) ? (lo) : ((x) > (hi) ? (hi) : (x)))
int main() {
\tprintf("%d\\n", CLAMP(5, 0, 10));
\treturn 0;
}`,
			expected: "5\n",
		},
		{
			name: "CLAMP(50, 1, 20) = 20",
			code: `#include <stdio.h>
#define ABS(x) ((x) < 0 ? -(x) : (x))
#define CLAMP(x, lo, hi) ((x) < (lo) ? (lo) : ((x) > (hi) ? (hi) : (x)))
int main() {
\tprintf("%d\\n", CLAMP(50, 1, 20));
\treturn 0;
}`,
			expected: "20\n",
		},
	],
};
