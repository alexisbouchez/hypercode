import type { Lesson } from "../../types";

export const conditionals: Lesson = {
	id: "conditionals",
	title: "Conditionals",
	chapterId: "control-flow",
	content: `## Conditionals

C uses \`if\`, \`else if\`, and \`else\` for conditional execution.

### if / else

\`\`\`c
int x = 10;
if (x > 0) {
    printf("positive\\n");
} else if (x < 0) {
    printf("negative\\n");
} else {
    printf("zero\\n");
}
\`\`\`

> Red alert conditions: "Shields up!" only triggers when sensors detect a threat. That's an \`if\` statement protecting the ship.

### Comparison Operators

| Operator | Meaning |
|----------|---------|
| \`==\` | Equal to |
| \`!=\` | Not equal to |
| \`<\` | Less than |
| \`>\` | Greater than |
| \`<=\` | Less than or equal |
| \`>=\` | Greater than or equal |

### Logical Operators

| Operator | Meaning |
|----------|---------|
| \`&&\` | Logical AND |
| \`\\|\\|\` | Logical OR |
| \`!\` | Logical NOT |

### The Ternary Operator

A compact way to write simple conditionals:

\`\`\`c
int x = 5;
const char *label = (x > 0) ? "positive" : "non-positive";
\`\`\`

### Your Task

Write a function \`classify\` that takes an \`int\` and prints \`"positive"\`, \`"negative"\`, or \`"zero"\` followed by a newline. Call it three times from \`main\` with the values 42, -7, and 0.`,

	starterCode: `#include <stdio.h>

void classify(int n) {
\t// Print "positive", "negative", or "zero" based on n
}

int main() {
\tclassify(42);
\tclassify(-7);
\tclassify(0);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void classify(int n) {
\tif (n > 0) {
\t\tprintf("positive\\n");
\t} else if (n < 0) {
\t\tprintf("negative\\n");
\t} else {
\t\tprintf("zero\\n");
\t}
}

int main() {
\tclassify(42);
\tclassify(-7);
\tclassify(0);
\treturn 0;
}
`,

	tests: [
		{
			name: "classifies numbers",
			expected: "positive\nnegative\nzero\n",
		},
		{
			name: "positive (100)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tclassify(100);
\treturn 0;
}`,
			expected: "positive\n",
		},
		{
			name: "negative (-1)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tclassify(-1);
\treturn 0;
}`,
			expected: "negative\n",
		},
		{
			name: "zero (0)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tclassify(0);
\treturn 0;
}`,
			expected: "zero\n",
		},
		{
			name: "positive (1)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tclassify(1);
\treturn 0;
}`,
			expected: "positive\n",
		},
	],
};
