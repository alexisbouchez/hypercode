import type { Lesson } from "../../types";

export const loops: Lesson = {
	id: "loops",
	title: "Loops",
	chapterId: "control-flow",
	content: `## Loops

C has three loop constructs: \`for\`, \`while\`, and \`do-while\`.

### for Loop

The most common loop. It has three parts: initialization, condition, and update:

\`\`\`c
for (int i = 0; i < 5; i++) {
    printf("%d\\n", i);
}
\`\`\`

> "Captain, we appear to be caught in a temporal causality loop." -- The Enterprise keeps reliving the same moment (TNG S5E18). At least they had a \`break\` condition.

### while Loop

Repeats while a condition is true:

\`\`\`c
int i = 0;
while (i < 5) {
    printf("%d\\n", i);
    i++;
}
\`\`\`

### do-while Loop

Executes the body at least once, then checks the condition:

\`\`\`c
int i = 0;
do {
    printf("%d\\n", i);
    i++;
} while (i < 5);
\`\`\`

### break and continue

- \`break\` -- exits the loop immediately
- \`continue\` -- skips to the next iteration

\`\`\`c
for (int i = 0; i < 10; i++) {
    if (i == 5) break;       // stops at 5
    if (i % 2 == 0) continue; // skip even numbers
    printf("%d\\n", i);       // prints 1, 3
}
\`\`\`

### Your Task

Write a function \`int sum_range(int a, int b)\` that returns the sum of all integers from \`a\` to \`b\` (inclusive). For example, \`sum_range(1, 5)\` returns 15. Print the result of \`sum_range(1, 10)\`.`,

	starterCode: `#include <stdio.h>

int sum_range(int a, int b) {
\t// Sum all integers from a to b inclusive
\treturn 0;
}

int main() {
\tprintf("%d\\n", sum_range(1, 10));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int sum_range(int a, int b) {
\tint sum = 0;
\tfor (int i = a; i <= b; i++) {
\t\tsum = sum + i;
\t}
\treturn sum;
}

int main() {
\tprintf("%d\\n", sum_range(1, 10));
\treturn 0;
}
`,

	tests: [
		{
			name: "sum_range(1, 10) = 55",
			expected: "55\n",
		},
		{
			name: "sum_range(1, 5) = 15",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", sum_range(1, 5));
\treturn 0;
}`,
			expected: "15\n",
		},
		{
			name: "sum_range(3, 3) = 3",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", sum_range(3, 3));
\treturn 0;
}`,
			expected: "3\n",
		},
		{
			name: "sum_range(0, 0) = 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", sum_range(0, 0));
\treturn 0;
}`,
			expected: "0\n",
		},
	],
};
