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

Write a program that prints the numbers 1 through 10, each on a separate line.`,

	starterCode: `#include <stdio.h>

int main() {
\t// Print numbers 1 through 10, each on its own line
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int main() {
\tfor (int i = 1; i <= 10; i++) {
\t\tprintf("%d\\n", i);
\t}
\treturn 0;
}
`,

	tests: [
		{
			name: "prints 1 to 10",
			expected: "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n",
		},
	],
};
