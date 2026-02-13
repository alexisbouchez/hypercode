import type { Lesson } from "../../types";

export const arithmetic: Lesson = {
	id: "arithmetic",
	title: "Arithmetic",
	chapterId: "basics",
	content: `## Arithmetic Operators

C supports the standard arithmetic operators:

| Operator | Operation |
|----------|-----------|
| \`+\` | Addition |
| \`-\` | Subtraction |
| \`*\` | Multiplication |
| \`/\` | Division |
| \`%\` | Modulus (remainder) |

### Integer Arithmetic

When both operands are integers, the result is an integer. Division truncates toward zero:

\`\`\`c
int a = 17;
int b = 5;
printf("%d\\n", a / b);   // 3 (not 3.4)
printf("%d\\n", a % b);   // 2 (remainder)
\`\`\`

### Operator Precedence

C follows standard mathematical precedence: \`*\`, \`/\`, \`%\` are evaluated before \`+\`, \`-\`. Use parentheses to override:

\`\`\`c
int result = 2 + 3 * 4;    // 14, not 20
int other = (2 + 3) * 4;   // 20
\`\`\`

### Type Casting

You can convert between types using a cast:

\`\`\`c
int a = 7;
int b = 2;
printf("%d\\n", a / b);  // 3 (integer division)
\`\`\`

### Your Task

Given \`a = 17\` and \`b = 5\`, print the results of addition, subtraction, multiplication, integer division, and modulus, each on a separate line.`,

	starterCode: `#include <stdio.h>

int main() {
\tint a = 17;
\tint b = 5;
\t// Print: a + b, a - b, a * b, a / b, a % b
\t// Each on its own line, just the number
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int main() {
\tint a = 17;
\tint b = 5;
\tprintf("%d\\n", a + b);
\tprintf("%d\\n", a - b);
\tprintf("%d\\n", a * b);
\tprintf("%d\\n", a / b);
\tprintf("%d\\n", a % b);
\treturn 0;
}
`,

	tests: [
		{
			name: "prints arithmetic results",
			expected: "22\n12\n85\n3\n2\n",
		},
	],
};
