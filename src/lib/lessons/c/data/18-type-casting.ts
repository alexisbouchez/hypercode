import type { Lesson } from "../../types";

export const typeCasting: Lesson = {
	id: "type-casting",
	title: "Type Casting",
	chapterId: "basics",
	content: `## Type Casting

Type casting converts a value from one type to another. C performs some conversions automatically (implicit) and allows you to request others explicitly.

### Implicit Conversions

C automatically converts between compatible types when needed:

\`\`\`c
int a = 5;
long b = a;      // int -> long (safe, no data loss)
int c = 3.14;    // double -> int (truncates to 3)
\`\`\`

> Like a Changeling shapeshifting: the same underlying matter, reinterpreted as a different form.

### Explicit Casts

Use the cast operator \`(type)\` to convert explicitly:

\`\`\`c
int a = 7;
int b = 2;
double result = (double)a / b;  // 3.5 (not 3)
\`\`\`

Without the cast, \`7 / 2\` would be integer division (3). Casting \`a\` to \`double\` forces floating-point division.

### Integer Truncation

Converting a larger type to a smaller one may lose data:

\`\`\`c
int big = 300;
char small = (char)big;  // 44 (300 % 256)
\`\`\`

### Characters and Integers

In C, \`char\` is an integer type. Every character has an ASCII value:

\`\`\`c
char c = 'A';
int ascii = c;      // 65
char next = c + 1;  // 'B' (66)
\`\`\`

Common ASCII values:
| Character | Value |
|-----------|-------|
| \`'0'\` | 48 |
| \`'A'\` | 65 |
| \`'a'\` | 97 |

### Converting Digit Characters

\`\`\`c
char digit = '7';
int value = digit - '0';  // 7
\`\`\`

### Your Task

Write a function \`int to_upper(int c)\` that converts a lowercase letter to uppercase. If the character is not a lowercase letter, return it unchanged. Use the fact that \`'a'\` is 97 and \`'A'\` is 65 (a difference of 32). Print the results for 'h', 'i', '!', and 'A'.`,

	starterCode: `#include <stdio.h>

int to_upper(int c) {
\t// Convert lowercase to uppercase, leave others unchanged
\treturn c;
}

int main() {
\tprintf("%c\\n", to_upper('h'));
\tprintf("%c\\n", to_upper('i'));
\tprintf("%c\\n", to_upper('!'));
\tprintf("%c\\n", to_upper('A'));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int to_upper(int c) {
\tif (c >= 'a' && c <= 'z') {
\t\treturn c - 32;
\t}
\treturn c;
}

int main() {
\tprintf("%c\\n", to_upper('h'));
\tprintf("%c\\n", to_upper('i'));
\tprintf("%c\\n", to_upper('!'));
\tprintf("%c\\n", to_upper('A'));
\treturn 0;
}
`,

	tests: [
		{
			name: "converts to uppercase",
			expected: "H\nI\n!\nA\n",
		},
		{
			name: "to_upper('a') = 'A'",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%c\\n", to_upper('a'));
\treturn 0;
}`,
			expected: "A\n",
		},
		{
			name: "to_upper('z') = 'Z'",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%c\\n", to_upper('z'));
\treturn 0;
}`,
			expected: "Z\n",
		},
		{
			name: "to_upper('5') unchanged",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%c\\n", to_upper('5'));
\treturn 0;
}`,
			expected: "5\n",
		},
		{
			name: "to_upper('Z') unchanged",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%c\\n", to_upper('Z'));
\treturn 0;
}`,
			expected: "Z\n",
		},
	],
};
