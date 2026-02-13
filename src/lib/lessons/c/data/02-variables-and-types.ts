import type { Lesson } from "../../types";

export const variablesAndTypes: Lesson = {
	id: "variables-and-types",
	title: "Variables and Types",
	chapterId: "basics",
	content: `## Variables and Types

C is a statically typed language. Every variable must be declared with a type before use.

> The Enterprise's main computer stores data in specific types too -- just like C requires explicit type declarations. No ambiguity allowed on the bridge.

### Basic Types

| Type | Size | Description |
|------|------|-------------|
| \`int\` | 4 bytes | Integer (at least 16 bits, typically 32) |
| \`char\` | 1 byte | Single character / small integer |
| \`long\` | 8 bytes | Long integer (at least 32 bits, typically 64) |
| \`float\` | 4 bytes | Single-precision floating point |
| \`double\` | 8 bytes | Double-precision floating point |

### Declaring Variables

\`\`\`c
int age = 25;
char letter = 'A';
long big = 1000000000L;
\`\`\`

### Format Specifiers

Use the matching format specifier when printing:

\`\`\`c
int x = 42;
printf("x = %d\\n", x);

char c = 'Z';
printf("c = %c\\n", c);

long n = 999;
printf("n = %ld\\n", n);
\`\`\`

### Constants

Use \`const\` to declare a value that cannot be changed:

\`\`\`c
const int MAX = 100;
\`\`\`

### Your Task

Declare three variables: an \`int\` set to 42, a \`char\` set to \`'Z'\`, and a \`long\` set to 100. Print them each on a separate line using the format shown in the expected output.`,

	starterCode: `#include <stdio.h>

int main() {
\t// Declare: int x = 42, char c = 'Z', long n = 100
\t// Print each on its own line:
\t// "x = 42"
\t// "c = Z"
\t// "n = 100"
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int main() {
\tint x = 42;
\tchar c = 'Z';
\tlong n = 100;
\tprintf("x = %d\\n", x);
\tprintf("c = %c\\n", c);
\tprintf("n = %ld\\n", n);
\treturn 0;
}
`,

	tests: [
		{
			name: "prints variables",
			expected: "x = 42\nc = Z\nn = 100\n",
		},
	],
};
