import type { Lesson } from "../../types";

export const strings: Lesson = {
	id: "strings",
	title: "Strings",
	chapterId: "arrays-and-strings",
	content: `## Strings

In C, strings are arrays of \`char\` terminated by a null byte (\`'\\0'\`). There is no dedicated string type.

### String Literals

\`\`\`c
const char *greeting = "Hello";
\`\`\`

This creates a string \`{'H', 'e', 'l', 'l', 'o', '\\0'}\` in memory and stores its address in \`greeting\`.

> Subspace messages: character arrays transmitted across the quadrant, always null-terminated so you know where the message ends.

### Character Arrays

You can also store strings in character arrays:

\`\`\`c
char name[10] = "Alice";
\`\`\`

This copies the string into the \`name\` array. You can modify it (unlike string literals).

### String Functions (string.h)

| Function | Description |
|----------|-------------|
| \`strlen(s)\` | Returns the length (excluding null terminator) |
| \`strcmp(s1, s2)\` | Compares two strings (0 if equal) |
| \`strcpy(dest, src)\` | Copies src into dest |

### Printing Strings

\`\`\`c
printf("%s\\n", greeting);    // Hello
puts(greeting);               // Hello (adds newline automatically)
\`\`\`

### Iterating Characters

\`\`\`c
const char *s = "Hello";
for (int i = 0; s[i] != '\\0'; i++) {
    printf("%c", s[i]);
}
printf("\\n");
\`\`\`

### Your Task

Write a function \`int count_char(const char *s, char c)\` that counts how many times character \`c\` appears in string \`s\`. Call it from main to count the letter 'l' in "hello world" and print the result.`,

	starterCode: `#include <stdio.h>

int count_char(const char *s, char c) {
\t// Count occurrences of c in s
\treturn 0;
}

int main() {
\tprintf("%d\\n", count_char("hello world", 'l'));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int count_char(const char *s, char c) {
\tint count = 0;
\tfor (int i = 0; s[i] != '\\0'; i++) {
\t\tif (s[i] == c) {
\t\t\tcount++;
\t\t}
\t}
\treturn count;
}

int main() {
\tprintf("%d\\n", count_char("hello world", 'l'));
\treturn 0;
}
`,

	tests: [
		{
			name: "counts character occurrences",
			expected: "3\n",
		},
		{
			name: "count_char('aaa', 'a') = 3",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_char("aaa", 'a'));
\treturn 0;
}`,
			expected: "3\n",
		},
		{
			name: "count_char('hello', 'z') = 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_char("hello", 'z'));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "count_char('banana', 'a') = 3",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_char("banana", 'a'));
\treturn 0;
}`,
			expected: "3\n",
		},
		{
			name: "count_char('', 'x') = 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_char("", 'x'));
\treturn 0;
}`,
			expected: "0\n",
		},
	],
};
