import type { Lesson } from "../../types";

export const tr: Lesson = {
	id: "tr",
	title: "tr",
	chapterId: "transformation",
	content: `## The \`tr\` Command

\`tr\` (**translate**) replaces every occurrence of one character with another:

\`\`\`bash
$ echo "hello world" | tr 'o' '0'
hell0 w0rld
\`\`\`

It is a character-level find-and-replace. Every character in the input is either passed through or replaced.

### Your Implementation

Write \`void my_tr(const char *s, char from, char to)\` that replaces every occurrence of \`from\` with \`to\`, and prints all other characters unchanged.

\`\`\`c
void my_tr(const char *s, char from, char to) {
    while (*s) {
        putchar(*s == from ? to : *s);
        s++;
    }
}
\`\`\`

The ternary operator \`*s == from ? to : *s\` picks between the replacement and the original in a single expression.

### Real tr is More Powerful

The real \`tr\` command takes ranges and sets:

\`\`\`bash
tr 'a-z' 'A-Z'     # uppercase all lowercase letters
tr -d ' '           # delete all spaces (-d = delete)
tr -s ' '           # squeeze repeated spaces into one (-s = squeeze)
\`\`\`

Your simplified version handles the single-character case, which is the core idea.

### Your Task

Implement \`my_tr\` that replaces every \`from\` character with \`to\`.`,

	starterCode: `#include <stdio.h>

void my_tr(const char *s, char from, char to) {
\t// Replace every occurrence of 'from' with 'to'
}

int main() {
\tmy_tr("hello world\\n", 'o', '0');
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_tr(const char *s, char from, char to) {
\twhile (*s) {
\t\tputchar(*s == from ? to : *s);
\t\ts++;
\t}
}

int main() {
\tmy_tr("hello world\\n", 'o', '0');
\treturn 0;
}
`,

	tests: [
		{
			name: "replaces o with 0",
			expected: "hell0 w0rld\n",
		},
		{
			name: "replaces spaces with underscores",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tr("hello world\\n", ' ', '_');
\treturn 0;
}`,
			expected: "hello_world\n",
		},
		{
			name: "no match passes through",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tr("abcdef\\n", 'z', 'Z');
\treturn 0;
}`,
			expected: "abcdef\n",
		},
		{
			name: "replaces a with @",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tr("banana\\n", 'a', '@');
\treturn 0;
}`,
			expected: "b@n@n@\n",
		},
	],
};
