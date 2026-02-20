import type { Lesson } from "../../types";

export const fold: Lesson = {
	id: "fold",
	title: "fold",
	chapterId: "transformation",
	content: `## The \`fold\` Command

\`fold\` wraps long lines by inserting a newline after every \`width\` characters:

\`\`\`bash
$ echo "hello world" | fold -w 5
hello
 worl
d
\`\`\`

It is a pure character-level wrap — it does not care about words. Every \`width\` characters, a newline is inserted.

### Your Implementation

Write \`void my_fold(const char *s, int width)\` that wraps lines at \`width\` characters.

Track a column counter. Print each character and increment the counter. When the counter reaches \`width\`, print a newline and reset it. Reset the counter also whenever the input has a real newline.

\`\`\`c
void my_fold(const char *s, int width) {
    int col = 0;
    while (*s) {
        if (*s == '\\n') {
            putchar('\\n');
            col = 0;
        } else {
            if (col == width) { putchar('\\n'); col = 0; }
            putchar(*s);
            col++;
        }
        s++;
    }
}
\`\`\`

### Column Counter

\`col\` tracks how many characters have been printed on the current line. When it hits \`width\`, inject a newline and reset. Existing newlines in the input also reset the counter — the output preserves paragraph breaks.

### Your Task

Implement \`my_fold\` that wraps text at \`width\` characters.`,

	starterCode: `#include <stdio.h>

void my_fold(const char *s, int width) {
\t// Wrap lines at width characters
}

int main() {
\tmy_fold("hello world", 5);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_fold(const char *s, int width) {
\tint col = 0;
\twhile (*s) {
\t\tif (*s == '\\n') {
\t\t\tputchar('\\n');
\t\t\tcol = 0;
\t\t} else {
\t\t\tif (col == width) { putchar('\\n'); col = 0; }
\t\t\tputchar(*s);
\t\t\tcol++;
\t\t}
\t\ts++;
\t}
}

int main() {
\tmy_fold("hello world", 5);
\treturn 0;
}
`,

	tests: [
		{
			name: "wraps at 5 characters",
			expected: "hello\n worl\nd",
		},
		{
			name: "wraps at 4 characters",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_fold("abcdefgh", 4);
\treturn 0;
}`,
			expected: "abcd\nefgh",
		},
		{
			name: "short string fits on one line",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_fold("hi", 10);
\treturn 0;
}`,
			expected: "hi",
		},
		{
			name: "preserves existing newlines",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_fold("abc\\ndef", 5);
\treturn 0;
}`,
			expected: "abc\ndef",
		},
	],
};
