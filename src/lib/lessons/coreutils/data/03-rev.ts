import type { Lesson } from "../../types";

export const rev: Lesson = {
	id: "rev",
	title: "rev",
	chapterId: "output",
	content: `## The \`rev\` Command

\`rev\` reverses each line of its input. Each line is printed backwards; the line order stays the same.

\`\`\`bash
$ echo "hello" | rev
olleh

$ printf "cat\\ndog\\n" | rev
tac
god
\`\`\`

### Your Implementation

Write \`void my_rev(const char *s)\` that reverses each line.

The algorithm: scan forward through the string. When you hit a \`\\n\` (or end of string), you know where the current line ends. Walk backwards from end to start, printing each character, then print the newline.

\`\`\`c
void my_rev(const char *s) {
    const char *line_start = s;

    while (*s) {
        if (*s == '\\n') {
            // Print characters from (s-1) down to line_start
            const char *p = s - 1;
            while (p >= line_start) {
                putchar(*p);
                p--;
            }
            putchar('\\n');
            line_start = s + 1;
        }
        s++;
    }
}
\`\`\`

### Pointer Arithmetic

This lesson is about **pointer arithmetic** — a core C skill:
- \`const char *line_start = s\` — pointer to the start of the current line
- \`const char *p = s - 1\` — pointer one character before the \`\\n\`
- \`p--\` — move the pointer one character earlier
- \`p >= line_start\` — stop when we have passed the start

Pointers and array indices are interchangeable in C: \`p[0]\` and \`*p\` are the same thing.

### Your Task

Implement \`my_rev\` so it prints each line reversed.`,

	starterCode: `#include <stdio.h>

void my_rev(const char *s) {
\t// Reverse each line and print it
}

int main() {
\tmy_rev("hello\\nworld\\n");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_rev(const char *s) {
\tconst char *line_start = s;

\twhile (*s) {
\t\tif (*s == '\\n') {
\t\t\tconst char *p = s - 1;
\t\t\twhile (p >= line_start) {
\t\t\t\tputchar(*p);
\t\t\t\tp--;
\t\t\t}
\t\t\tputchar('\\n');
\t\t\tline_start = s + 1;
\t\t}
\t\ts++;
\t}
}

int main() {
\tmy_rev("hello\\nworld\\n");
\treturn 0;
}
`,

	tests: [
		{
			name: "reverses two lines",
			expected: "olleh\ndlrow\n",
		},
		{
			name: "reverses single word",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_rev("coreutils\\n");
\treturn 0;
}`,
			expected: "slitueroc\n",
		},
		{
			name: "reverses cat and dog",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_rev("cat\\ndog\\n");
\treturn 0;
}`,
			expected: "tac\ngod\n",
		},
	],
};
