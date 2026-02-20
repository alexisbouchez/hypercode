import type { Lesson } from "../../types";

export const cat: Lesson = {
	id: "cat",
	title: "cat",
	chapterId: "output",
	content: `## The \`cat\` Command

\`cat\` (short for **concatenate**) reads files and writes their contents to stdout. In its simplest form:

\`\`\`bash
$ cat notes.txt
Learn Linux
Practice daily
Have fun
\`\`\`

The real \`cat\` reads from files or stdin, but the core operation is: take a string, print it unchanged.

### Your Implementation

Write \`void my_cat(const char *s)\` that prints \`s\` exactly as-is — no extra newlines, no modifications.

The key difference from \`echo\`: \`cat\` does **not** add a newline. The string already contains its own newlines:

\`\`\`c
void my_cat(const char *s) {
    printf("%s", s);     // No \\n — the content has its own
}
\`\`\`

### Walking Character by Character

You could also print one character at a time:

\`\`\`c
void my_cat(const char *s) {
    while (*s) {
        putchar(*s);
        s++;
    }
}
\`\`\`

Both work. The \`putchar\` version shows you how \`cat\` actually works under the hood — it reads bytes from input and writes bytes to output, one at a time.

### Your Task

Implement \`my_cat\` so it prints the string exactly as given.`,

	starterCode: `#include <stdio.h>

void my_cat(const char *s) {
\t// Print s exactly as-is (no extra newline)
}

int main() {
\tmy_cat("Learn Linux\\nPractice daily\\nHave fun\\n");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_cat(const char *s) {
\twhile (*s) {
\t\tputchar(*s);
\t\ts++;
\t}
}

int main() {
\tmy_cat("Learn Linux\\nPractice daily\\nHave fun\\n");
\treturn 0;
}
`,

	tests: [
		{
			name: "my_cat prints multi-line string unchanged",
			expected: "Learn Linux\nPractice daily\nHave fun\n",
		},
		{
			name: "my_cat with single line",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_cat("hello world\\n");
\treturn 0;
}`,
			expected: "hello world\n",
		},
		{
			name: "my_cat with two lines",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_cat("foo\\nbar\\n");
\treturn 0;
}`,
			expected: "foo\nbar\n",
		},
	],
};
