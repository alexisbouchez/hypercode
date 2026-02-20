import type { Lesson } from "../../types";

export const echo: Lesson = {
	id: "echo",
	title: "echo",
	chapterId: "output",
	content: `## The \`echo\` Command

Every Linux system has an \`echo\` command. It prints its argument to stdout followed by a newline. Here is how it works:

\`\`\`bash
$ echo "hello"
hello
\`\`\`

Simple — but it is a real compiled C program, part of the **GNU coreutils** package. Your task in this course is to reimplement each of these tools from scratch in C.

### Your Implementation

Write a function \`void my_echo(const char *s)\` that prints the string \`s\` followed by a newline character.

\`\`\`c
void my_echo(const char *s) {
    printf("%s\\n", s);
}
\`\`\`

\`printf\` with \`%s\\n\` is all you need. The real \`echo\` handles flags like \`-n\` (no newline) and \`-e\` (escape sequences), but start with the core behavior.

### Why Rewrite Coreutils?

The tools you used in the Linux course — \`cat\`, \`grep\`, \`wc\`, \`head\`, \`tail\` — are all C programs. They are small (a few hundred lines each), focused, and brilliant. Reimplementing them teaches you:

- How to process strings with pointer arithmetic
- How to build state machines for text parsing
- How C programs actually work under the hood
- Why the Unix philosophy ("do one thing well") produces great software

### Your Task

Implement \`my_echo\` so it prints the string followed by a newline.`,

	starterCode: `#include <stdio.h>

void my_echo(const char *s) {
\t// Print s followed by a newline
}

int main() {
\tmy_echo("Hello, World!");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_echo(const char *s) {
\tprintf("%s\\n", s);
}

int main() {
\tmy_echo("Hello, World!");
\treturn 0;
}
`,

	tests: [
		{
			name: "my_echo prints Hello, World!",
			expected: "Hello, World!\n",
		},
		{
			name: "my_echo prints coreutils",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_echo("coreutils");
\treturn 0;
}`,
			expected: "coreutils\n",
		},
		{
			name: "my_echo with empty string prints blank line",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_echo("");
\treturn 0;
}`,
			expected: "\n",
		},
	],
};
