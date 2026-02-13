import type { Lesson } from "../../types";

export const helloC: Lesson = {
	id: "hello-c",
	title: "Hello, C!",
	chapterId: "basics",
	content: `## Your First C Program

C is one of the most influential programming languages ever created. Developed by Dennis Ritchie at Bell Labs in 1972, it remains the foundation of operating systems, embedded systems, and high-performance software.

> C (1972) is ancient technology -- like discovering Vulcan ruins that still power modern civilization.

### Program Structure

Every C program needs a \`main\` function -- this is where execution begins:

\`\`\`c
#include <stdio.h>

int main() {
    printf("Hello, World!\\n");
    return 0;
}
\`\`\`

Let's break this down:

- \`#include <stdio.h>\` -- includes the standard I/O library, giving us access to \`printf\`.
- \`int main()\` -- the entry point. The \`int\` means it returns an integer (0 = success).
- \`printf("Hello, World!\\n")\` -- prints text to stdout. \`\\n\` is a newline character.
- \`return 0\` -- returns exit code 0 (success) to the operating system.

### printf

\`printf\` is the most common way to output text in C. It supports format specifiers:

| Specifier | Type |
|-----------|------|
| \`%d\` | Integer |
| \`%s\` | String |
| \`%c\` | Character |
| \`%x\` | Hexadecimal |
| \`%%\` | Literal % |

### Assembly View

Click **Run** and then check the **Assembly** tab to see the ARM64 assembly generated from your C code. You'll see how \`printf\` gets compiled into function calls and how string literals end up in the data section.

### Your Task

Write a program that prints exactly \`Hello, C!\\n\` to stdout.`,

	starterCode: `#include <stdio.h>

int main() {
\t// Print "Hello, C!" followed by a newline
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int main() {
\tprintf("Hello, C!\\n");
\treturn 0;
}
`,

	tests: [
		{
			name: "prints Hello, C!",
			expected: "Hello, C!\n",
		},
	],
};
