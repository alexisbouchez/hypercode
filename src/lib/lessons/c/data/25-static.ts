import type { Lesson } from "../../types";

export const staticVariables: Lesson = {
	id: "static",
	title: "Static Variables",
	chapterId: "functions",
	content: `## Static Variables

A normal local variable is created when its function is called and destroyed when the function returns. A **static** local variable is different: it's initialized once and retains its value between calls.

### Regular vs Static

\`\`\`c
void counter() {
    int count = 0;       // reset every call
    count++;
    printf("%d\\n", count); // always prints 1
}

void sticky_counter() {
    static int count = 0; // initialized once
    count++;
    printf("%d\\n", count); // prints 1, 2, 3, ...
}
\`\`\`

The \`static\` keyword tells the compiler to store the variable in a persistent location (the data segment) rather than on the stack. The initializer runs only on the first call.

### Use Cases

Static locals are useful for:

- **Counters**: tracking how many times a function has been called
- **Caching**: remembering a previously computed result
- **State machines**: keeping track of internal state between calls

> Like the ship's computer on the Enterprise -- it remembers the stardate of every query you've ever made, even after you walk away from the console.

### Static at File Scope

When \`static\` is used on a global variable or function, it limits visibility to the current file (internal linkage). This is useful in multi-file projects to avoid naming conflicts, but won't affect our single-file exercises.

### Assembly View

Check the **Assembly** tab after running. Static variables appear in the \`.data\` section rather than being allocated on the stack with \`SUB SP\` instructions.

### Your Task

Write a function \`int next_id()\` that returns incrementing IDs starting from 1. The first call returns 1, the second returns 2, and so on. Use a static variable to remember the current count.

Print the result of calling \`next_id()\` five times, each on a separate line.`,

	starterCode: `#include <stdio.h>

int next_id() {
\t// Use a static variable to return 1, 2, 3, ...
\treturn 0;
}

int main() {
\tfor (int i = 0; i < 5; i++) {
\t\tprintf("%d\\n", next_id());
\t}
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int next_id() {
\tstatic int id = 0;
\tid++;
\treturn id;
}

int main() {
\tfor (int i = 0; i < 5; i++) {
\t\tprintf("%d\\n", next_id());
\t}
\treturn 0;
}
`,

	tests: [
		{
			name: "returns incrementing IDs",
			expected: "1\n2\n3\n4\n5\n",
		},
		{
			name: "next_id() starts at 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", next_id());
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "next_id() increments across calls",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tnext_id();
\tnext_id();
\tprintf("%d\\n", next_id());
\treturn 0;
}`,
			expected: "3\n",
		},
		{
			name: "next_id() continues incrementing",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tfor (int i = 0; i < 10; i++) {
\t\tnext_id();
\t}
\tprintf("%d\\n", next_id());
\treturn 0;
}`,
			expected: "11\n",
		},
	],
};
