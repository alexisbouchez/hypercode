import type { Lesson } from "../../types";

export const tac: Lesson = {
	id: "tac",
	title: "tac",
	chapterId: "transformation",
	content: `## The \`tac\` Command

\`tac\` is \`cat\` spelled backward — it prints lines in **reverse order**:

\`\`\`bash
$ printf "a\\nb\\nc\\n" | tac
c
b
a
\`\`\`

The last line becomes the first. The characters within each line are unchanged.

### Your Implementation

Write \`void my_tac(const char *s)\` that prints lines in reverse order.

The challenge: you do not know the last line until you have scanned through the entire string. You need to store the line positions first, then print them in reverse.

\`\`\`c
void my_tac(const char *s) {
    const char *starts[64];   // pointer to start of each line
    int count = 0;

    const char *p = s;
    const char *line_start = s;

    while (*p) {
        if (*p == '\\n') {
            starts[count++] = line_start;
            line_start = p + 1;
        }
        p++;
    }

    for (int i = count - 1; i >= 0; i--) {
        const char *q = starts[i];
        while (*q && *q != '\\n') { putchar(*q); q++; }
        putchar('\\n');
    }
}
\`\`\`

### Pointers as Array Entries

\`starts[count] = line_start\` stores a **pointer** (a memory address) in an array. Later, we use \`const char *q = starts[i]\` and walk forward with \`q++\` until we hit \`'\\n'\`.

This is a key C pattern: an array of pointers pointing into a larger buffer. It is how \`argv\` works — \`argv[0]\`, \`argv[1]\`, etc. are pointers into the program's argument memory.

### Your Task

Implement \`my_tac\` that prints lines in reverse order. You can assume the input has at most 64 lines.`,

	starterCode: `#include <stdio.h>

void my_tac(const char *s) {
\t// Print lines in reverse order
}

int main() {
\tmy_tac("a\\nb\\nc\\n");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_tac(const char *s) {
\tconst char *starts[64];
\tint count = 0;

\tconst char *p = s;
\tconst char *line_start = s;

\twhile (*p) {
\t\tif (*p == '\\n') {
\t\t\tstarts[count++] = line_start;
\t\t\tline_start = p + 1;
\t\t}
\t\tp++;
\t}

\tfor (int i = count - 1; i >= 0; i--) {
\t\tconst char *q = starts[i];
\t\twhile (*q && *q != '\\n') { putchar(*q); q++; }
\t\tputchar('\\n');
\t}
}

int main() {
\tmy_tac("a\\nb\\nc\\n");
\treturn 0;
}
`,

	tests: [
		{
			name: "reverses line order",
			expected: "c\nb\na\n",
		},
		{
			name: "single line stays the same",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tac("hello\\n");
\treturn 0;
}`,
			expected: "hello\n",
		},
		{
			name: "reverses two lines",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tac("first\\nsecond\\n");
\treturn 0;
}`,
			expected: "second\nfirst\n",
		},
		{
			name: "reverses four lines",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tac("w\\nx\\ny\\nz\\n");
\treturn 0;
}`,
			expected: "z\ny\nx\nw\n",
		},
	],
};
