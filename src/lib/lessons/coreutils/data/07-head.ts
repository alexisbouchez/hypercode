import type { Lesson } from "../../types";

export const head: Lesson = {
	id: "head",
	title: "head",
	chapterId: "filtering",
	content: `## The \`head\` Command

\`head -n N\` prints the first N lines of its input:

\`\`\`bash
$ head -n 2 notes.txt
Learn Linux
Practice daily
\`\`\`

### Your Implementation

Write \`void my_head(const char *s, int n)\` that prints the first \`n\` lines.

The algorithm: walk the string, printing each character. Keep a counter of how many newlines you have seen. Stop when you have printed \`n\` complete lines.

\`\`\`c
void my_head(const char *s, int n) {
    int lines = 0;
    while (*s && lines < n) {
        putchar(*s);
        if (*s == '\\n') lines++;
        s++;
    }
}
\`\`\`

Notice that the line count increments **after** printing the newline — so the newline is included in the output. This means "printing a line" includes its terminating \`\\n\`.

### Early Exit

The \`while (*s && lines < n)\` condition exits as soon as either:
- We reach the end of the string (\`*s == '\\0'\`)
- We have printed \`n\` complete lines (\`lines == n\`)

This is more efficient than the real \`head\`, which exits as soon as N lines have been written so that \`cat hugefile.txt | head -n 1\` does not read the whole file.

### Your Task

Implement \`my_head\` that prints the first \`n\` lines of the string.`,

	starterCode: `#include <stdio.h>

void my_head(const char *s, int n) {
\t// Print the first n lines
}

int main() {
\tmy_head("Learn Linux\\nPractice daily\\nHave fun\\n", 2);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_head(const char *s, int n) {
\tint lines = 0;
\twhile (*s && lines < n) {
\t\tputchar(*s);
\t\tif (*s == '\\n') lines++;
\t\ts++;
\t}
}

int main() {
\tmy_head("Learn Linux\\nPractice daily\\nHave fun\\n", 2);
\treturn 0;
}
`,

	tests: [
		{
			name: "head -n 2 prints first 2 lines",
			expected: "Learn Linux\nPractice daily\n",
		},
		{
			name: "head -n 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_head("a\\nb\\nc\\n", 1);
\treturn 0;
}`,
			expected: "a\n",
		},
		{
			name: "head -n 3 prints all 3 lines",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_head("a\\nb\\nc\\n", 3);
\treturn 0;
}`,
			expected: "a\nb\nc\n",
		},
		{
			name: "head -n 0 prints nothing",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_head("a\\nb\\nc\\n", 0);
\treturn 0;
}`,
			expected: "",
		},
	],
};
