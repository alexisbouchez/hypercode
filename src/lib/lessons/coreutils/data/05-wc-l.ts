import type { Lesson } from "../../types";

export const wcL: Lesson = {
	id: "wc-l",
	title: "wc -l",
	chapterId: "counting",
	content: `## wc -l: Count Lines

\`wc -l\` counts the number of lines in a file:

\`\`\`bash
$ wc -l notes.txt
3 notes.txt
\`\`\`

A "line" in Unix means a string terminated by a newline character \`'\\n'\`. A file with 3 newlines has 3 lines.

### Your Implementation

Write \`int count_lines(const char *s)\` that counts the number of newline characters in the string.

\`\`\`c
int count_lines(const char *s) {
    int n = 0;
    while (*s) {
        if (*s == '\\n') n++;
        s++;
    }
    return n;
}
\`\`\`

Simple: walk the string, increment the counter every time you see \`'\\n'\`.

### A Common Use Case

Counting lines is incredibly useful in pipelines:

\`\`\`bash
ls | wc -l          # how many files?
grep "error" log | wc -l    # how many errors?
\`\`\`

The output of one command flows into \`wc -l\`, which tells you how many lines (i.e., how many results) there were.

### Your Task

Implement \`count_lines\` that returns the number of newline characters.`,

	starterCode: `#include <stdio.h>

int count_lines(const char *s) {
\t// Count newline characters
\treturn 0;
}

int main() {
\tprintf("%d\\n", count_lines("Learn Linux\\nPractice daily\\nHave fun\\n"));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int count_lines(const char *s) {
\tint n = 0;
\twhile (*s) {
\t\tif (*s == '\\n') n++;
\t\ts++;
\t}
\treturn n;
}

int main() {
\tprintf("%d\\n", count_lines("Learn Linux\\nPractice daily\\nHave fun\\n"));
\treturn 0;
}
`,

	tests: [
		{
			name: "count_lines with 3 lines",
			expected: "3\n",
		},
		{
			name: "count_lines(\"\") = 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_lines(""));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "count_lines with 1 line",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_lines("hello\\n"));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "count_lines with 5 lines",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_lines("a\\nb\\nc\\nd\\ne\\n"));
\treturn 0;
}`,
			expected: "5\n",
		},
	],
};
