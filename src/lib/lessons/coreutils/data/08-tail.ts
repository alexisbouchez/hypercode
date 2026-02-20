import type { Lesson } from "../../types";

export const tail: Lesson = {
	id: "tail",
	title: "tail",
	chapterId: "filtering",
	content: `## The \`tail\` Command

\`tail -n N\` prints the **last** N lines of its input:

\`\`\`bash
$ tail -n 2 notes.txt
Practice daily
Have fun
\`\`\`

### Your Implementation

Write \`void my_tail(const char *s, int n)\` that prints the last \`n\` lines.

\`head\` was easy: scan forward, stop after N newlines. \`tail\` is harder because you do not know where the last N lines start until you have seen the whole string.

The approach:
1. Find the length of the string
2. Scan **backwards** from the end (skipping the trailing newline), counting newlines
3. Once you have counted \`n\` newlines, everything after that position is the last \`n\` lines

\`\`\`c
void my_tail(const char *s, int n) {
    int len = 0;
    while (s[len]) len++;

    // Skip trailing newline for counting
    int end = (len > 0 && s[len - 1] == '\\n') ? len - 1 : len;

    int count = 0;
    int start = 0;
    for (int i = end - 1; i >= 0; i--) {
        if (s[i] == '\\n') {
            count++;
            if (count == n) {
                start = i + 1;
                break;
            }
        }
    }
    printf("%s", s + start);
}
\`\`\`

### Two Passes

This is a **two-pass** algorithm: first pass finds the length, second pass scans backwards. The real \`tail\` uses a circular buffer to do it in one pass, but the two-pass approach is clear and correct.

### Your Task

Implement \`my_tail\` that prints the last \`n\` lines.`,

	starterCode: `#include <stdio.h>

void my_tail(const char *s, int n) {
\t// Print the last n lines
}

int main() {
\tmy_tail("Learn Linux\\nPractice daily\\nHave fun\\n", 2);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_tail(const char *s, int n) {
\tint len = 0;
\twhile (s[len]) len++;

\tint end = (len > 0 && s[len - 1] == '\\n') ? len - 1 : len;

\tint count = 0;
\tint start = 0;
\tfor (int i = end - 1; i >= 0; i--) {
\t\tif (s[i] == '\\n') {
\t\t\tcount++;
\t\t\tif (count == n) {
\t\t\t\tstart = i + 1;
\t\t\t\tbreak;
\t\t\t}
\t\t}
\t}
\tprintf("%s", s + start);
}

int main() {
\tmy_tail("Learn Linux\\nPractice daily\\nHave fun\\n", 2);
\treturn 0;
}
`,

	tests: [
		{
			name: "tail -n 2 prints last 2 lines",
			expected: "Practice daily\nHave fun\n",
		},
		{
			name: "tail -n 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tail("a\\nb\\nc\\n", 1);
\treturn 0;
}`,
			expected: "c\n",
		},
		{
			name: "tail -n 3 prints all lines",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tail("a\\nb\\nc\\n", 3);
\treturn 0;
}`,
			expected: "a\nb\nc\n",
		},
		{
			name: "tail -n 2 of 4 lines",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tail("w\\nx\\ny\\nz\\n", 2);
\treturn 0;
}`,
			expected: "y\nz\n",
		},
	],
};
