import type { Lesson } from "../../types";

export const seq: Lesson = {
	id: "seq",
	title: "seq",
	chapterId: "output",
	content: `## The \`seq\` Command

\`seq\` prints a sequence of numbers, one per line:

\`\`\`bash
$ seq 1 5
1
2
3
4
5
\`\`\`

It takes a first and last value and counts up from one to the other.

### Your Implementation

Write \`void my_seq(int first, int last)\` that prints integers from \`first\` to \`last\` inclusive, one per line.

\`\`\`c
void my_seq(int first, int last) {
    for (int i = first; i <= last; i++)
        printf("%d\\n", i);
}
\`\`\`

That is all there is to it. A simple \`for\` loop.

### Going Backwards

The real \`seq\` also supports counting down when \`first > last\` by accepting a negative step:

\`\`\`bash
$ seq 5 -1 1
5
4
3
2
1
\`\`\`

Our version keeps it simple — just \`first\` to \`last\` counting upward.

### Your Task

Implement \`my_seq\` that prints integers from \`first\` to \`last\`, one per line.`,

	starterCode: `#include <stdio.h>

void my_seq(int first, int last) {
\t// Print integers from first to last, one per line
}

int main() {
\tmy_seq(1, 5);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_seq(int first, int last) {
\tfor (int i = first; i <= last; i++)
\t\tprintf("%d\\n", i);
}

int main() {
\tmy_seq(1, 5);
\treturn 0;
}
`,

	tests: [
		{
			name: "seq 1 5",
			expected: "1\n2\n3\n4\n5\n",
		},
		{
			name: "seq 1 1 prints a single number",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_seq(1, 1);
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "seq 3 7",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_seq(3, 7);
\treturn 0;
}`,
			expected: "3\n4\n5\n6\n7\n",
		},
		{
			name: "seq 0 3",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_seq(0, 3);
\treturn 0;
}`,
			expected: "0\n1\n2\n3\n",
		},
	],
};
