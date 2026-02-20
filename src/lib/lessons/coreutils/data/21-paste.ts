import type { Lesson } from "../../types";

export const paste: Lesson = {
	id: "paste",
	title: "paste",
	chapterId: "transformation",
	content: `## The \`paste\` Command

\`paste\` merges corresponding lines from two inputs side-by-side with a delimiter:

\`\`\`bash
$ paste -d: names.txt ages.txt
alice:30
bob:25
carol:35
\`\`\`

Line 1 of \`names.txt\` is joined to line 1 of \`ages.txt\` with \`:\`, and so on.

### Your Implementation

Write \`void my_paste(const char *a, const char *b, char delim)\` that merges lines from \`a\` and \`b\` with \`delim\`.

The algorithm: advance through both strings simultaneously. For each pair of lines, print the line from \`a\`, then the delimiter, then the line from \`b\`, then a newline.

\`\`\`c
void my_paste(const char *a, const char *b, char delim) {
    while (*a || *b) {
        while (*a && *a != '\\n') { putchar(*a); a++; }
        putchar(delim);
        while (*b && *b != '\\n') { putchar(*b); b++; }
        putchar('\\n');
        if (*a == '\\n') a++;
        if (*b == '\\n') b++;
    }
}
\`\`\`

### Two Pointers, One Loop

Unlike previous tools that operate on a single string, \`paste\` walks two strings at once. The \`while (*a || *b)\` condition continues as long as either string still has content — so if one is longer, its extra lines still appear (with an empty other side).

### Your Task

Implement \`my_paste\` that merges lines from \`a\` and \`b\` with the given delimiter.`,

	starterCode: `#include <stdio.h>

void my_paste(const char *a, const char *b, char delim) {
\t// Merge lines from a and b with delim
}

int main() {
\tmy_paste("alice\\nbob\\ncarol\\n", "30\\n25\\n35\\n", ':');
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_paste(const char *a, const char *b, char delim) {
\twhile (*a || *b) {
\t\twhile (*a && *a != '\\n') { putchar(*a); a++; }
\t\tputchar(delim);
\t\twhile (*b && *b != '\\n') { putchar(*b); b++; }
\t\tputchar('\\n');
\t\tif (*a == '\\n') a++;
\t\tif (*b == '\\n') b++;
\t}
}

int main() {
\tmy_paste("alice\\nbob\\ncarol\\n", "30\\n25\\n35\\n", ':');
\treturn 0;
}
`,

	tests: [
		{
			name: "merges names and ages with colon",
			expected: "alice:30\nbob:25\ncarol:35\n",
		},
		{
			name: "merges with tab delimiter",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_paste("one\\ntwo\\n", "1\\n2\\n", '\\t');
\treturn 0;
}`,
			expected: "one\t1\ntwo\t2\n",
		},
		{
			name: "merges with comma",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_paste("a\\nb\\nc\\n", "x\\ny\\nz\\n", ',');
\treturn 0;
}`,
			expected: "a,x\nb,y\nc,z\n",
		},
		{
			name: "single line merge",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_paste("hello\\n", "world\\n", '-');
\treturn 0;
}`,
			expected: "hello-world\n",
		},
	],
};
