import type { Lesson } from "../../types";

export const cut: Lesson = {
	id: "cut",
	title: "cut",
	chapterId: "filtering",
	content: `## The \`cut\` Command

\`cut\` extracts a specific field from each line of delimited text:

\`\`\`bash
$ printf "alice:30:engineer\\nbob:25:designer\\n" | cut -d: -f2
30
25
\`\`\`

\`-d:\` sets the delimiter to \`:\`, and \`-f2\` selects the second field. Fields are 1-indexed.

### Your Implementation

Write \`void my_cut(const char *s, char delim, int field)\` that prints the given field from each line.

The algorithm: for each line, count delimiter occurrences to track which field you are in. When you reach the target field, print its characters.

\`\`\`c
void my_cut(const char *s, char delim, int field) {
    while (*s) {
        int cur = 1;
        while (*s && *s != '\\n') {
            if (*s == delim) {
                cur++;
            } else if (cur == field) {
                putchar(*s);
            }
            s++;
        }
        putchar('\\n');
        if (*s == '\\n') s++;
    }
}
\`\`\`

### Counting Fields

Each time you encounter the delimiter, move to the next field (\`cur++\`). When \`cur == field\`, the current character belongs to the target field — print it. Delimiter characters themselves are never printed.

### Your Task

Implement \`my_cut\` that prints the specified field from each line using the given delimiter.`,

	starterCode: `#include <stdio.h>

void my_cut(const char *s, char delim, int field) {
\t// Print the given field from each line
}

int main() {
\tmy_cut("alice:30:engineer\\nbob:25:designer\\n", ':', 2);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_cut(const char *s, char delim, int field) {
\twhile (*s) {
\t\tint cur = 1;
\t\twhile (*s && *s != '\\n') {
\t\t\tif (*s == delim) {
\t\t\t\tcur++;
\t\t\t} else if (cur == field) {
\t\t\t\tputchar(*s);
\t\t\t}
\t\t\ts++;
\t\t}
\t\tputchar('\\n');
\t\tif (*s == '\\n') s++;
\t}
}

int main() {
\tmy_cut("alice:30:engineer\\nbob:25:designer\\n", ':', 2);
\treturn 0;
}
`,

	tests: [
		{
			name: "cuts second colon-delimited field",
			expected: "30\n25\n",
		},
		{
			name: "cuts first field",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_cut("alice:30:engineer\\nbob:25:designer\\n", ':', 1);
\treturn 0;
}`,
			expected: "alice\nbob\n",
		},
		{
			name: "cuts third field",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_cut("alice:30:engineer\\nbob:25:designer\\n", ':', 3);
\treturn 0;
}`,
			expected: "engineer\ndesigner\n",
		},
		{
			name: "works with comma delimiter",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_cut("one,two,three\\nfour,five,six\\n", ',', 2);
\treturn 0;
}`,
			expected: "two\nfive\n",
		},
	],
};
