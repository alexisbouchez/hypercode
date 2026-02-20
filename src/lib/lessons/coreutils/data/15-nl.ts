import type { Lesson } from "../../types";

export const nl: Lesson = {
	id: "nl",
	title: "nl",
	chapterId: "transformation",
	content: `## The \`nl\` Command

\`nl\` numbers the lines of its input:

\`\`\`bash
$ printf "apple\\nbanana\\ncherry\\n" | nl
     1\tapple
     2\tbanana
     3\tcherry
\`\`\`

Each line is prefixed with a right-justified line number in a 6-character field, followed by a tab. The real \`nl\` uses \`%6d\\t\` — our version will match that format by computing the padding manually.

### Your Implementation

Write \`void my_nl(const char *s)\` that numbers and prints lines.

To right-justify a number in 6 characters:
1. Count its digits
2. Print \`6 - digits\` spaces
3. Print the number with \`printf("%d\\t", n)\`

\`\`\`c
void my_nl(const char *s) {
    int n = 1;
    while (*s) {
        // Count digits of n
        int tmp = n, digits = 0;
        do { digits++; tmp /= 10; } while (tmp);
        // Pad to width 6
        for (int i = digits; i < 6; i++) putchar(' ');
        printf("%d\\t", n++);
        while (*s && *s != '\\n') { putchar(*s); s++; }
        putchar('\\n');
        if (*s == '\\n') s++;
    }
}
\`\`\`

### Counting Digits

The \`do { digits++; tmp /= 10; } while (tmp);\` loop counts digits by repeatedly dividing by 10. A \`do…while\` is used so that 0 is counted as 1 digit.

### Your Task

Implement \`my_nl\` that prints each line prefixed with its right-justified line number in a 6-character field followed by a tab.`,

	starterCode: `#include <stdio.h>

void my_nl(const char *s) {
\t// Number each line, right-justified in 6 chars, followed by a tab
}

int main() {
\tmy_nl("apple\\nbanana\\ncherry\\n");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_nl(const char *s) {
\tint n = 1;
\twhile (*s) {
\t\tint tmp = n, digits = 0;
\t\tdo { digits++; tmp /= 10; } while (tmp);
\t\tfor (int i = digits; i < 6; i++) putchar(' ');
\t\tprintf("%d\\t", n++);
\t\twhile (*s && *s != '\\n') { putchar(*s); s++; }
\t\tputchar('\\n');
\t\tif (*s == '\\n') s++;
\t}
}

int main() {
\tmy_nl("apple\\nbanana\\ncherry\\n");
\treturn 0;
}
`,

	tests: [
		{
			name: "numbers three lines",
			expected: "     1\tapple\n     2\tbanana\n     3\tcherry\n",
		},
		{
			name: "single line gets number 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_nl("hello\\n");
\treturn 0;
}`,
			expected: "     1\thello\n",
		},
		{
			name: "two lines numbered correctly",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_nl("foo\\nbar\\n");
\treturn 0;
}`,
			expected: "     1\tfoo\n     2\tbar\n",
		},
		{
			name: "numbers ten lines",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_nl("a\\nb\\nc\\nd\\ne\\nf\\ng\\nh\\ni\\nj\\n");
\treturn 0;
}`,
			expected: "     1\ta\n     2\tb\n     3\tc\n     4\td\n     5\te\n     6\tf\n     7\tg\n     8\th\n     9\ti\n    10\tj\n",
		},
	],
};
