import type { Lesson } from "../../types";

export const wcC: Lesson = {
	id: "wc-c",
	title: "wc -c",
	chapterId: "counting",
	content: `## wc -c: Count Characters

\`wc\` (**word count**) counts lines, words, and characters in a file. With \`-c\` it counts bytes (characters):

\`\`\`bash
$ echo "Hello" | wc -c
6
\`\`\`

Six because "Hello" is 5 characters plus the newline \`echo\` appends.

### Your Implementation

Write \`int count_chars(const char *s)\` that returns the total number of characters in the string, including newlines.

\`\`\`c
int count_chars(const char *s) {
    int n = 0;
    while (*s++) n++;
    return n;
}
\`\`\`

This is essentially \`strlen\` — walk the string until the null terminator, counting each step.

### The Null Terminator

C strings end with a \`'\\0'\` byte. The pointer arithmetic \`while (*s++)\` works because:
1. \`*s\` dereferences the pointer to get the current character
2. \`s++\` advances the pointer (but evaluates before incrementing, so we read the character first)
3. When \`*s\` is \`'\\0'\` (zero / false), the loop ends

### Your Task

Implement \`count_chars\` that returns the number of characters in the string.`,

	starterCode: `#include <stdio.h>

int count_chars(const char *s) {
\t// Count all characters (including newlines)
\treturn 0;
}

int main() {
\tprintf("%d\\n", count_chars("Hello\\n"));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int count_chars(const char *s) {
\tint n = 0;
\twhile (*s++) n++;
\treturn n;
}

int main() {
\tprintf("%d\\n", count_chars("Hello\\n"));
\treturn 0;
}
`,

	tests: [
		{
			name: "count_chars(\"Hello\\n\") = 6",
			expected: "6\n",
		},
		{
			name: "count_chars(\"abc\") = 3",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_chars("abc"));
\treturn 0;
}`,
			expected: "3\n",
		},
		{
			name: "count_chars(\"\") = 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_chars(""));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "count_chars with newlines",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_chars("a\\nb\\nc\\n"));
\treturn 0;
}`,
			expected: "6\n",
		},
	],
};
