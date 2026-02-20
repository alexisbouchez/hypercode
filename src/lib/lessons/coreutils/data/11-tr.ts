import type { Lesson } from "../../types";

export const tr: Lesson = {
	id: "tr",
	title: "tr",
	chapterId: "transformation",
	content: `## The \`tr\` Command

\`tr\` (**translate**) maps every character in \`from\` to the corresponding character in \`to\`:

\`\`\`bash
$ echo "hello" | tr 'aeiou' 'AEIOU'
hEllO
$ echo "hello world" | tr 'a-z' 'A-Z'
HELLO WORLD
\`\`\`

Each character in \`from[i]\` is replaced by \`to[i]\`. Characters not in \`from\` pass through unchanged.

### Your Implementation

Write \`void my_tr(const char *s, const char *from, const char *to)\` that translates characters.

For each character in \`s\`, search for it in \`from\`. If found at position \`i\`, print \`to[i]\`. Otherwise print the character as-is.

\`\`\`c
void my_tr(const char *s, const char *from, const char *to) {
    while (*s) {
        char c = *s;
        for (int i = 0; from[i] && to[i]; i++) {
            if (c == from[i]) { c = to[i]; break; }
        }
        putchar(c);
        s++;
    }
}
\`\`\`

### Why a Loop Instead of a Ternary?

The single-char version used \`*s == from ? to : *s\`. With a string mapping you need to search the full \`from\` table for a match. The inner \`for\` loop does this — it walks both \`from\` and \`to\` together, stopping when it finds a match or reaches the end.

### Your Task

Implement \`my_tr\` that translates every character in \`s\` according to the \`from\` → \`to\` mapping.`,

	starterCode: `#include <stdio.h>

void my_tr(const char *s, const char *from, const char *to) {
\t// Replace each character found in from with the corresponding char in to
}

int main() {
\tmy_tr("hello world\\n", "aeiou", "AEIOU");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_tr(const char *s, const char *from, const char *to) {
\twhile (*s) {
\t\tchar c = *s;
\t\tfor (int i = 0; from[i] && to[i]; i++) {
\t\t\tif (c == from[i]) { c = to[i]; break; }
\t\t}
\t\tputchar(c);
\t\ts++;
\t}
}

int main() {
\tmy_tr("hello world\\n", "aeiou", "AEIOU");
\treturn 0;
}
`,

	tests: [
		{
			name: "replaces vowels with uppercase",
			expected: "hEllO wOrld\n",
		},
		{
			name: "replaces spaces with underscores",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tr("hello world\\n", " ", "_");
\treturn 0;
}`,
			expected: "hello_world\n",
		},
		{
			name: "no match passes through",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tr("abcdef\\n", "xyz", "XYZ");
\treturn 0;
}`,
			expected: "abcdef\n",
		},
		{
			name: "maps multiple characters",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tr("banana\\n", "abc", "ABC");
\treturn 0;
}`,
			expected: "BAnAnA\n",
		},
	],
};
