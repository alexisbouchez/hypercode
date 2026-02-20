import type { Lesson } from "../../types";

export const toupper: Lesson = {
	id: "toupper",
	title: "toupper",
	chapterId: "transformation",
	content: `## Uppercasing Text

There is no standard \`toupper\` coreutil, but it is a classic C exercise that shows how character manipulation works at the ASCII level.

The task: print a string with every lowercase letter converted to uppercase. All other characters (spaces, digits, punctuation, newlines) pass through unchanged.

### ASCII and Character Arithmetic

In ASCII, the lowercase letters \`a\`–\`z\` have codes 97–122. The uppercase letters \`A\`–\`Z\` have codes 65–90. The difference is exactly 32.

So to uppercase a character:

\`\`\`c
if (c >= 'a' && c <= 'z') c = c - 'a' + 'A';
\`\`\`

Or equivalently: \`c -= 32\` (since \`'a' - 'A' == 32\`). The first form is clearer.

### Your Implementation

\`\`\`c
void to_upper(const char *s) {
    while (*s) {
        char c = *s;
        if (c >= 'a' && c <= 'z') c = c - 'a' + 'A';
        putchar(c);
        s++;
    }
}
\`\`\`

No library functions needed — just arithmetic on character codes.

### The Real tr Command

You can actually do this in the shell with \`tr\`:

\`\`\`bash
$ echo "hello" | tr 'a-z' 'A-Z'
HELLO
\`\`\`

You will implement a simplified version of \`tr\` in the next lesson.

### Your Task

Implement \`to_upper\` that converts lowercase letters to uppercase, passing everything else through unchanged.`,

	starterCode: `#include <stdio.h>

void to_upper(const char *s) {
\t// Print s with lowercase letters converted to uppercase
}

int main() {
\tto_upper("hello, world!\\n");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void to_upper(const char *s) {
\twhile (*s) {
\t\tchar c = *s;
\t\tif (c >= 'a' && c <= 'z') c = c - 'a' + 'A';
\t\tputchar(c);
\t\ts++;
\t}
}

int main() {
\tto_upper("hello, world!\\n");
\treturn 0;
}
`,

	tests: [
		{
			name: "uppercases hello, world!",
			expected: "HELLO, WORLD!\n",
		},
		{
			name: "all caps stays all caps",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tto_upper("LINUX\\n");
\treturn 0;
}`,
			expected: "LINUX\n",
		},
		{
			name: "mixed case",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tto_upper("Hello, C!\\n");
\treturn 0;
}`,
			expected: "HELLO, C!\n",
		},
		{
			name: "preserves non-letter characters",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tto_upper("abc123xyz\\n");
\treturn 0;
}`,
			expected: "ABC123XYZ\n",
		},
	],
};
