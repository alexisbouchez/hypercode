import type { Lesson } from "../../types";

export const uniq: Lesson = {
	id: "uniq",
	title: "uniq",
	chapterId: "transformation",
	content: `## The \`uniq\` Command

\`uniq\` removes **consecutive duplicate lines**. Only adjacent duplicates are removed — non-adjacent duplicates are kept:

\`\`\`bash
$ printf "apple\\napple\\nbanana\\nbanana\\napple\\n" | uniq
apple
banana
apple
\`\`\`

The first two \`apple\` lines collapse into one. The final \`apple\` stays because it is not adjacent to another \`apple\`.

### Your Implementation

Write \`void my_uniq(const char *s)\` that prints deduplicated lines.

You need to remember the **previous line** and compare it to the current one. If they are different, print the current line. If they are the same, skip it.

\`\`\`c
void my_uniq(const char *s) {
    char prev[256];
    char line[256];
    prev[0] = '\\0';

    while (*s) {
        char *out = line;
        while (*s && *s != '\\n') { *out++ = *s++; }
        *out = '\\0';
        if (*s == '\\n') s++;

        // Compare line with prev using pointer arithmetic
        const char *a = line, *b = prev;
        while (*a && *a == *b) { a++; b++; }
        if (*a != *b) {
            printf("%s\\n", line);
            // Copy line into prev
            char *d = prev;
            const char *c = line;
            while (*c) *d++ = *c++;
            *d = '\\0';
        }
    }
}
\`\`\`

### Pointer-based String Compare and Copy

Instead of \`strcmp\` and \`strcpy\` from \`<string.h>\`, we compare and copy with pointer loops:

- **Compare**: walk both pointers together until a mismatch or end. If the final characters differ, the strings differ.
- **Copy**: walk \`src\` and write each character to \`dest\` until the null terminator.

This is exactly what \`strcmp\` and \`strcpy\` do internally — writing them yourself is great practice.

### Your Task

Implement \`my_uniq\` that removes consecutive duplicate lines.`,

	starterCode: `#include <stdio.h>

void my_uniq(const char *s) {
\t// Remove consecutive duplicate lines
}

int main() {
\tmy_uniq("apple\\napple\\nbanana\\nbanana\\napple\\n");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_uniq(const char *s) {
\tchar prev[256];
\tchar line[256];
\tprev[0] = '\\0';

\twhile (*s) {
\t\tchar *out = line;
\t\twhile (*s && *s != '\\n') { *out++ = *s++; }
\t\t*out = '\\0';
\t\tif (*s == '\\n') s++;

\t\tconst char *a = line, *b = prev;
\t\twhile (*a && *a == *b) { a++; b++; }
\t\tif (*a != *b) {
\t\t\tprintf("%s\\n", line);
\t\t\tchar *d = prev;
\t\t\tconst char *c = line;
\t\t\twhile (*c) *d++ = *c++;
\t\t\t*d = '\\0';
\t\t}
\t}
}

int main() {
\tmy_uniq("apple\\napple\\nbanana\\nbanana\\napple\\n");
\treturn 0;
}
`,

	tests: [
		{
			name: "removes consecutive duplicates",
			expected: "apple\nbanana\napple\n",
		},
		{
			name: "no duplicates passes through",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_uniq("a\\nb\\nc\\n");
\treturn 0;
}`,
			expected: "a\nb\nc\n",
		},
		{
			name: "all same collapses to one",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_uniq("x\\nx\\nx\\n");
\treturn 0;
}`,
			expected: "x\n",
		},
		{
			name: "alternating keeps all",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_uniq("a\\nb\\na\\nb\\n");
\treturn 0;
}`,
			expected: "a\nb\na\nb\n",
		},
	],
};
