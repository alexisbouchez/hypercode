import type { Lesson } from "../../types";

export const grep: Lesson = {
	id: "grep",
	title: "grep",
	chapterId: "filtering",
	content: `## The \`grep\` Command

\`grep\` searches for a pattern in text and prints every line that contains it:

\`\`\`bash
$ grep "Linux" notes.txt
Learn Linux
\`\`\`

Only lines containing "Linux" are printed. Lines without it are discarded.

### Your Implementation

Write \`void my_grep(const char *s, const char *pat)\` that prints every line in \`s\` that contains the substring \`pat\`.

You need two sub-problems:
1. **Split the input into lines** (scan for \`'\\n'\`)
2. **Check if a line contains the pattern** (substring search)

For substring search, use two pointers — one walking the haystack, one walking the needle:

\`\`\`c
int contains(const char *hay, const char *needle) {
    for (; *hay; hay++) {
        const char *h = hay, *n = needle;
        while (*n && *h == *n) { h++; n++; }
        if (!*n) return 1;
    }
    return 0;
}
\`\`\`

For each position in \`hay\`, try to match the full \`needle\` starting there. If \`n\` reaches the null terminator, the full needle matched.

Then collect each line into a buffer with a pointer, check if it contains the pattern, and print it if so.

### Why Nested Loops?

The \`contains\` function is **O(n × m)** where n is the line length and m is the pattern length. For every position in the haystack, it tries to match the full needle. This is the naive algorithm.

Real \`grep\` uses algorithms like **Boyer-Moore** or **Knuth-Morris-Pratt** that are faster in practice, but for learning purposes, the nested loop is perfectly clear.

### Your Task

Implement \`my_grep\` that prints lines containing the given pattern.`,

	starterCode: `#include <stdio.h>

void my_grep(const char *s, const char *pat) {
\t// Print lines in s that contain pat
}

int main() {
\tmy_grep("Learn Linux\\nPractice daily\\nHave fun\\n", "Linux");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int contains(const char *hay, const char *needle) {
\tfor (; *hay; hay++) {
\t\tconst char *h = hay, *n = needle;
\t\twhile (*n && *h == *n) { h++; n++; }
\t\tif (!*n) return 1;
\t}
\treturn 0;
}

void my_grep(const char *s, const char *pat) {
\tchar line[256];
\twhile (*s) {
\t\tchar *out = line;
\t\twhile (*s && *s != '\\n') { *out++ = *s++; }
\t\t*out = '\\0';
\t\tif (*s == '\\n') s++;
\t\tif (contains(line, pat)) printf("%s\\n", line);
\t}
}

int main() {
\tmy_grep("Learn Linux\\nPractice daily\\nHave fun\\n", "Linux");
\treturn 0;
}
`,

	tests: [
		{
			name: "grep finds Linux",
			expected: "Learn Linux\n",
		},
		{
			name: "grep finds daily",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_grep("Learn Linux\\nPractice daily\\nHave fun\\n", "daily");
\treturn 0;
}`,
			expected: "Practice daily\n",
		},
		{
			name: "grep with no match prints nothing",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_grep("hello\\nworld\\n", "xyz");
\treturn 0;
}`,
			expected: "",
		},
		{
			name: "grep finds multiple matching lines",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_grep("foo bar\\nbaz\\nfoo qux\\n", "foo");
\treturn 0;
}`,
			expected: "foo bar\nfoo qux\n",
		},
	],
};
