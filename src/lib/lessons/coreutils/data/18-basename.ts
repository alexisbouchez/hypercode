import type { Lesson } from "../../types";

export const basename: Lesson = {
	id: "basename",
	title: "basename",
	chapterId: "filtering",
	content: `## The \`basename\` Command

\`basename\` strips the directory prefix from a path, leaving just the filename:

\`\`\`bash
$ basename /usr/bin/ls
ls
$ basename ./src/main.c
main.c
\`\`\`

Everything up to and including the last \`/\` is discarded.

### Your Implementation

Write \`void my_basename(const char *path)\` that prints the filename component.

The algorithm: scan the entire string tracking the position right after the last \`/\`. At the end, print from that position.

\`\`\`c
void my_basename(const char *path) {
    const char *last = path;
    for (const char *p = path; *p; p++)
        if (*p == '/') last = p + 1;
    printf("%s\\n", last);
}
\`\`\`

Start \`last\` pointing at the beginning of the string. Each time a \`/\` is found, advance \`last\` to the character after it. When the loop ends, \`last\` points at the start of the final component.

### Edge Cases

- \`/usr/bin/ls\` → \`ls\` (normal case)
- \`ls\` → \`ls\` (no slash — the whole string is the basename)
- \`/\` → empty string (trailing slash after root — real \`basename\` returns \`/\`, but we keep it simple)

### Your Task

Implement \`my_basename\` that prints the last path component.`,

	starterCode: `#include <stdio.h>

void my_basename(const char *path) {
\t// Print the filename component of path
}

int main() {
\tmy_basename("/usr/bin/ls");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_basename(const char *path) {
\tconst char *last = path;
\tfor (const char *p = path; *p; p++)
\t\tif (*p == '/') last = p + 1;
\tprintf("%s\\n", last);
}

int main() {
\tmy_basename("/usr/bin/ls");
\treturn 0;
}
`,

	tests: [
		{
			name: "strips full path",
			expected: "ls\n",
		},
		{
			name: "strips single directory",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_basename("./src/main.c");
\treturn 0;
}`,
			expected: "main.c\n",
		},
		{
			name: "no slash returns the whole string",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_basename("README.md");
\treturn 0;
}`,
			expected: "README.md\n",
		},
		{
			name: "deeply nested path",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_basename("/a/b/c/d/e");
\treturn 0;
}`,
			expected: "e\n",
		},
	],
};
