import type { Lesson } from "../../types";

export const dirname: Lesson = {
	id: "dirname",
	title: "dirname",
	chapterId: "filtering",
	content: `## The \`dirname\` Command

\`dirname\` is the complement of \`basename\` — it prints everything except the last path component:

\`\`\`bash
$ dirname /usr/bin/ls
/usr/bin
$ dirname ./src/main.c
./src
\`\`\`

### Your Implementation

Write \`void my_dirname(const char *path)\` that prints the directory component.

There are three cases to handle:

1. **No slash in path** → print \`.\` (the current directory)
2. **Slash is the first character only** → print \`/\` (root)
3. **Otherwise** → print everything up to (but not including) the last \`/\`

\`\`\`c
void my_dirname(const char *path) {
    int len = 0;
    while (path[len]) len++;

    // Find last slash
    int last = -1;
    for (int i = 0; i < len; i++)
        if (path[i] == '/') last = i;

    if (last == -1) {
        printf(".\\n");          // no slash → current dir
    } else if (last == 0) {
        printf("/\\n");          // leading slash only → root
    } else {
        for (int i = 0; i < last; i++) putchar(path[i]);
        putchar('\\n');
    }
}
\`\`\`

### Why Three Cases?

- \`ls\` has no slash → \`dirname\` returns \`.\` by POSIX convention
- \`/ls\` has a slash only at position 0 → the directory is \`/\` itself
- \`/usr/bin/ls\` → last slash is at index 8 → print chars 0–7: \`/usr/bin\`

### Your Task

Implement \`my_dirname\` that prints the directory portion of a path.`,

	starterCode: `#include <stdio.h>

void my_dirname(const char *path) {
\t// Print the directory component of path
}

int main() {
\tmy_dirname("/usr/bin/ls");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_dirname(const char *path) {
\tint len = 0;
\twhile (path[len]) len++;

\tint last = -1;
\tfor (int i = 0; i < len; i++)
\t\tif (path[i] == '/') last = i;

\tif (last == -1) {
\t\tprintf(".\\n");
\t} else if (last == 0) {
\t\tprintf("/\\n");
\t} else {
\t\tfor (int i = 0; i < last; i++) putchar(path[i]);
\t\tputchar('\\n');
\t}
}

int main() {
\tmy_dirname("/usr/bin/ls");
\treturn 0;
}
`,

	tests: [
		{
			name: "strips filename from full path",
			expected: "/usr/bin\n",
		},
		{
			name: "single directory level",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_dirname("./src/main.c");
\treturn 0;
}`,
			expected: "./src\n",
		},
		{
			name: "no slash returns dot",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_dirname("README.md");
\treturn 0;
}`,
			expected: ".\n",
		},
		{
			name: "root-level file returns slash",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_dirname("/etc");
\treturn 0;
}`,
			expected: "/\n",
		},
	],
};
