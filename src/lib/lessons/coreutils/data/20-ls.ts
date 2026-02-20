import type { Lesson } from "../../types";

export const ls: Lesson = {
	id: "ls",
	title: "ls",
	chapterId: "filtering",
	content: `## The \`ls\` Command

\`ls\` lists the contents of a directory, sorted alphabetically. By default it **skips hidden files** — those whose names begin with a dot:

\`\`\`bash
$ ls
Makefile  README.md  src  tests
\`\`\`

Files like \`.git\` and \`.env\` are present on disk but not shown without the \`-a\` flag.

### Your Implementation

Write \`void my_ls(const char *entries)\` that takes a newline-separated list of filenames (unsorted, may contain hidden files), skips any that start with \`.\`, sorts the rest, and prints them one per line.

This combines two operations you have already built:

1. **Filter** — skip entries whose first character is \`.\`
2. **Sort** — print the remaining entries in alphabetical order

\`\`\`c
void my_ls(const char *entries) {
    char lines[64][256];
    int count = 0;

    // Parse lines, skipping hidden files
    while (*entries) {
        char *out = lines[count];
        const char *start = entries;
        while (*entries && *entries != '\\n') { *out++ = *entries++; }
        *out = '\\0';
        if (*entries == '\\n') entries++;
        if (start[0] != '.') count++;  // keep only non-hidden
    }

    // Bubble sort
    ...

    for (int i = 0; i < count; i++) printf("%s\\n", lines[i]);
}
\`\`\`

### Your Task

Implement \`my_ls\` that filters hidden entries and prints the rest sorted alphabetically.`,

	starterCode: `#include <stdio.h>

void my_ls(const char *entries) {
\t// Skip hidden files (starting with '.'), sort the rest, print one per line
}

int main() {
\tmy_ls("README.md\\n.git\\nsrc\\n.env\\nMakefile\\n");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_ls(const char *entries) {
\tchar lines[64][256];
\tint count = 0;

\twhile (*entries) {
\t\tchar *out = lines[count];
\t\tconst char *start = entries;
\t\twhile (*entries && *entries != '\\n') { *out++ = *entries++; }
\t\t*out = '\\0';
\t\tif (*entries == '\\n') entries++;
\t\tif (start[0] != '.') count++;
\t}

\tfor (int i = 0; i < count - 1; i++) {
\t\tfor (int j = 0; j < count - 1 - i; j++) {
\t\t\tconst char *a = lines[j], *b = lines[j + 1];
\t\t\twhile (*a && *a == *b) { a++; b++; }
\t\t\tif (*a > *b) {
\t\t\t\tchar tmp[256];
\t\t\t\tchar *t = tmp; const char *c = lines[j];
\t\t\t\twhile (*c) *t++ = *c++; *t = '\\0';
\t\t\t\tt = lines[j]; c = lines[j + 1];
\t\t\t\twhile (*c) *t++ = *c++; *t = '\\0';
\t\t\t\tt = lines[j + 1]; c = tmp;
\t\t\t\twhile (*c) *t++ = *c++; *t = '\\0';
\t\t\t}
\t\t}
\t}

\tfor (int i = 0; i < count; i++) printf("%s\\n", lines[i]);
}

int main() {
\tmy_ls("README.md\\n.git\\nsrc\\n.env\\nMakefile\\n");
\treturn 0;
}
`,

	tests: [
		{
			name: "filters hidden files and sorts",
			expected: "Makefile\nREADME.md\nsrc\n",
		},
		{
			name: "no hidden files — just sorts",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_ls("zebra\\napple\\nmango\\n");
\treturn 0;
}`,
			expected: "apple\nmango\nzebra\n",
		},
		{
			name: "all hidden — prints nothing",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_ls(".git\\n.env\\n.DS_Store\\n");
\treturn 0;
}`,
			expected: "",
		},
		{
			name: "mixed hidden and visible",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_ls("main.c\\n.gitignore\\nlib.c\\n.env\\nREADME.md\\n");
\treturn 0;
}`,
			expected: "README.md\nlib.c\nmain.c\n",
		},
	],
};
