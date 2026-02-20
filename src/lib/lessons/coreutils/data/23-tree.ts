import type { Lesson } from "../../types";

export const tree: Lesson = {
	id: "tree",
	title: "tree",
	chapterId: "output",
	content: `## The \`tree\` Command

\`tree\` prints a directory hierarchy with connectors showing which items belong together:

\`\`\`
.
+-- Makefile
+-- README.md
+-- src
|   +-- lib.c
|   '-- main.c
'-- tests
    '-- test.c
\`\`\`

### Your Implementation

Write \`void my_tree(const char *paths)\` that takes a newline-separated, sorted list of paths and prints them as a tree.

The input uses \`/\` as a separator: \`"src/lib.c"\` means \`lib.c\` lives inside \`src\`. The output is always rooted at \`.\`.

The key insight: **recursive helper function**. \`print_dir\` receives the full path array and finds all direct children of a given prefix, then recurses into any subdirectories.

\`\`\`c
int has_prefix(char lines[][256], int i, const char *pre, int plen) {
    for (int k = 0; k < plen; k++)
        if (lines[i][k] != pre[k]) return 0;
    return 1;
}

void print_dir(char lines[][256], int n,
               const char *parent, int plen, const char *indent) {
    // Collect direct children: start with parent, no '/' in remainder
    int ch[64], cc = 0;
    for (int i = 0; i < n; i++) {
        if (!has_prefix(lines, i, parent, plen)) continue;
        const char *rest = lines[i] + plen;
        int slash = 0;
        while (*rest) { if (*rest == '/') { slash = 1; break; } rest++; }
        if (!slash) ch[cc++] = i;
    }
    for (int ci = 0; ci < cc; ci++) {
        int i = ch[ci];
        int last = (ci == cc - 1);
        printf("%s%s %s\\n", indent, last ? "'--" : "+--", lines[i] + plen);
        // Build child prefix: path + '/'
        char np[256]; int npl = 0;
        while (lines[i][npl]) { np[npl] = lines[i][npl]; npl++; }
        np[npl++] = '/'; np[npl] = '\\0';
        // Recurse if it has children
        int has_ch = 0;
        for (int j = 0; j < n; j++)
            if (has_prefix(lines, j, np, npl)) { has_ch = 1; break; }
        if (has_ch) {
            char ni[256]; int ii = 0;
            while (indent[ii]) { ni[ii] = indent[ii]; ii++; }
            const char *ext = last ? "    " : "|   ";
            while (*ext) ni[ii++] = *ext++;
            ni[ii] = '\\0';
            print_dir(lines, n, np, npl, ni);
        }
    }
}

void my_tree(const char *paths) {
    char lines[64][256];
    int n = 0;
    while (*paths) {
        char *out = lines[n];
        while (*paths && *paths != '\\n') *out++ = *paths++;
        *out = '\\0';
        if (*paths == '\\n') paths++;
        n++;
    }
    printf(".\\n");
    print_dir(lines, n, "", 0, "");
}
\`\`\`

### How It Works

1. **Parse** paths into a local 2D array \`lines[64][256]\`
2. **print_dir** receives the full array and its size, plus the current prefix and indent
3. It finds direct children — entries starting with \`prefix\` that have no extra \`/\` in the remainder
4. For each child: print \`+--\` or \`'--\`, then recurse if it has children, extending the indent with \`"|   "\` (non-last) or \`"    "\` (last)

Passing \`lines\` and \`n\` as parameters (rather than globals) keeps all state local to \`my_tree\`.

### Your Task

Implement \`my_tree\` that prints the path list as a directory tree rooted at \`.\`.`,

	starterCode: `#include <stdio.h>

int has_prefix(char lines[][256], int i, const char *pre, int plen) {
\t// Return 1 if lines[i] starts with pre[0..plen-1]
\treturn 0;
}

void print_dir(char lines[][256], int n,
               const char *parent, int plen, const char *indent) {
\t// Find direct children of parent and print with connectors
}

void my_tree(const char *paths) {
\t// Parse paths into lines[][], print ".", call print_dir
}

int main() {
\tmy_tree("Makefile\\nREADME.md\\nsrc\\nsrc/lib.c\\nsrc/main.c\\ntests\\ntests/test.c\\n");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int has_prefix(char lines[][256], int i, const char *pre, int plen) {
\tfor (int k = 0; k < plen; k++)
\t\tif (lines[i][k] != pre[k]) return 0;
\treturn 1;
}

void print_dir(char lines[][256], int n,
               const char *parent, int plen, const char *indent) {
\tint ch[64], cc = 0;
\tfor (int i = 0; i < n; i++) {
\t\tif (!has_prefix(lines, i, parent, plen)) continue;
\t\tconst char *rest = lines[i] + plen;
\t\tint slash = 0;
\t\twhile (*rest) { if (*rest == '/') { slash = 1; break; } rest++; }
\t\tif (!slash) ch[cc++] = i;
\t}
\tfor (int ci = 0; ci < cc; ci++) {
\t\tint i = ch[ci];
\t\tint last = (ci == cc - 1);
\t\tprintf("%s%s %s\\n", indent, last ? "'--" : "+--", lines[i] + plen);
\t\tchar np[256]; int npl = 0;
\t\twhile (lines[i][npl]) { np[npl] = lines[i][npl]; npl++; }
\t\tnp[npl++] = '/'; np[npl] = '\\0';
\t\tint has_ch = 0;
\t\tfor (int j = 0; j < n; j++)
\t\t\tif (has_prefix(lines, j, np, npl)) { has_ch = 1; break; }
\t\tif (has_ch) {
\t\t\tchar ni[256]; int ii = 0;
\t\t\twhile (indent[ii]) { ni[ii] = indent[ii]; ii++; }
\t\t\tconst char *ext = last ? "    " : "|   ";
\t\t\twhile (*ext) ni[ii++] = *ext++;
\t\t\tni[ii] = '\\0';
\t\t\tprint_dir(lines, n, np, npl, ni);
\t\t}
\t}
}

void my_tree(const char *paths) {
\tchar lines[64][256];
\tint n = 0;
\twhile (*paths) {
\t\tchar *out = lines[n];
\t\twhile (*paths && *paths != '\\n') *out++ = *paths++;
\t\t*out = '\\0';
\t\tif (*paths == '\\n') paths++;
\t\tn++;
\t}
\tprintf(".\\n");
\tprint_dir(lines, n, "", 0, "");
}

int main() {
\tmy_tree("Makefile\\nREADME.md\\nsrc\\nsrc/lib.c\\nsrc/main.c\\ntests\\ntests/test.c\\n");
\treturn 0;
}
`,

	tests: [
		{
			name: "prints two-level tree",
			expected: ".\n+-- Makefile\n+-- README.md\n+-- src\n|   +-- lib.c\n|   '-- main.c\n'-- tests\n    '-- test.c\n",
		},
		{
			name: "flat list with no subdirectories",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tree("a\\nb\\nc\\n");
\treturn 0;
}`,
			expected: ".\n+-- a\n+-- b\n'-- c\n",
		},
		{
			name: "single file",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tree("README.md\\n");
\treturn 0;
}`,
			expected: ".\n'-- README.md\n",
		},
		{
			name: "three-level nesting",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_tree("src\\nsrc/lib\\nsrc/lib/util.c\\nsrc/main.c\\n");
\treturn 0;
}`,
			expected: ".\n'-- src\n    +-- lib\n    |   '-- util.c\n    '-- main.c\n",
		},
	],
};
