import type { Lesson } from "../../types";

export const sort: Lesson = {
	id: "sort",
	title: "sort",
	chapterId: "transformation",
	content: `## The \`sort\` Command

\`sort\` reads lines and prints them in alphabetical order:

\`\`\`bash
$ printf "banana\\napple\\ncherry\\n" | sort
apple
banana
cherry
\`\`\`

### Your Implementation

Write \`void my_sort(const char *s)\` that prints lines in sorted order.

The approach: first collect every line into a 2D array, then sort that array with **bubble sort**, then print.

\`\`\`c
void my_sort(const char *s) {
    char lines[64][256];
    int count = 0;

    // Collect lines
    while (*s) {
        char *out = lines[count];
        while (*s && *s != '\\n') { *out++ = *s++; }
        *out = '\\0';
        if (*s == '\\n') s++;
        count++;
    }

    // Bubble sort
    for (int i = 0; i < count - 1; i++) {
        for (int j = 0; j < count - 1 - i; j++) {
            const char *a = lines[j], *b = lines[j + 1];
            while (*a && *a == *b) { a++; b++; }
            if (*a > *b) {
                char tmp[256];
                // swap lines[j] and lines[j+1]
                ...
            }
        }
    }

    for (int i = 0; i < count; i++) printf("%s\\n", lines[i]);
}
\`\`\`

### Bubble Sort

Bubble sort repeatedly walks adjacent pairs and swaps them if out of order. After each pass the largest unsorted element "bubbles" to its final position.

**Comparing strings** without \`strcmp\`: walk both pointers together while characters match, then compare the diverging characters. If \`*a > *b\`, line \`a\` sorts after line \`b\`.

**Swapping strings** without \`strcpy\`: copy into a temporary buffer, then overwrite each direction. Since each line is stored as a fixed-size \`char[256]\`, swapping means copying the bytes of one into \`tmp\`, copying the other into the first, then copying \`tmp\` into the second.

### Your Task

Implement \`my_sort\` that prints lines in ascending alphabetical order.`,

	starterCode: `#include <stdio.h>

void my_sort(const char *s) {
\t// Print lines in sorted order
}

int main() {
\tmy_sort("banana\\napple\\ncherry\\n");
\treturn 0;
}
`,

	solution: `#include <stdio.h>

void my_sort(const char *s) {
\tchar lines[64][256];
\tint count = 0;

\twhile (*s) {
\t\tchar *out = lines[count];
\t\twhile (*s && *s != '\\n') { *out++ = *s++; }
\t\t*out = '\\0';
\t\tif (*s == '\\n') s++;
\t\tcount++;
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
\tmy_sort("banana\\napple\\ncherry\\n");
\treturn 0;
}
`,

	tests: [
		{
			name: "sorts alphabetically",
			expected: "apple\nbanana\ncherry\n",
		},
		{
			name: "already sorted stays the same",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_sort("a\\nb\\nc\\n");
\treturn 0;
}`,
			expected: "a\nb\nc\n",
		},
		{
			name: "reverse order gets sorted",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_sort("c\\nb\\na\\n");
\treturn 0;
}`,
			expected: "a\nb\nc\n",
		},
		{
			name: "single line unchanged",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tmy_sort("only\\n");
\treturn 0;
}`,
			expected: "only\n",
		},
	],
};
