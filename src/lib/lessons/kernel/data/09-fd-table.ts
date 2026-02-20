import type { Lesson } from "../../types";

export const fdTable: Lesson = {
	id: "fd-table",
	title: "File Descriptor Table",
	chapterId: "filesystem",
	content: `## File Descriptor Table

Every process has a **file descriptor table** — an array where each index is a file descriptor (fd) and each slot points to an open file. FDs 0, 1, and 2 are pre-assigned:

| fd | name   | meaning        |
|----|--------|----------------|
| 0  | stdin  | standard input |
| 1  | stdout | standard output |
| 2  | stderr | standard error |

When you call \`open()\`, the kernel scans the table for the **lowest free slot** starting at 3 and returns that fd. \`close()\` marks the slot free again.

### Your Implementation

\`\`\`c
typedef struct {
    char name[32];
    int  open;
} FDEntry;

int my_open(FDEntry table[], const char *name) {
    for (int fd = 3; fd < 16; fd++) {
        if (!table[fd].open) {
            table[fd].open = 1;
            char *d = table[fd].name;
            while (*name) *d++ = *name++;
            *d = '\\0';
            return fd;
        }
    }
    return -1; // EMFILE — too many open files
}

void my_close(FDEntry table[], int fd) {
    if (fd >= 0 && fd < 16) table[fd].open = 0;
}

const char *my_getname(FDEntry table[], int fd) {
    if (fd >= 0 && fd < 16 && table[fd].open) return table[fd].name;
    return NULL;
}
\`\`\`

### Your Task

Implement \`my_open\`, \`my_close\`, and \`my_getname\` for a 16-entry file descriptor table (fds 0–15, with 0/1/2 pre-reserved).`,

	starterCode: `#include <stdio.h>

typedef struct {
\tchar name[32];
\tint  open;
} FDEntry;

int my_open(FDEntry table[], const char *name) {
\t// Find lowest free fd >= 3, mark open, copy name, return fd
\treturn -1;
}

void my_close(FDEntry table[], int fd) {
\t// Mark fd as closed
}

const char *my_getname(FDEntry table[], int fd) {
\t// Return the filename for fd, or NULL if not open
\treturn 0;
}

int main() {
\tFDEntry table[16] = {};
\ttable[0].open = table[1].open = table[2].open = 1;
\tint a = my_open(table, "/etc/passwd");
\tint b = my_open(table, "/etc/hosts");
\tprintf("%d %d\\n", a, b);
\tmy_close(table, a);
\tint c = my_open(table, "/etc/resolv.conf");
\tprintf("%d\\n", c);
\tprintf("%s\\n", my_getname(table, b));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

typedef struct {
\tchar name[32];
\tint  open;
} FDEntry;

int my_open(FDEntry table[], const char *name) {
\tfor (int fd = 3; fd < 16; fd++) {
\t\tif (!table[fd].open) {
\t\t\ttable[fd].open = 1;
\t\t\tchar *d = table[fd].name;
\t\t\twhile (*name) *d++ = *name++;
\t\t\t*d = '\\0';
\t\t\treturn fd;
\t\t}
\t}
\treturn -1;
}

void my_close(FDEntry table[], int fd) {
\tif (fd >= 0 && fd < 16) table[fd].open = 0;
}

const char *my_getname(FDEntry table[], int fd) {
\tif (fd >= 0 && fd < 16 && table[fd].open) return table[fd].name;
\treturn 0;
}

int main() {
\tFDEntry table[16] = {};
\ttable[0].open = table[1].open = table[2].open = 1;
\tint a = my_open(table, "/etc/passwd");
\tint b = my_open(table, "/etc/hosts");
\tprintf("%d %d\\n", a, b);
\tmy_close(table, a);
\tint c = my_open(table, "/etc/resolv.conf");
\tprintf("%d\\n", c);
\tprintf("%s\\n", my_getname(table, b));
\treturn 0;
}
`,

	tests: [
		{
			name: "fds start at 3, closed fd is reused",
			expected: "3 4\n3\n/etc/hosts\n",
		},
		{
			name: "getname returns NULL for closed fd",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tFDEntry table[16] = {};
\ttable[0].open = table[1].open = table[2].open = 1;
\tint fd = my_open(table, "test.txt");
\tmy_close(table, fd);
\tprintf("%d\\n", my_getname(table, fd) == 0);
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "table full returns -1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tFDEntry table[16] = {};
\tfor (int i = 0; i < 16; i++) table[i].open = 1;
\tprintf("%d\\n", my_open(table, "x"));
\treturn 0;
}`,
			expected: "-1\n",
		},
		{
			name: "sequential opens get sequential fds",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tFDEntry table[16] = {};
\ttable[0].open = table[1].open = table[2].open = 1;
\tfor (int i = 0; i < 4; i++) printf("%d\\n", my_open(table, "f"));
\treturn 0;
}`,
			expected: "3\n4\n5\n6\n",
		},
	],
};
