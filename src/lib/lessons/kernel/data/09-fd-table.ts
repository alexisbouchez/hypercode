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

### dup and dup2

Two important system calls duplicate file descriptors:

- **\`dup(oldfd)\`** — duplicates \`oldfd\` into the **lowest available** fd. Both fds now refer to the same file.
- **\`dup2(oldfd, newfd)\`** — duplicates \`oldfd\` into exactly \`newfd\`. If \`newfd\` is already open, it is silently closed first. If \`oldfd == newfd\`, return \`newfd\` without doing anything.

This is how shells implement I/O redirection:
\`\`\`
// Redirect stdout to a file:
int fd = open("out.txt", ...);
dup2(fd, 1);    // fd 1 (stdout) now points to "out.txt"
close(fd);      // original fd no longer needed
\`\`\`

### Your Task

Implement \`my_open\`, \`my_close\`, \`my_getname\`, \`my_dup\`, and \`my_dup2\` for a 16-entry file descriptor table (fds 0–15, with 0/1/2 pre-reserved).`,

	starterCode: `#include <stdio.h>

typedef struct {
\tchar name[32];
\tint  open;
} FDEntry;

void copy_name(FDEntry *entry, const char *name) {
\tchar *d = entry->name;
\twhile (*name) *d++ = *name++;
\t*d = '\\0';
}

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

int my_dup(FDEntry table[], int oldfd) {
\t// Duplicate oldfd into lowest available fd
\t// Return new fd, or -1 on error
\treturn -1;
}

int my_dup2(FDEntry table[], int oldfd, int newfd) {
\t// Duplicate oldfd into exactly newfd
\t// If oldfd == newfd, return newfd
\t// If newfd is open, close it first
\t// Return newfd on success, -1 on error
\treturn -1;
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
\tint d = my_dup(table, b);
\tprintf("%d %s\\n", d, my_getname(table, d));
\tint e = my_dup2(table, c, 5);
\tprintf("%d %s\\n", e, my_getname(table, e));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

typedef struct {
\tchar name[32];
\tint  open;
} FDEntry;

void copy_name(FDEntry *entry, const char *name) {
\tchar *d = entry->name;
\twhile (*name) *d++ = *name++;
\t*d = '\\0';
}

int my_open(FDEntry table[], const char *name) {
\tfor (int fd = 3; fd < 16; fd++) {
\t\tif (!table[fd].open) {
\t\t\ttable[fd].open = 1;
\t\t\tcopy_name(&table[fd], name);
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

int my_dup(FDEntry table[], int oldfd) {
\tif (oldfd < 0 || oldfd >= 16 || !table[oldfd].open) return -1;
\tfor (int fd = 0; fd < 16; fd++) {
\t\tif (!table[fd].open) {
\t\t\ttable[fd].open = 1;
\t\t\tcopy_name(&table[fd], table[oldfd].name);
\t\t\treturn fd;
\t\t}
\t}
\treturn -1;
}

int my_dup2(FDEntry table[], int oldfd, int newfd) {
\tif (oldfd < 0 || oldfd >= 16 || !table[oldfd].open) return -1;
\tif (newfd < 0 || newfd >= 16) return -1;
\tif (oldfd == newfd) return newfd;
\tif (table[newfd].open) table[newfd].open = 0;
\ttable[newfd].open = 1;
\tcopy_name(&table[newfd], table[oldfd].name);
\treturn newfd;
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
\tint d = my_dup(table, b);
\tprintf("%d %s\\n", d, my_getname(table, d));
\tint e = my_dup2(table, c, 5);
\tprintf("%d %s\\n", e, my_getname(table, e));
\treturn 0;
}
`,

	tests: [
		{
			name: "open, close, reopen, dup, dup2",
			expected: "3 4\n3\n/etc/hosts\n5 /etc/hosts\n5 /etc/resolv.conf\n",
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
\tint r = my_open(table, "x");
\tprintf("%d\\n", r);
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
		{
			name: "close and reopen reuses lowest available fd",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tFDEntry table[16] = {};
\ttable[0].open = table[1].open = table[2].open = 1;
\tmy_open(table, "a");
\tmy_open(table, "b");
\tmy_open(table, "c");
\tmy_close(table, 4);
\tmy_close(table, 3);
\tprintf("%d\\n", my_open(table, "d"));
\treturn 0;
}`,
			expected: "3\n",
		},
		{
			name: "dup uses lowest available fd",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tFDEntry table[16] = {};
\ttable[0].open = table[1].open = table[2].open = 1;
\tint a = my_open(table, "log.txt");
\tint b = my_dup(table, a);
\tprintf("%d %d\\n", a, b);
\tprintf("%s\\n", my_getname(table, b));
\treturn 0;
}`,
			expected: "3 4\nlog.txt\n",
		},
		{
			name: "dup fills lowest gap",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tFDEntry table[16] = {};
\tfor (int i = 0; i < 6; i++) table[i].open = 1;
\tmy_close(table, 2);
\tint fd = my_dup(table, 3);
\tprintf("%d\\n", fd);
\treturn 0;
}`,
			expected: "2\n",
		},
		{
			name: "dup of invalid fd returns -1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tFDEntry table[16] = {};
\tprintf("%d\\n", my_dup(table, 5));
\treturn 0;
}`,
			expected: "-1\n",
		},
		{
			name: "dup2 targets exact fd",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tFDEntry table[16] = {};
\ttable[0].open = table[1].open = table[2].open = 1;
\tint a = my_open(table, "out.txt");
\tint r = my_dup2(table, a, 1);
\tprintf("%d\\n", r);
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "dup2 closes existing fd at target",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tFDEntry table[16] = {};
\ttable[0].open = table[1].open = table[2].open = 1;
\tint a = my_open(table, "file_a");
\tint b = my_open(table, "file_b");
\tmy_dup2(table, a, b);
\tprintf("%s\\n", my_getname(table, b));
\treturn 0;
}`,
			expected: "file_a\n",
		},
		{
			name: "dup2 with oldfd == newfd is no-op",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tFDEntry table[16] = {};
\ttable[0].open = table[1].open = table[2].open = 1;
\tint a = my_open(table, "same.txt");
\tint r = my_dup2(table, a, a);
\tprintf("%d %s\\n", r, my_getname(table, r));
\treturn 0;
}`,
			expected: "3 same.txt\n",
		},
		{
			name: "dup2 with invalid oldfd returns -1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tFDEntry table[16] = {};
\tprintf("%d\\n", my_dup2(table, 10, 3));
\treturn 0;
}`,
			expected: "-1\n",
		},
	],
};
