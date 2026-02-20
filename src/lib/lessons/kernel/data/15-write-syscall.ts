import type { Lesson } from "../../types";

export const writeSyscall: Lesson = {
	id: "write-syscall",
	title: "sys_write",
	chapterId: "syscalls",
	content: `## The write() System Call

\`write(fd, buf, count)\` is syscall number 1 on Linux x86-64. It writes \`count\` bytes from \`buf\` to the file descriptor \`fd\`. The return value is the number of bytes written, or \`-1\` on error.

\`\`\`c
ssize_t write(int fd, const void *buf, size_t count);
\`\`\`

In the kernel, \`sys_write\` dispatches to the file's \`write\` method via the VFS layer. For file descriptors 1 (stdout) and 2 (stderr), it ends up in the terminal driver which sends bytes to the screen.

### Your Implementation

Write \`int sys_write(int fd, const char *buf, int len)\`:
- If \`fd\` is 1 (stdout) or 2 (stderr): write all bytes with \`putchar\`, return \`len\`
- Otherwise: return \`-1\` (EBADF — bad file descriptor)

\`\`\`c
int sys_write(int fd, const char *buf, int len) {
    if (fd == 1 || fd == 2) {
        for (int i = 0; i < len; i++) putchar(buf[i]);
        return len;
    }
    return -1;
}
\`\`\`

### Why Separate fd 1 and 2?

Real \`write\` does not treat stdout and stderr differently at the syscall level — both go to their respective file descriptions. We treat them identically because in our simulation both write to the same output stream.

### Your Task

Implement \`sys_write\` that writes to stdout/stderr (fd 1/2) and returns -1 for all other file descriptors.`,

	starterCode: `#include <stdio.h>

#define STDOUT 1
#define STDERR 2

int sys_write(int fd, const char *buf, int len) {
\t// Write len bytes to stdout/stderr; return len or -1
\treturn -1;
}

int main() {
\tint n = sys_write(STDOUT, "hello\\n", 6);
\tprintf("%d\\n", n);
\tprintf("%d\\n", sys_write(0, "x", 1));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

#define STDOUT 1
#define STDERR 2

int sys_write(int fd, const char *buf, int len) {
\tif (fd == STDOUT || fd == STDERR) {
\t\tfor (int i = 0; i < len; i++) putchar(buf[i]);
\t\treturn len;
\t}
\treturn -1;
}

int main() {
\tint n = sys_write(STDOUT, "hello\\n", 6);
\tprintf("%d\\n", n);
\tint m = sys_write(0, "x", 1);
\tprintf("%d\\n", m);
\treturn 0;
}
`,

	tests: [
		{
			name: "write to stdout returns byte count",
			expected: "hello\n6\n-1\n",
		},
		{
			name: "write to stderr also works",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", sys_write(STDERR, "err\\n", 4));
\treturn 0;
}`,
			expected: "err\n4\n",
		},
		{
			name: "write to stdin (fd 0) returns -1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint r = sys_write(0, "x", 1);
\tprintf("%d\\n", r);
\treturn 0;
}`,
			expected: "-1\n",
		},
		{
			name: "write to arbitrary fd returns -1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint r = sys_write(5, "data", 4);
\tprintf("%d\\n", r);
\treturn 0;
}`,
			expected: "-1\n",
		},
	],
};
