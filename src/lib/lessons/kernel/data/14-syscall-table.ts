import type { Lesson } from "../../types";

export const syscallTable: Lesson = {
	id: "syscall-table",
	title: "Syscall Table",
	chapterId: "syscalls",
	content: `## The System Call Table

When a user program calls \`read()\` or \`write()\`, it triggers a software interrupt that transfers control to the kernel. The kernel looks up the syscall number in the **syscall table** — a global array of function pointers — and dispatches to the right handler.

\`\`\`c
// From arch/x86/entry/syscalls/syscall_64.tbl (abbreviated)
// 0  read
// 1  write
// 2  open
// 3  close
// ...
\`\`\`

On x86-64, the syscall number is in \`rax\` and the kernel uses it as an index into \`sys_call_table[]\`.

### Your Implementation

\`\`\`c
typedef void (*syscall_fn)(void);
#define NR_SYSCALLS 16

void register_syscall(syscall_fn table[], int nr, syscall_fn fn) {
    if (nr >= 0 && nr < NR_SYSCALLS) table[nr] = fn;
}

void do_syscall(syscall_fn table[], int nr) {
    if (nr >= 0 && nr < NR_SYSCALLS && table[nr])
        table[nr]();
    else
        printf("unknown syscall %d\\n", nr);
}
\`\`\`

### Your Task

Implement \`register_syscall\` and \`do_syscall\` for a 16-entry syscall table.`,

	starterCode: `#include <stdio.h>

typedef void (*syscall_fn)(void);
#define NR_SYSCALLS 16

void register_syscall(syscall_fn table[], int nr, syscall_fn fn) {
\t// Register fn at table[nr]
}

void do_syscall(syscall_fn table[], int nr) {
\t// Dispatch to table[nr], or print "unknown syscall N"
}

void sys_read(void)  { printf("sys_read\\n"); }
void sys_write(void) { printf("sys_write\\n"); }
void sys_open(void)  { printf("sys_open\\n"); }

int main() {
\tsyscall_fn table[NR_SYSCALLS] = {};
\tregister_syscall(table, 0, sys_read);
\tregister_syscall(table, 1, sys_write);
\tregister_syscall(table, 2, sys_open);
\tdo_syscall(table, 1);
\tdo_syscall(table, 0);
\tdo_syscall(table, 9);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

typedef void (*syscall_fn)(void);
#define NR_SYSCALLS 16

void register_syscall(syscall_fn table[], int nr, syscall_fn fn) {
\tif (nr >= 0 && nr < NR_SYSCALLS) table[nr] = fn;
}

void do_syscall(syscall_fn table[], int nr) {
\tif (nr >= 0 && nr < NR_SYSCALLS && table[nr])
\t\ttable[nr]();
\telse
\t\tprintf("unknown syscall %d\\n", nr);
}

void sys_read(void)  { printf("sys_read\\n"); }
void sys_write(void) { printf("sys_write\\n"); }
void sys_open(void)  { printf("sys_open\\n"); }

int main() {
\tsyscall_fn table[NR_SYSCALLS] = {};
\tregister_syscall(table, 0, sys_read);
\tregister_syscall(table, 1, sys_write);
\tregister_syscall(table, 2, sys_open);
\tdo_syscall(table, 1);
\tdo_syscall(table, 0);
\tdo_syscall(table, 9);
\treturn 0;
}
`,

	tests: [
		{
			name: "dispatches registered syscalls, unknown prints error",
			expected: "sys_write\nsys_read\nunknown syscall 9\n",
		},
		{
			name: "out-of-range number prints unknown",
			code: `#include <stdio.h>
{{FUNC}}
void noop(void) {}
int main() {
\tsyscall_fn table[NR_SYSCALLS] = {};
\tdo_syscall(table, 100);
\treturn 0;
}`,
			expected: "unknown syscall 100\n",
		},
		{
			name: "unregistered slot prints unknown",
			code: `#include <stdio.h>
{{FUNC}}
void noop(void) {}
int main() {
\tsyscall_fn table[NR_SYSCALLS] = {};
\tdo_syscall(table, 5);
\treturn 0;
}`,
			expected: "unknown syscall 5\n",
		},
		{
			name: "registered syscall is dispatched",
			code: `#include <stdio.h>
{{FUNC}}
void my_handler(void) { printf("hello from syscall\\n"); }
int main() {
\tsyscall_fn table[NR_SYSCALLS] = {};
\tregister_syscall(table, 3, my_handler);
\tdo_syscall(table, 3);
\treturn 0;
}`,
			expected: "hello from syscall\n",
		},
	],
};
