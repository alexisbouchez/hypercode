import type { Lesson } from "../../types";

export const fork: Lesson = {
	id: "fork",
	title: "fork()",
	chapterId: "processes",
	content: `## The fork() System Call

\`fork()\` creates a new process by duplicating the calling process. The child is an almost exact copy of the parent — same memory, same open files, same code. Only a few things differ:

- The child gets a **new PID**
- The child's **parent PID** (ppid) is set to the parent's PID
- The child starts in **READY** state (not RUNNING)
- The child's CPU time counters are reset to 0

In the kernel, \`fork()\` calls \`copy_process()\`, which allocates a new \`task_struct\` and copies the parent's fields one by one.

### Your Implementation

Write \`void my_fork(PCB *parent, PCB *child, int new_pid)\` that initializes \`child\` as a copy of \`parent\` with the new PID and READY state.

\`\`\`c
void my_fork(PCB *parent, PCB *child, int new_pid) {
    child->pid      = new_pid;
    child->state    = READY;
    child->priority = parent->priority;
    // copy name
    char *s = parent->name, *d = child->name;
    while (*s) *d++ = *s++;
    *d = '\\0';
}
\`\`\`

After forking, print both parent and child with \`print_pcb\`.

### Your Task

Implement \`my_fork\` that copies the parent PCB into the child, assigning the new PID and setting state to READY.`,

	starterCode: `#include <stdio.h>

#define NEW     0
#define READY   1
#define RUNNING 2
#define WAITING 3
#define ZOMBIE  4

typedef struct {
\tint  pid;
\tint  state;
\tchar name[32];
\tint  priority;
} PCB;

const char *state_name(int s) {
\tif (s == NEW)     return "NEW";
\tif (s == READY)   return "READY";
\tif (s == RUNNING) return "RUNNING";
\tif (s == WAITING) return "WAITING";
\tif (s == ZOMBIE)  return "ZOMBIE";
\treturn "UNKNOWN";
}

void print_pcb(PCB *p) {
\tprintf("PID:      %d\\n", p->pid);
\tprintf("Name:     %s\\n", p->name);
\tprintf("State:    %s\\n", state_name(p->state));
\tprintf("Priority: %d\\n", p->priority);
}

void my_fork(PCB *parent, PCB *child, int new_pid) {
\t// Copy parent into child, set new_pid, set state to READY
}

int main() {
\tPCB parent = {1, RUNNING, "bash", 20};
\tPCB child;
\tmy_fork(&parent, &child, 2);
\tprintf("parent:\\n"); print_pcb(&parent);
\tprintf("child:\\n");  print_pcb(&child);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

#define NEW     0
#define READY   1
#define RUNNING 2
#define WAITING 3
#define ZOMBIE  4

typedef struct {
\tint  pid;
\tint  state;
\tchar name[32];
\tint  priority;
} PCB;

const char *state_name(int s) {
\tif (s == NEW)     return "NEW";
\tif (s == READY)   return "READY";
\tif (s == RUNNING) return "RUNNING";
\tif (s == WAITING) return "WAITING";
\tif (s == ZOMBIE)  return "ZOMBIE";
\treturn "UNKNOWN";
}

void print_pcb(PCB *p) {
\tprintf("PID:      %d\\n", p->pid);
\tprintf("Name:     %s\\n", p->name);
\tprintf("State:    %s\\n", state_name(p->state));
\tprintf("Priority: %d\\n", p->priority);
}

void my_fork(PCB *parent, PCB *child, int new_pid) {
\tchild->pid = new_pid;
\tchild->state = READY;
\tchild->priority = parent->priority;
\tchar *s = parent->name, *d = child->name;
\twhile (*s) *d++ = *s++;
\t*d = '\\0';
}

int main() {
\tPCB parent = {1, RUNNING, "bash", 20};
\tPCB child;
\tmy_fork(&parent, &child, 2);
\tprintf("parent:\\n"); print_pcb(&parent);
\tprintf("child:\\n");  print_pcb(&child);
\treturn 0;
}
`,

	tests: [
		{
			name: "child gets new pid and READY state",
			expected: "parent:\nPID:      1\nName:     bash\nState:    RUNNING\nPriority: 20\nchild:\nPID:      2\nName:     bash\nState:    READY\nPriority: 20\n",
		},
		{
			name: "parent is unchanged after fork",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tPCB parent = {5, RUNNING, "vim", 10};
\tPCB child;
\tmy_fork(&parent, &child, 6);
\tprintf("%d\\n", parent.pid);
\tprintf("%s\\n", state_name(parent.state));
\treturn 0;
}`,
			expected: "5\nRUNNING\n",
		},
		{
			name: "child inherits priority",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tPCB parent = {10, RUNNING, "gcc", 5};
\tPCB child;
\tmy_fork(&parent, &child, 11);
\tprintf("%d\\n", child.priority);
\tprintf("%s\\n", child.name);
\treturn 0;
}`,
			expected: "5\ngcc\n",
		},
		{
			name: "child state is always READY",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tPCB parent = {3, WAITING, "sleep", 19};
\tPCB child;
\tmy_fork(&parent, &child, 4);
\tprintf("%s\\n", state_name(child.state));
\treturn 0;
}`,
			expected: "READY\n",
		},
	],
};
