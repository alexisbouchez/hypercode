import type { Lesson } from "../../types";

export const pcb: Lesson = {
	id: "pcb",
	title: "Process Control Block",
	chapterId: "processes",
	content: `## The Process Control Block

Every process in Linux is represented by a **Process Control Block** (PCB) — a struct that holds everything the kernel needs to know about a process. In the Linux source, this is called \`task_struct\` and lives in \`include/linux/sched.h\`.

A minimal PCB contains:

\`\`\`c
#define NEW     0
#define READY   1
#define RUNNING 2
#define WAITING 3
#define ZOMBIE  4

typedef struct {
    int  pid;
    int  state;
    char name[32];
    int  priority;
} PCB;
\`\`\`

- **pid** — process ID, unique identifier assigned by the kernel
- **state** — current lifecycle state of the process
- **name** — human-readable name (in Linux: \`comm\`, 16 bytes max)
- **priority** — scheduling priority (0 = highest in Linux's real-time range)

### Your Implementation

Write \`void print_pcb(PCB *p)\` that prints each field on its own line:

\`\`\`
PID:      1
Name:     init
State:    RUNNING
Priority: 0
\`\`\`

Use a helper \`const char *state_name(int s)\` that maps the state integer to its string.

### Your Task

Implement \`print_pcb\` that prints the PCB fields in the format shown above.`,

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
\t// Return the string for each state
\treturn "UNKNOWN";
}

void print_pcb(PCB *p) {
\t// Print pid, name, state, priority
}

int main() {
\tPCB p = {1, RUNNING, "init", 0};
\tprint_pcb(&p);
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

int main() {
\tPCB p = {1, RUNNING, "init", 0};
\tprint_pcb(&p);
\treturn 0;
}
`,

	tests: [
		{
			name: "prints init process",
			expected: "PID:      1\nName:     init\nState:    RUNNING\nPriority: 0\n",
		},
		{
			name: "prints bash process in READY state",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tPCB p = {42, READY, "bash", 20};
\tprint_pcb(&p);
\treturn 0;
}`,
			expected: "PID:      42\nName:     bash\nState:    READY\nPriority: 20\n",
		},
		{
			name: "prints WAITING state",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tPCB p = {7, WAITING, "sleep", 19};
\tprint_pcb(&p);
\treturn 0;
}`,
			expected: "PID:      7\nName:     sleep\nState:    WAITING\nPriority: 19\n",
		},
		{
			name: "prints ZOMBIE state",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tPCB p = {99, ZOMBIE, "defunct", 0};
\tprint_pcb(&p);
\treturn 0;
}`,
			expected: "PID:      99\nName:     defunct\nState:    ZOMBIE\nPriority: 0\n",
		},
	],
};
