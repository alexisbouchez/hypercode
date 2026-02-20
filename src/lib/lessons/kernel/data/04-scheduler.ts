import type { Lesson } from "../../types";

export const scheduler: Lesson = {
	id: "scheduler",
	title: "Round-Robin Scheduler",
	chapterId: "processes",
	content: `## The Linux Scheduler

The Linux kernel scheduler decides which READY process gets to run next. The original scheduler (pre-2.6) used a simple **round-robin** approach: give each process one time slice, then move to the next.

Modern Linux uses CFS (Completely Fair Scheduler), but round-robin is still used for real-time processes and is the foundation all schedulers build on.

### Round-Robin Algorithm

\`\`\`
[P1, P2, P3] all READY
tick 0: run P1 → advance to P2
tick 1: run P2 → advance to P3
tick 2: run P3 → advance to P1
tick 3: run P1 → ...
\`\`\`

Key rules:
- Only schedule READY processes (skip WAITING or ZOMBIE)
- If no process is ready, print "idle"
- Maintain a cursor that advances after each scheduled process

### Your Implementation

Write \`void schedule(PCB procs[], int n, int ticks)\` that simulates \`ticks\` time slices:

\`\`\`c
void schedule(PCB procs[], int n, int ticks) {
    int cur = 0;
    for (int t = 0; t < ticks; t++) {
        int found = 0;
        for (int i = 0; i < n; i++) {
            if (procs[cur].state == READY) {
                printf("tick %d: %s\\n", t, procs[cur].name);
                cur = (cur + 1) % n;
                found = 1;
                break;
            }
            cur = (cur + 1) % n;
        }
        if (!found) printf("tick %d: idle\\n", t);
    }
}
\`\`\`

### Your Task

Implement \`schedule\` that simulates round-robin scheduling over the given process array.`,

	starterCode: `#include <stdio.h>

#define READY   1
#define RUNNING 2
#define WAITING 3

typedef struct {
\tint  pid;
\tint  state;
\tchar name[32];
\tint  priority;
} PCB;

void schedule(PCB procs[], int n, int ticks) {
\t// Round-robin: print "tick N: name" or "tick N: idle"
}

int main() {
\tPCB procs[3] = {
\t\t{1, READY, "init",  0},
\t\t{2, READY, "bash",  20},
\t\t{3, READY, "nginx", 10},
\t};
\tschedule(procs, 3, 6);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

#define READY   1
#define RUNNING 2
#define WAITING 3

typedef struct {
\tint  pid;
\tint  state;
\tchar name[32];
\tint  priority;
} PCB;

void schedule(PCB procs[], int n, int ticks) {
\tint cur = 0;
\tfor (int t = 0; t < ticks; t++) {
\t\tint found = 0;
\t\tfor (int i = 0; i < n; i++) {
\t\t\tif (procs[cur].state == READY) {
\t\t\t\tprintf("tick %d: %s\\n", t, procs[cur].name);
\t\t\t\tcur = (cur + 1) % n;
\t\t\t\tfound = 1;
\t\t\t\tbreak;
\t\t\t}
\t\t\tcur = (cur + 1) % n;
\t\t}
\t\tif (!found) printf("tick %d: idle\\n", t);
\t}
}

int main() {
\tPCB procs[3] = {
\t\t{1, READY, "init",  0},
\t\t{2, READY, "bash",  20},
\t\t{3, READY, "nginx", 10},
\t};
\tschedule(procs, 3, 6);
\treturn 0;
}
`,

	tests: [
		{
			name: "3 ready processes cycle round-robin",
			expected: "tick 0: init\ntick 1: bash\ntick 2: nginx\ntick 3: init\ntick 4: bash\ntick 5: nginx\n",
		},
		{
			name: "waiting process is skipped",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tPCB procs[3] = {
\t\t{1, READY,   "init", 0},
\t\t{2, WAITING, "bash", 0},
\t\t{3, READY,   "vim",  0},
\t};
\tschedule(procs, 3, 4);
\treturn 0;
}`,
			expected: "tick 0: init\ntick 1: vim\ntick 2: init\ntick 3: vim\n",
		},
		{
			name: "all waiting prints idle",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tPCB procs[2] = {
\t\t{1, WAITING, "a", 0},
\t\t{2, WAITING, "b", 0},
\t};
\tschedule(procs, 2, 3);
\treturn 0;
}`,
			expected: "tick 0: idle\ntick 1: idle\ntick 2: idle\n",
		},
		{
			name: "single process runs every tick",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tPCB procs[1] = {{1, READY, "solo", 0}};
\tschedule(procs, 1, 3);
\treturn 0;
}`,
			expected: "tick 0: solo\ntick 1: solo\ntick 2: solo\n",
		},
	],
};
