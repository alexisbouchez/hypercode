import type { Lesson } from "../../types";

export const processStates: Lesson = {
	id: "process-states",
	title: "Process State Machine",
	chapterId: "processes",
	content: `## Process States

A Linux process moves through a well-defined set of states during its lifetime. There are exactly **5 states** and **5 valid transitions**:

\`\`\`
          SCHEDULE
NEW ──────────────► READY ◄──── WAKE ────┐
                      │                   │
                 SCHEDULE                  │
                      │                   │
                      ▼                   │
                  RUNNING ──── WAIT ──► WAITING
                      │
                     EXIT
                      │
                      ▼
                   ZOMBIE
\`\`\`

### The 5 Valid Transitions

| # | From | Event | To | When |
|---|------|-------|----|------|
| 1 | NEW | SCHEDULE | READY | Process created, admitted to ready queue |
| 2 | READY | SCHEDULE | RUNNING | Scheduler dispatches the process to CPU |
| 3 | RUNNING | WAIT | WAITING | Process blocks on I/O, lock, or sleep |
| 4 | WAITING | WAKE | READY | I/O completes or lock is released |
| 5 | RUNNING | EXIT | ZOMBIE | Process calls \`exit()\` or returns from main |

**ZOMBIE** is a terminal state — no event can move a process out of it. The process stays in ZOMBIE until its parent calls \`wait()\` to collect its exit status.

Any event that doesn't match one of the 5 transitions above is **invalid** and the state remains unchanged. For example, a READY process receiving WAIT is a no-op — only a RUNNING process can block.

### Events

\`\`\`c
#define SCHEDULE 0   // Scheduler picks a process
#define WAIT     1   // Process blocks (I/O, lock)
#define WAKE     2   // Blocked process is unblocked
#define EXIT     3   // Process terminates
\`\`\`

### Your Task

Implement \`transition(state, event)\` that returns the new state, and \`trace(start, events[], n)\` that prints each state name after applying each event in sequence.`,

	starterCode: `#include <stdio.h>

#define NEW      0
#define READY    1
#define RUNNING  2
#define WAITING  3
#define ZOMBIE   4

#define SCHEDULE 0
#define WAIT     1
#define WAKE     2
#define EXIT     3

const char *state_name(int s) {
\tif (s == NEW)     return "NEW";
\tif (s == READY)   return "READY";
\tif (s == RUNNING) return "RUNNING";
\tif (s == WAITING) return "WAITING";
\tif (s == ZOMBIE)  return "ZOMBIE";
\treturn "UNKNOWN";
}

int transition(int state, int event) {
\t// Return new state based on state + event
\treturn state;
}

void trace(int start, int events[], int n) {
\tint s = start;
\tfor (int i = 0; i < n; i++) {
\t\ts = transition(s, events[i]);
\t\tprintf("%s\\n", state_name(s));
\t}
}

int main() {
\tint events[] = {SCHEDULE, SCHEDULE, WAIT, WAKE, SCHEDULE, EXIT};
\ttrace(NEW, events, 6);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

#define NEW      0
#define READY    1
#define RUNNING  2
#define WAITING  3
#define ZOMBIE   4

#define SCHEDULE 0
#define WAIT     1
#define WAKE     2
#define EXIT     3

const char *state_name(int s) {
\tif (s == NEW)     return "NEW";
\tif (s == READY)   return "READY";
\tif (s == RUNNING) return "RUNNING";
\tif (s == WAITING) return "WAITING";
\tif (s == ZOMBIE)  return "ZOMBIE";
\treturn "UNKNOWN";
}

int transition(int state, int event) {
\tif (state == NEW     && event == SCHEDULE) return READY;
\tif (state == READY   && event == SCHEDULE) return RUNNING;
\tif (state == RUNNING && event == WAIT)     return WAITING;
\tif (state == RUNNING && event == EXIT)     return ZOMBIE;
\tif (state == WAITING && event == WAKE)     return READY;
\treturn state;
}

void trace(int start, int events[], int n) {
\tint s = start;
\tfor (int i = 0; i < n; i++) {
\t\ts = transition(s, events[i]);
\t\tprintf("%s\\n", state_name(s));
\t}
}

int main() {
\tint events[] = {SCHEDULE, SCHEDULE, WAIT, WAKE, SCHEDULE, EXIT};
\ttrace(NEW, events, 6);
\treturn 0;
}
`,

	tests: [
		{
			name: "full lifecycle trace",
			expected: "READY\nRUNNING\nWAITING\nREADY\nRUNNING\nZOMBIE\n",
		},
		{
			name: "transition 1: NEW -> READY on SCHEDULE",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint events[] = {SCHEDULE};
\ttrace(NEW, events, 1);
\treturn 0;
}`,
			expected: "READY\n",
		},
		{
			name: "transition 2: READY -> RUNNING on SCHEDULE",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint events[] = {SCHEDULE};
\ttrace(READY, events, 1);
\treturn 0;
}`,
			expected: "RUNNING\n",
		},
		{
			name: "transition 3: RUNNING -> WAITING on WAIT",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint events[] = {WAIT};
\ttrace(RUNNING, events, 1);
\treturn 0;
}`,
			expected: "WAITING\n",
		},
		{
			name: "transition 4: WAITING -> READY on WAKE",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint events[] = {WAKE};
\ttrace(WAITING, events, 1);
\treturn 0;
}`,
			expected: "READY\n",
		},
		{
			name: "transition 5: RUNNING -> ZOMBIE on EXIT",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint events[] = {EXIT};
\ttrace(RUNNING, events, 1);
\treturn 0;
}`,
			expected: "ZOMBIE\n",
		},
		{
			name: "ZOMBIE is terminal — all events ignored",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint events[] = {SCHEDULE, WAIT, WAKE, EXIT};
\ttrace(ZOMBIE, events, 4);
\treturn 0;
}`,
			expected: "ZOMBIE\nZOMBIE\nZOMBIE\nZOMBIE\n",
		},
		{
			name: "NEW ignores invalid events before SCHEDULE",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint events[] = {WAIT, EXIT, WAKE, SCHEDULE};
\ttrace(NEW, events, 4);
\treturn 0;
}`,
			expected: "NEW\nNEW\nNEW\nREADY\n",
		},
		{
			name: "READY ignores WAIT, WAKE, EXIT",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint events[] = {WAIT, WAKE, EXIT};
\ttrace(READY, events, 3);
\treturn 0;
}`,
			expected: "READY\nREADY\nREADY\n",
		},
		{
			name: "RUNNING ignores SCHEDULE and WAKE",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint events[] = {SCHEDULE, WAKE};
\ttrace(RUNNING, events, 2);
\treturn 0;
}`,
			expected: "RUNNING\nRUNNING\n",
		},
		{
			name: "WAITING ignores SCHEDULE, WAIT, EXIT",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint events[] = {SCHEDULE, WAIT, EXIT};
\ttrace(WAITING, events, 3);
\treturn 0;
}`,
			expected: "WAITING\nWAITING\nWAITING\n",
		},
		{
			name: "repeated wait/wake cycles return to RUNNING",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint events[] = {WAIT, WAKE, SCHEDULE, WAIT, WAKE, SCHEDULE, EXIT};
\ttrace(RUNNING, events, 7);
\treturn 0;
}`,
			expected: "WAITING\nREADY\nRUNNING\nWAITING\nREADY\nRUNNING\nZOMBIE\n",
		},
	],
};
