import type { Lesson } from "../../types";

export const processStates: Lesson = {
	id: "process-states",
	title: "Process State Machine",
	chapterId: "processes",
	content: `## Process States

A Linux process moves through a well-defined set of states during its lifetime:

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

- **NEW → READY**: process created, waiting for its first time slice
- **READY → RUNNING**: scheduler selects the process
- **RUNNING → WAITING**: process blocks on I/O or a lock
- **WAITING → READY**: I/O completes, lock released
- **RUNNING → ZOMBIE**: process calls \`exit()\`

### Events

\`\`\`c
#define SCHEDULE 0
#define WAIT     1
#define WAKE     2
#define EXIT     3
\`\`\`

### Your Implementation

Write \`int transition(int state, int event)\` that returns the new state for a given state/event pair. For invalid combinations (e.g., ZOMBIE + SCHEDULE), return the current state unchanged.

\`\`\`c
int transition(int state, int event) {
    if (state == NEW     && event == SCHEDULE) return READY;
    if (state == READY   && event == SCHEDULE) return RUNNING;
    if (state == RUNNING && event == WAIT)     return WAITING;
    if (state == RUNNING && event == EXIT)     return ZOMBIE;
    if (state == WAITING && event == WAKE)     return READY;
    return state;
}
\`\`\`

### Your Task

Implement \`transition\` and \`trace\` — which takes an array of events, starts from a given state, and prints each new state name.`,

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
			name: "invalid transition stays in same state",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint events[] = {EXIT, WAKE};
\ttrace(ZOMBIE, events, 2);
\treturn 0;
}`,
			expected: "ZOMBIE\nZOMBIE\n",
		},
		{
			name: "READY to RUNNING",
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
			name: "wait and wake cycle",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tint events[] = {WAIT, WAKE, SCHEDULE};
\ttrace(RUNNING, events, 3);
\treturn 0;
}`,
			expected: "WAITING\nREADY\nRUNNING\n",
		},
	],
};
