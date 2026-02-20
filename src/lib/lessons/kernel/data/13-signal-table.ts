import type { Lesson } from "../../types";

export const signalTable: Lesson = {
	id: "signal-table",
	title: "Signal Table",
	chapterId: "ipc",
	content: `## Signals

Signals are the simplest form of IPC in Linux — asynchronous notifications sent to a process. Common signals:

| Number | Name    | Default action |
|--------|---------|----------------|
| 2      | SIGINT  | Terminate (Ctrl+C) |
| 9      | SIGKILL | Terminate (cannot catch) |
| 11     | SIGSEGV | Terminate + core dump |
| 15     | SIGTERM | Graceful terminate |

Each process has a **signal table** — an array of function pointers indexed by signal number. When a signal is delivered, the kernel looks up the handler and calls it.

\`\`\`c
typedef void (*handler_t)(int);
#define NSIG 32

void my_sigaction(handler_t table[], int sig, handler_t h) {
    if (sig >= 0 && sig < NSIG) table[sig] = h;
}

void my_raise(handler_t table[], int sig) {
    if (sig >= 0 && sig < NSIG && table[sig])
        table[sig](sig);
}
\`\`\`

### SIG_DFL and SIG_IGN

Two special handler values:
- **SIG_DFL** (0) — default action (usually terminate)
- **SIG_IGN** — ignore the signal

In our simulation, a NULL handler means SIG_DFL (no-op).

### Your Task

Implement \`my_sigaction\` that registers a handler, and \`my_raise\` that delivers a signal by calling its handler.`,

	starterCode: `#include <stdio.h>

typedef void (*handler_t)(int);
#define NSIG 32
#define SIGINT  2
#define SIGTERM 15

void my_sigaction(handler_t table[], int sig, handler_t h) {
\t// Register handler h for signal sig
}

void my_raise(handler_t table[], int sig) {
\t// Deliver signal sig: call its handler if registered
}

void on_sigterm(int sig) { printf("caught SIGTERM\\n"); }
void on_sigint(int sig)  { printf("caught SIGINT\\n"); }

int main() {
\thandler_t table[NSIG] = {};
\tmy_sigaction(table, SIGTERM, on_sigterm);
\tmy_sigaction(table, SIGINT,  on_sigint);
\tmy_raise(table, SIGTERM);
\tmy_raise(table, SIGINT);
\tmy_raise(table, 9);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

typedef void (*handler_t)(int);
#define NSIG 32
#define SIGINT  2
#define SIGTERM 15

void my_sigaction(handler_t table[], int sig, handler_t h) {
\tif (sig >= 0 && sig < NSIG) table[sig] = h;
}

void my_raise(handler_t table[], int sig) {
\tif (sig >= 0 && sig < NSIG && table[sig])
\t\ttable[sig](sig);
}

void on_sigterm(int sig) { printf("caught SIGTERM\\n"); }
void on_sigint(int sig)  { printf("caught SIGINT\\n"); }

int main() {
\thandler_t table[NSIG] = {};
\tmy_sigaction(table, SIGTERM, on_sigterm);
\tmy_sigaction(table, SIGINT,  on_sigint);
\tmy_raise(table, SIGTERM);
\tmy_raise(table, SIGINT);
\tmy_raise(table, 9);
\treturn 0;
}
`,

	tests: [
		{
			name: "SIGTERM and SIGINT handled, SIGKILL (no handler) ignored",
			expected: "caught SIGTERM\ncaught SIGINT\n",
		},
		{
			name: "handler replaced by new sigaction",
			code: `#include <stdio.h>
{{FUNC}}
void handler_a(int sig) { printf("A\\n"); }
void handler_b(int sig) { printf("B\\n"); }
int main() {
\thandler_t table[NSIG] = {};
\tmy_sigaction(table, SIGTERM, handler_a);
\tmy_raise(table, SIGTERM);
\tmy_sigaction(table, SIGTERM, handler_b);
\tmy_raise(table, SIGTERM);
\treturn 0;
}`,
			expected: "A\nB\n",
		},
		{
			name: "signal number passed to handler",
			code: `#include <stdio.h>
{{FUNC}}
void show_sig(int sig) { printf("signal %d\\n", sig); }
int main() {
\thandler_t table[NSIG] = {};
\tmy_sigaction(table, SIGTERM, show_sig);
\tmy_raise(table, SIGTERM);
\treturn 0;
}`,
			expected: "signal 15\n",
		},
		{
			name: "out-of-range signal does nothing",
			code: `#include <stdio.h>
{{FUNC}}
void boom(int sig) { printf("boom\\n"); }
int main() {
\thandler_t table[NSIG] = {};
\tmy_sigaction(table, 100, boom);
\tmy_raise(table, 100);
\tprintf("ok\\n");
\treturn 0;
}`,
			expected: "ok\n",
		},
	],
};
