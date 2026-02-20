import type { Lesson } from "../../types";

export const semaphore: Lesson = {
	id: "semaphore",
	title: "Semaphore",
	chapterId: "ipc",
	content: `## The Semaphore

A **semaphore** is a synchronization primitive used throughout the Linux kernel — for protecting shared resources, signalling between processes, and limiting concurrency.

A counting semaphore has an integer value:

- **\`sem_down\`** (also called \`P\` or \`wait\`): decrement the value. If it was already 0, the caller **blocks** — it waits until another process calls \`sem_up\`.
- **\`sem_up\`** (also called \`V\` or \`signal\`): increment the value. If any processes are blocked, wake one of them.

\`\`\`c
typedef struct { int value; } Semaphore;

// Returns 1 if acquired, 0 if would block
int sem_down(Semaphore *s) {
    if (s->value > 0) { s->value--; return 1; }
    return 0;
}

void sem_up(Semaphore *s) { s->value++; }
\`\`\`

Since we cannot actually block in our simulation, \`sem_down\` returns 1 if it acquired the semaphore, or 0 if it would have blocked.

### Binary vs Counting Semaphores

- **Binary** (value: 0 or 1) — acts like a mutex: only one holder at a time
- **Counting** (value: N) — allows up to N concurrent holders

Linux uses both: \`mutex_lock\`/\`mutex_unlock\` for mutual exclusion, \`down\`/\`up\` in the VFS for I/O synchronization.

### Your Task

Implement \`sem_down\` (returns 1 if acquired, 0 if blocked) and \`sem_up\`.`,

	starterCode: `#include <stdio.h>

typedef struct {
\tint value;
} Semaphore;

int sem_down(Semaphore *s) {
\t// Decrement if > 0 and return 1; return 0 if would block
\treturn 0;
}

void sem_up(Semaphore *s) {
\t// Increment
}

int main() {
\tSemaphore s = {2};
\tprintf("%d\\n", sem_down(&s));
\tprintf("%d\\n", sem_down(&s));
\tprintf("%d\\n", sem_down(&s));
\tsem_up(&s);
\tprintf("%d\\n", sem_down(&s));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

typedef struct {
\tint value;
} Semaphore;

int sem_down(Semaphore *s) {
\tif (s->value > 0) { s->value--; return 1; }
\treturn 0;
}

void sem_up(Semaphore *s) {
\ts->value++;
}

int main() {
\tSemaphore s = {2};
\tprintf("%d\\n", sem_down(&s));
\tprintf("%d\\n", sem_down(&s));
\tprintf("%d\\n", sem_down(&s));
\tsem_up(&s);
\tprintf("%d\\n", sem_down(&s));
\treturn 0;
}
`,

	tests: [
		{
			name: "count-2 semaphore: two acquires succeed, third blocks, up then acquire",
			expected: "1\n1\n0\n1\n",
		},
		{
			name: "binary semaphore (value 1)",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tSemaphore s = {1};
\tprintf("%d\\n", sem_down(&s));
\tprintf("%d\\n", sem_down(&s));
\tsem_up(&s);
\tprintf("%d\\n", sem_down(&s));
\treturn 0;
}`,
			expected: "1\n0\n1\n",
		},
		{
			name: "sem_up releases blocked waiter",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tSemaphore s = {0};
\tprintf("%d\\n", sem_down(&s));
\tsem_up(&s);
\tprintf("%d\\n", sem_down(&s));
\treturn 0;
}`,
			expected: "0\n1\n",
		},
		{
			name: "value never goes negative",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tSemaphore s = {1};
\tsem_down(&s);
\tsem_down(&s);
\tsem_down(&s);
\tprintf("%d\\n", s.value);
\treturn 0;
}`,
			expected: "0\n",
		},
	],
};
