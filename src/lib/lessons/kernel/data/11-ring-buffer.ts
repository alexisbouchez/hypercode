import type { Lesson } from "../../types";

export const ringBuffer: Lesson = {
	id: "ring-buffer",
	title: "Ring Buffer",
	chapterId: "ipc",
	content: `## The Ring Buffer

A **ring buffer** (circular buffer) is the data structure at the heart of Linux pipes. It is a fixed-size array where write and read positions wrap around:

\`\`\`
buf:  [ h | e | l | l | o |   |   |   ]
       ↑                   ↑
      head              tail
      (read here)      (write here)
\`\`\`

- **write**: store byte at \`tail\`, advance \`tail = (tail + 1) % size\`
- **read**: load byte from \`head\`, advance \`head = (head + 1) % size\`
- **full**: \`count == size\`
- **empty**: \`count == 0\`

### Your Implementation

\`\`\`c
#define RB_SIZE 8

typedef struct {
    char buf[RB_SIZE];
    int  head, tail, count;
} RingBuf;

int rb_write(RingBuf *rb, char c) {
    if (rb->count == RB_SIZE) return 0;
    rb->buf[rb->tail] = c;
    rb->tail = (rb->tail + 1) % RB_SIZE;
    rb->count++;
    return 1;
}

int rb_read(RingBuf *rb, char *c) {
    if (rb->count == 0) return 0;
    *c = rb->buf[rb->head];
    rb->head = (rb->head + 1) % RB_SIZE;
    rb->count--;
    return 1;
}
\`\`\`

### Your Task

Implement \`rb_write\` and \`rb_read\`. Also write \`rb_drain\` that reads and prints all remaining bytes.`,

	starterCode: `#include <stdio.h>

#define RB_SIZE 8

typedef struct {
\tchar buf[RB_SIZE];
\tint  head, tail, count;
} RingBuf;

int rb_write(RingBuf *rb, char c) {
\t// Write c if space available; return 1 on success, 0 if full
\treturn 0;
}

int rb_read(RingBuf *rb, char *c) {
\t// Read one byte into *c; return 1 on success, 0 if empty
\treturn 0;
}

void rb_drain(RingBuf *rb) {
\tchar c;
\twhile (rb_read(rb, &c)) putchar(c);
}

int main() {
\tRingBuf rb = {};
\tconst char *msg = "hello";
\tfor (int i = 0; msg[i]; i++) rb_write(&rb, msg[i]);
\trb_drain(&rb);
\tputchar('\\n');
\treturn 0;
}
`,

	solution: `#include <stdio.h>

#define RB_SIZE 8

typedef struct {
\tchar buf[RB_SIZE];
\tint  head, tail, count;
} RingBuf;

int rb_write(RingBuf *rb, char c) {
\tif (rb->count == RB_SIZE) return 0;
\trb->buf[rb->tail] = c;
\trb->tail = (rb->tail + 1) % RB_SIZE;
\trb->count++;
\treturn 1;
}

int rb_read(RingBuf *rb, char *c) {
\tif (rb->count == 0) return 0;
\t*c = rb->buf[rb->head];
\trb->head = (rb->head + 1) % RB_SIZE;
\trb->count--;
\treturn 1;
}

void rb_drain(RingBuf *rb) {
\tchar c;
\twhile (rb_read(rb, &c)) putchar(c);
}

int main() {
\tRingBuf rb = {};
\tconst char *msg = "hello";
\tfor (int i = 0; msg[i]; i++) rb_write(&rb, msg[i]);
\trb_drain(&rb);
\tputchar('\\n');
\treturn 0;
}
`,

	tests: [
		{
			name: "write then drain",
			expected: "hello\n",
		},
		{
			name: "write returns 0 when full",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tRingBuf rb = {};
\tint ok = 1;
\tfor (int i = 0; i < RB_SIZE; i++) ok = rb_write(&rb, 'x');
\tprintf("%d\\n", ok);
\tprintf("%d\\n", rb_write(&rb, 'y'));
\treturn 0;
}`,
			expected: "1\n0\n",
		},
		{
			name: "interleaved write and read",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tRingBuf rb = {};
\trb_write(&rb, 'a');
\trb_write(&rb, 'b');
\trb_write(&rb, 'c');
\tchar c;
\trb_read(&rb, &c); putchar(c);
\trb_write(&rb, 'd');
\trb_drain(&rb);
\tputchar('\\n');
\treturn 0;
}`,
			expected: "abcd\n",
		},
		{
			name: "read from empty returns 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tRingBuf rb = {};
\tchar c;
\tprintf("%d\\n", rb_read(&rb, &c));
\treturn 0;
}`,
			expected: "0\n",
		},
	],
};
