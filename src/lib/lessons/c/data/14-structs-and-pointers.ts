import type { Lesson } from "../../types";

export const structsAndPointers: Lesson = {
	id: "structs-and-pointers",
	title: "Structs and Pointers",
	chapterId: "structs",
	content: `## Structs and Pointers

Passing structs by pointer is more efficient than copying them, and allows functions to modify the original struct.

### Pointer to Struct

\`\`\`c
struct Point {
    int x;
    int y;
};

struct Point p = {10, 20};
struct Point *pp = &p;
\`\`\`

### The Arrow Operator

Use \`->\` to access members through a pointer (equivalent to \`(*pp).x\`):

\`\`\`c
printf("%d\\n", pp->x);    // 10
printf("%d\\n", pp->y);    // 20
pp->x = 99;               // modify through pointer
\`\`\`

### Modifying Structs in Functions

\`\`\`c
void move(struct Point *p, int dx, int dy) {
    p->x += dx;
    p->y += dy;
}

int main() {
    struct Point p = {0, 0};
    move(&p, 5, 3);
    printf("(%d, %d)\\n", p.x, p.y);  // (5, 3)
}
\`\`\`

### Arrays of Structs

\`\`\`c
struct Point points[3] = {{1, 2}, {3, 4}, {5, 6}};
for (int i = 0; i < 3; i++) {
    printf("(%d, %d)\\n", points[i].x, points[i].y);
}
\`\`\`

### Your Task

Define a struct \`Counter\` with an \`int value\` field. Write two functions:
- \`void increment(struct Counter *c)\` -- increases value by 1
- \`void add(struct Counter *c, int n)\` -- increases value by n

Start with a counter at 0, increment it 3 times, then add 10. Print the final value.`,

	starterCode: `#include <stdio.h>

struct Counter {
\tint value;
};

void increment(struct Counter *c) {
\t// Increase c->value by 1
}

void add(struct Counter *c, int n) {
\t// Increase c->value by n
}

int main() {
\tstruct Counter c;
\tc.value = 0;
\tincrement(&c);
\tincrement(&c);
\tincrement(&c);
\tadd(&c, 10);
\tprintf("%d\\n", c.value);
\treturn 0;
}
`,

	solution: `#include <stdio.h>

struct Counter {
\tint value;
};

void increment(struct Counter *c) {
\tc->value++;
}

void add(struct Counter *c, int n) {
\tc->value += n;
}

int main() {
\tstruct Counter c;
\tc.value = 0;
\tincrement(&c);
\tincrement(&c);
\tincrement(&c);
\tadd(&c, 10);
\tprintf("%d\\n", c.value);
\treturn 0;
}
`,

	tests: [
		{
			name: "counter reaches 13",
			expected: "13\n",
		},
	],
};
