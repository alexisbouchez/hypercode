import type { Lesson } from "../../types";

export const structs: Lesson = {
	id: "structs",
	title: "Structs",
	chapterId: "structs",
	content: `## Structs

A \`struct\` groups related variables together into a single type. Each variable inside a struct is called a **member** or **field**.

### Definition

\`\`\`c
struct Point {
    int x;
    int y;
};
\`\`\`

### Declaration and Initialization

\`\`\`c
struct Point p1 = {10, 20};
struct Point p2 = {.x = 30, .y = 40};  // designated initializers
\`\`\`

### Accessing Members

Use the dot operator \`.\`:

\`\`\`c
printf("(%d, %d)\\n", p1.x, p1.y);  // (10, 20)
p1.x = 99;
\`\`\`

### Structs as Function Parameters

Structs are passed by value (copied) when passed to functions:

\`\`\`c
void print_point(struct Point p) {
    printf("(%d, %d)\\n", p.x, p.y);
}
\`\`\`

### Returning Structs

Functions can return structs:

\`\`\`c
struct Point make_point(int x, int y) {
    struct Point p;
    p.x = x;
    p.y = y;
    return p;
}
\`\`\`

### typedef

You can use \`typedef\` to avoid writing \`struct\` every time:

\`\`\`c
typedef struct {
    int x;
    int y;
} Point;

Point p = {10, 20};  // no need to write "struct"
\`\`\`

### Your Task

Define a struct \`Rectangle\` with \`int width\` and \`int height\`. Write a function \`int area(struct Rectangle r)\` that returns the area. Print the area of a 5x3 rectangle.`,

	starterCode: `#include <stdio.h>

// Define struct Rectangle here

// Write int area(struct Rectangle r) here

int main() {
\tstruct Rectangle r;
\tr.width = 5;
\tr.height = 3;
\tprintf("%d\\n", area(r));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

struct Rectangle {
\tint width;
\tint height;
};

int area(struct Rectangle r) {
\treturn r.width * r.height;
}

int main() {
\tstruct Rectangle r;
\tr.width = 5;
\tr.height = 3;
\tprintf("%d\\n", area(r));
\treturn 0;
}
`,

	tests: [
		{
			name: "computes rectangle area",
			expected: "15\n",
		},
		{
			name: "area(1, 1) = 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tstruct Rectangle r;
\tr.width = 1;
\tr.height = 1;
\tprintf("%d\\n", area(r));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "area(10, 20) = 200",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tstruct Rectangle r;
\tr.width = 10;
\tr.height = 20;
\tprintf("%d\\n", area(r));
\treturn 0;
}`,
			expected: "200\n",
		},
		{
			name: "area(7, 0) = 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tstruct Rectangle r;
\tr.width = 7;
\tr.height = 0;
\tprintf("%d\\n", area(r));
\treturn 0;
}`,
			expected: "0\n",
		},
	],
};
