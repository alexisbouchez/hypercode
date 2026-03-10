import type { Lesson } from "../../types";

export const unions: Lesson = {
	id: "unions",
	title: "Unions",
	chapterId: "structs",
	content: `## Unions

A \`union\` looks like a struct, but all its members **share the same memory**. The size of a union equals the size of its largest member. Only one member holds a valid value at any time.

### Syntax

\`\`\`c
union Number {
    int i;
    char c;
};
\`\`\`

Both \`i\` and \`c\` occupy the same bytes in memory. Writing to one overwrites the other.

### Unions vs Structs

In a struct, each member has its own storage. In a union, all members overlap:

\`\`\`c
struct S { int a; int b; };  // size = 8 (two ints)
union  U { int a; int b; };  // size = 4 (one int, shared)
\`\`\`

### Basic Usage

\`\`\`c
union Number n;
n.i = 42;
printf("%d\\n", n.i);  // 42

n.c = 'A';
// n.i is now invalid -- only n.c is meaningful
printf("%c\\n", n.c);  // A
\`\`\`

### Tagged Unions

The most important union pattern is the **tagged union** (also called a discriminated union or variant). You pair a union with an enum tag that tracks which member is active:

\`\`\`c
enum ValueType { TYPE_INT, TYPE_CHAR };

struct TaggedValue {
    enum ValueType type;
    union {
        int i;
        char c;
    } data;
};
\`\`\`

This pattern is the foundation of variant types in many languages. You check the tag before accessing the union:

\`\`\`c
void print_value(struct TaggedValue v) {
    if (v.type == TYPE_INT) {
        printf("%d\\n", v.data.i);
    } else {
        printf("%c\\n", v.data.c);
    }
}
\`\`\`

### Your Task

Define an enum \`ShapeType\` with values \`CIRCLE\` and \`RECTANGLE\`. Define a \`struct Shape\` that contains a \`type\` field (enum ShapeType) and a union \`data\` with \`int radius\` for circles and a nested struct with \`int width\` and \`int height\` for rectangles. Write \`int compute_area(struct Shape s)\` that returns \`3 * radius * radius\` for circles (using 3 as an approximation for pi) and \`width * height\` for rectangles. Print the area of a circle with radius 5 and a rectangle 4x6.`,

	starterCode: `#include <stdio.h>

enum ShapeType {
\tCIRCLE,
\tRECTANGLE
};

struct Shape {
\tenum ShapeType type;
\tunion {
\t\tint radius;
\t\tstruct {
\t\t\tint width;
\t\t\tint height;
\t\t} rect;
\t} data;
};

int compute_area(struct Shape s) {
\t// Return 3 * radius * radius for CIRCLE
\t// Return width * height for RECTANGLE
\treturn 0;
}

int main() {
\tstruct Shape c;
\tc.type = CIRCLE;
\tc.data.radius = 5;
\tprintf("%d\\n", compute_area(c));

\tstruct Shape r;
\tr.type = RECTANGLE;
\tr.data.rect.width = 4;
\tr.data.rect.height = 6;
\tprintf("%d\\n", compute_area(r));

\treturn 0;
}
`,

	solution: `#include <stdio.h>

enum ShapeType {
\tCIRCLE,
\tRECTANGLE
};

struct Shape {
\tenum ShapeType type;
\tunion {
\t\tint radius;
\t\tstruct {
\t\t\tint width;
\t\t\tint height;
\t\t} rect;
\t} data;
};

int compute_area(struct Shape s) {
\tif (s.type == CIRCLE) {
\t\treturn 3 * s.data.radius * s.data.radius;
\t}
\treturn s.data.rect.width * s.data.rect.height;
}

int main() {
\tstruct Shape c;
\tc.type = CIRCLE;
\tc.data.radius = 5;
\tprintf("%d\\n", compute_area(c));

\tstruct Shape r;
\tr.type = RECTANGLE;
\tr.data.rect.width = 4;
\tr.data.rect.height = 6;
\tprintf("%d\\n", compute_area(r));

\treturn 0;
}
`,

	tests: [
		{
			name: "circle and rectangle areas",
			expected: "75\n24\n",
		},
		{
			name: "circle with radius 10",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tstruct Shape c;
\tc.type = CIRCLE;
\tc.data.radius = 10;
\tprintf("%d\\n", compute_area(c));
\treturn 0;
}`,
			expected: "300\n",
		},
		{
			name: "rectangle 7x3",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tstruct Shape r;
\tr.type = RECTANGLE;
\tr.data.rect.width = 7;
\tr.data.rect.height = 3;
\tprintf("%d\\n", compute_area(r));
\treturn 0;
}`,
			expected: "21\n",
		},
		{
			name: "circle with radius 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tstruct Shape c;
\tc.type = CIRCLE;
\tc.data.radius = 1;
\tprintf("%d\\n", compute_area(c));
\treturn 0;
}`,
			expected: "3\n",
		},
	],
};
