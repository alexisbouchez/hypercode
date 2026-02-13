import type { Lesson } from "../../types";

export const enums: Lesson = {
	id: "enums",
	title: "Enums",
	chapterId: "enums-and-bitwise",
	content: `## Enums

An \`enum\` (enumeration) defines a set of named integer constants. Enums make code more readable by giving meaningful names to numeric values.

### Declaration

\`\`\`c
enum Color {
    RED,     // 0
    GREEN,   // 1
    BLUE     // 2
};
\`\`\`

By default, the first constant is 0, and each subsequent one increments by 1.

> Starfleet ranks: ENSIGN, LIEUTENANT, COMMANDER, CAPTAIN -- each with a well-defined numeric value.

### Using Enums

\`\`\`c
enum Color c = GREEN;
if (c == GREEN) {
    printf("Green!\\n");
}
\`\`\`

### Custom Values

You can assign specific values:

\`\`\`c
enum HttpStatus {
    OK = 200,
    NOT_FOUND = 404,
    SERVER_ERROR = 500
};
\`\`\`

### Enums in Switch

Enums work naturally with \`switch\`:

\`\`\`c
enum Direction { NORTH, SOUTH, EAST, WEST };

void print_direction(enum Direction d) {
    switch (d) {
        case NORTH: printf("North\\n"); break;
        case SOUTH: printf("South\\n"); break;
        case EAST:  printf("East\\n");  break;
        case WEST:  printf("West\\n");  break;
    }
}
\`\`\`

### Enums are Integers

Under the hood, enums are just integers. You can use them in arithmetic and comparisons:

\`\`\`c
enum Season { SPRING, SUMMER, FALL, WINTER };
int s = SUMMER;  // s == 1
\`\`\`

### Your Task

Define an enum \`Season\` with values \`SPRING\`, \`SUMMER\`, \`FALL\`, \`WINTER\`. Write a function \`int is_warm(enum Season s)\` that returns 1 if the season is \`SPRING\` or \`SUMMER\`, and 0 otherwise. Print the result for all four seasons.`,

	starterCode: `#include <stdio.h>

enum Season {
\tSPRING,
\tSUMMER,
\tFALL,
\tWINTER
};

int is_warm(enum Season s) {
\t// Return 1 for SPRING or SUMMER, 0 otherwise
\treturn 0;
}

int main() {
\tprintf("%d\\n", is_warm(SPRING));
\tprintf("%d\\n", is_warm(SUMMER));
\tprintf("%d\\n", is_warm(FALL));
\tprintf("%d\\n", is_warm(WINTER));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

enum Season {
\tSPRING,
\tSUMMER,
\tFALL,
\tWINTER
};

int is_warm(enum Season s) {
\tif (s == SPRING || s == SUMMER) {
\t\treturn 1;
\t}
\treturn 0;
}

int main() {
\tprintf("%d\\n", is_warm(SPRING));
\tprintf("%d\\n", is_warm(SUMMER));
\tprintf("%d\\n", is_warm(FALL));
\tprintf("%d\\n", is_warm(WINTER));
\treturn 0;
}
`,

	tests: [
		{
			name: "all four seasons",
			expected: "1\n1\n0\n0\n",
		},
		{
			name: "SPRING is warm",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", is_warm(SPRING));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "FALL is not warm",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", is_warm(FALL));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "WINTER is not warm",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", is_warm(WINTER));
\treturn 0;
}`,
			expected: "0\n",
		},
	],
};
