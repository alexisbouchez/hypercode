import type { Lesson } from "../../types";

export const switchStatement: Lesson = {
	id: "switch",
	title: "Switch",
	chapterId: "control-flow",
	content: `## Switch

The \`switch\` statement selects one of many code blocks to execute based on the value of an expression.

### Basic Syntax

\`\`\`c
int day = 3;
switch (day) {
    case 1:
        printf("Monday\\n");
        break;
    case 2:
        printf("Tuesday\\n");
        break;
    case 3:
        printf("Wednesday\\n");
        break;
    default:
        printf("Other\\n");
        break;
}
\`\`\`

> Bridge stations: each crew member mans a different console. Helm, navigation, tactical -- switch to the right station for the job.

### The break Statement

Each \`case\` must end with \`break\`, otherwise execution **falls through** to the next case:

\`\`\`c
int x = 1;
switch (x) {
    case 1:
        printf("one\\n");
        // no break -- falls through!
    case 2:
        printf("two\\n");
        break;
}
// prints: one
//         two
\`\`\`

### Fall-through on Purpose

Sometimes fall-through is intentional, to handle multiple cases the same way:

\`\`\`c
switch (day) {
    case 6:
    case 7:
        printf("Weekend\\n");
        break;
    default:
        printf("Weekday\\n");
        break;
}
\`\`\`

### default

The \`default\` case runs when no other case matches. It is optional but recommended.

### Switch vs if-else

Use \`switch\` when comparing a single value against multiple constants. Use \`if-else\` for ranges or complex conditions.

### Your Task

Write a function \`const char *day_type(int day)\` that returns \`"weekend"\` if day is 6 or 7, and \`"weekday"\` otherwise (1-5). Print the result for days 1, 6, and 7.`,

	starterCode: `#include <stdio.h>

const char *day_type(int day) {
\t// Return "weekend" for 6 or 7, "weekday" otherwise
\treturn "";
}

int main() {
\tprintf("%s\\n", day_type(1));
\tprintf("%s\\n", day_type(6));
\tprintf("%s\\n", day_type(7));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

const char *day_type(int day) {
\tswitch (day) {
\t\tcase 6:
\t\tcase 7:
\t\t\treturn "weekend";
\t\tdefault:
\t\t\treturn "weekday";
\t}
}

int main() {
\tprintf("%s\\n", day_type(1));
\tprintf("%s\\n", day_type(6));
\tprintf("%s\\n", day_type(7));
\treturn 0;
}
`,

	tests: [
		{
			name: "days 1, 6, 7",
			expected: "weekday\nweekend\nweekend\n",
		},
		{
			name: "day 1 is weekday",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%s\\n", day_type(1));
\treturn 0;
}`,
			expected: "weekday\n",
		},
		{
			name: "day 5 is weekday",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%s\\n", day_type(5));
\treturn 0;
}`,
			expected: "weekday\n",
		},
		{
			name: "day 6 is weekend",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%s\\n", day_type(6));
\treturn 0;
}`,
			expected: "weekend\n",
		},
		{
			name: "day 7 is weekend",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%s\\n", day_type(7));
\treturn 0;
}`,
			expected: "weekend\n",
		},
	],
};
