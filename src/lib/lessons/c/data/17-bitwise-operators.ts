import type { Lesson } from "../../types";

export const bitwiseOperators: Lesson = {
	id: "bitwise-operators",
	title: "Bitwise Operators",
	chapterId: "enums-and-bitwise",
	content: `## Bitwise Operators

Bitwise operators work on individual bits of integer values. They are essential for low-level programming, flags, and performance-critical code.

### The Operators

| Operator | Name | Example |
|----------|------|---------|
| \`&\` | AND | \`5 & 3\` = 1 |
| \`\\|\` | OR | \`5 \\| 3\` = 7 |
| \`^\` | XOR | \`5 ^ 3\` = 6 |
| \`~\` | NOT | \`~5\` = -6 |
| \`<<\` | Left shift | \`1 << 3\` = 8 |
| \`>>\` | Right shift | \`8 >> 2\` = 2 |

> The Borg think in binary. "You will be assimilated" -- one bit at a time.

### How It Works

Think in binary:
\`\`\`
  5 = 0101
  3 = 0011
---------
  & = 0001 (1)   -- both bits must be 1
  | = 0111 (7)   -- either bit can be 1
  ^ = 0110 (6)   -- exactly one bit must be 1
\`\`\`

### Bit Flags

A common pattern is using individual bits as boolean flags:

\`\`\`c
#define READ    1   // 001
#define WRITE   2   // 010
#define EXEC    4   // 100

int perms = READ | WRITE;  // 011 = 3
if (perms & READ) {
    printf("Can read\\n");
}
\`\`\`

### Setting, Clearing, and Toggling Bits

\`\`\`c
int flags = 0;
flags = flags | (1 << 2);   // set bit 2
flags = flags & ~(1 << 2);  // clear bit 2
flags = flags ^ (1 << 2);   // toggle bit 2
\`\`\`

### Left Shift as Multiply

Shifting left by \`n\` is the same as multiplying by \`2^n\`:

\`\`\`c
int x = 3 << 2;  // 3 * 4 = 12
\`\`\`

### Your Task

Write a function \`int count_set_bits(int n)\` that counts how many bits are set to 1 in the binary representation of \`n\`. Use bitwise AND and left shift to check each bit position. Print the result for 0, 7, 255, and 1.`,

	starterCode: `#include <stdio.h>

int count_set_bits(int n) {
\t// Check each of the 32 bit positions
\treturn 0;
}

int main() {
\tprintf("%d\\n", count_set_bits(0));
\tprintf("%d\\n", count_set_bits(7));
\tprintf("%d\\n", count_set_bits(255));
\tprintf("%d\\n", count_set_bits(1));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int count_set_bits(int n) {
\tint count = 0;
\tfor (int i = 0; i < 32; i++) {
\t\tif (n & (1 << i)) {
\t\t\tcount++;
\t\t}
\t}
\treturn count;
}

int main() {
\tprintf("%d\\n", count_set_bits(0));
\tprintf("%d\\n", count_set_bits(7));
\tprintf("%d\\n", count_set_bits(255));
\tprintf("%d\\n", count_set_bits(1));
\treturn 0;
}
`,

	tests: [
		{
			name: "counts set bits",
			expected: "0\n3\n8\n1\n",
		},
		{
			name: "count_set_bits(0) = 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_set_bits(0));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "count_set_bits(15) = 4",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_set_bits(15));
\treturn 0;
}`,
			expected: "4\n",
		},
		{
			name: "count_set_bits(1023) = 10",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_set_bits(1023));
\treturn 0;
}`,
			expected: "10\n",
		},
		{
			name: "count_set_bits(128) = 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", count_set_bits(128));
\treturn 0;
}`,
			expected: "1\n",
		},
	],
};
