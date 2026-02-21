import type { Lesson } from "../../types";

export const logarithms: Lesson = {
	id: "logarithms",
	title: "Logarithms & Exponentials",
	chapterId: "mathematics",
	content: `## Logarithms and Exponentials in C

\`<math.h>\` provides logarithm and exponential functions — essential for scientific computation, signal processing, and algorithm analysis.

### The Functions

| Function | Description | Example |
|----------|-------------|---------|
| \`log(x)\` | Natural log (base e) | \`log(M_E)\` → 1.0 |
| \`log2(x)\` | Log base 2 | \`log2(8.0)\` → 3.0 |
| \`log10(x)\` | Log base 10 | \`log10(100.0)\` → 2.0 |
| \`exp(x)\` | e raised to x | \`exp(1.0)\` → 2.71828... |

### Key Identities

\`\`\`c
// log and exp are inverses
exp(log(5.0)) == 5.0

// Change of base formula
// log_b(x) = log(x) / log(b)
double log3_of_81 = log(81.0) / log(3.0);  // 4.0

// log2 is handy for computer science
// How many bits to represent n values?
int bits = (int)ceil(log2(256.0));  // 8
\`\`\`

### Decibels (dB)

Logarithms are used in audio to express power ratios:

\`\`\`c
// dB = 10 * log10(power_ratio)
double ratio = 100.0;
double db = 10.0 * log10(ratio);  // 20.0 dB
\`\`\`

### Complexity Analysis

\`log2\` appears constantly in algorithm analysis (binary search, divide-and-conquer):

\`\`\`c
// Steps for binary search on n elements ≈ log2(n)
printf("%.0f\\n", log2(1024.0));  // 10 steps for 1024 elements
\`\`\`

### Your Task

Using \`<math.h>\`, compute and print with \`%.4f\`:
1. \`log(M_E)\` — natural log of e
2. \`log2(16.0)\` — log base 2 of 16
3. \`log10(1000.0)\` — log base 10 of 1000
4. \`exp(1.0)\` — e to the power of 1`,

	starterCode: `#include <stdio.h>
#include <math.h>

int main() {
\t// Print each result with %.4f
\treturn 0;
}
`,

	solution: `#include <stdio.h>
#include <math.h>

int main() {
\tprintf("%.4f\\n", log(M_E));
\tprintf("%.4f\\n", log2(16.0));
\tprintf("%.4f\\n", log10(1000.0));
\tprintf("%.4f\\n", exp(1.0));
\treturn 0;
}
`,

	tests: [
		{
			name: "prints logarithm and exponential results",
			expected: "1.0000\n4.0000\n3.0000\n2.7183\n",
		},
		{
			name: "log(M_E) = 1.0000",
			code: `#include <stdio.h>
#include <math.h>
int main() {
\tprintf("%.4f\\n", log(M_E));
\treturn 0;
}`,
			expected: "1.0000\n",
		},
		{
			name: "log2(16.0) = 4.0000",
			code: `#include <stdio.h>
#include <math.h>
int main() {
\tprintf("%.4f\\n", log2(16.0));
\treturn 0;
}`,
			expected: "4.0000\n",
		},
		{
			name: "log10(1000.0) = 3.0000",
			code: `#include <stdio.h>
#include <math.h>
int main() {
\tprintf("%.4f\\n", log10(1000.0));
\treturn 0;
}`,
			expected: "3.0000\n",
		},
		{
			name: "exp(1.0) = e = 2.7183",
			code: `#include <stdio.h>
#include <math.h>
int main() {
\tprintf("%.4f\\n", exp(1.0));
\treturn 0;
}`,
			expected: "2.7183\n",
		},
	],
};
