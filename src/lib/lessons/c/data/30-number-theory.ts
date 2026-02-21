import type { Lesson } from "../../types";

export const numberTheory: Lesson = {
	id: "number-theory",
	title: "Number Theory",
	chapterId: "mathematics",
	content: `## Number Theory in C

Number theory is the branch of mathematics dealing with integers and their properties. C's integer types make it an excellent language for implementing classic number-theoretic algorithms.

### Greatest Common Divisor (GCD)

The GCD of two integers is the largest integer that divides both. The **Euclidean algorithm** computes it efficiently:

\`\`\`c
int gcd(int a, int b) {
    while (b != 0) {
        int t = b;
        b = a % b;
        a = t;
    }
    return a;
}
\`\`\`

It works by repeatedly replacing \`(a, b)\` with \`(b, a % b)\` until \`b\` is zero. Then \`a\` is the GCD.

\`\`\`c
gcd(48, 18)   // 6
gcd(100, 75)  // 25
\`\`\`

### Least Common Multiple (LCM)

From the GCD, the LCM follows immediately:

$$\\text{lcm}(a, b) = \\frac{|a \\cdot b|}{\\gcd(a, b)}$$

\`\`\`c
int lcm(int a, int b) {
    return a / gcd(a, b) * b;  // divide first to avoid overflow
}
\`\`\`

### Primality Testing

A number is **prime** if it has no divisors other than 1 and itself. The basic trial-division test checks up to √n:

\`\`\`c
int is_prime(int n) {
    if (n < 2) return 0;
    for (int i = 2; i * i <= n; i++) {
        if (n % i == 0) return 0;
    }
    return 1;
}
\`\`\`

Only checking up to √n is sufficient — if \`n\` has a factor greater than √n, it must also have one smaller than √n.

\`\`\`c
is_prime(2)   // 1 (prime)
is_prime(17)  // 1 (prime)
is_prime(20)  // 0 (composite: 4×5)
\`\`\`

### Your Task

Implement \`gcd\` (using the Euclidean algorithm) and \`is_prime\` (trial division up to √n), then print:
1. \`gcd(48, 18)\`
2. \`gcd(100, 75)\`
3. \`is_prime(17)\`
4. \`is_prime(20)\``,

	starterCode: `#include <stdio.h>

int gcd(int a, int b) {
\t// Euclidean algorithm
\treturn 0;
}

int is_prime(int n) {
\t// Trial division up to sqrt(n)
\treturn 0;
}

int main() {
\tprintf("%d\\n", gcd(48, 18));
\tprintf("%d\\n", gcd(100, 75));
\tprintf("%d\\n", is_prime(17));
\tprintf("%d\\n", is_prime(20));
\treturn 0;
}
`,

	solution: `#include <stdio.h>

int gcd(int a, int b) {
\twhile (b != 0) {
\t\tint t = b;
\t\tb = a % b;
\t\ta = t;
\t}
\treturn a;
}

int is_prime(int n) {
\tif (n < 2) return 0;
\tfor (int i = 2; i * i <= n; i++) {
\t\tif (n % i == 0) return 0;
\t}
\treturn 1;
}

int main() {
\tprintf("%d\\n", gcd(48, 18));
\tprintf("%d\\n", gcd(100, 75));
\tprintf("%d\\n", is_prime(17));
\tprintf("%d\\n", is_prime(20));
\treturn 0;
}
`,

	tests: [
		{
			name: "gcd and is_prime results",
			expected: "6\n25\n1\n0\n",
		},
		{
			name: "gcd(48, 18) = 6",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", gcd(48, 18));
\treturn 0;
}`,
			expected: "6\n",
		},
		{
			name: "gcd(100, 75) = 25",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", gcd(100, 75));
\treturn 0;
}`,
			expected: "25\n",
		},
		{
			name: "is_prime(17) = 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", is_prime(17));
\treturn 0;
}`,
			expected: "1\n",
		},
		{
			name: "is_prime(20) = 0",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", is_prime(20));
\treturn 0;
}`,
			expected: "0\n",
		},
		{
			name: "is_prime(2) = 1",
			code: `#include <stdio.h>
{{FUNC}}
int main() {
\tprintf("%d\\n", is_prime(2));
\treturn 0;
}`,
			expected: "1\n",
		},
	],
};
