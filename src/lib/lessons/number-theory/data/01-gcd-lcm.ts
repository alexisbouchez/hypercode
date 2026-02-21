import type { Lesson } from "../../types";

export const gcdLcm: Lesson = {
	id: "gcd-lcm",
	title: "GCD & LCM",
	chapterId: "divisibility",
	content: `## Greatest Common Divisor & Least Common Multiple

Two of the most fundamental operations in number theory are the **GCD** (greatest common divisor) and the **LCM** (least common multiple).

### Euclidean Algorithm

The GCD of two integers $a$ and $b$ is the largest integer that divides both. The Euclidean algorithm computes it efficiently using the key identity:

$$\\gcd(a, b) = \\gcd(b,\\, a \\bmod b)$$

This recurses until $b = 0$, at which point $\\gcd(a, 0) = a$.

\`\`\`python
def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

print(gcd(48, 18))  # 6
\`\`\`

The algorithm runs in $O(\\log(\\min(a,b)))$ steps — extremely fast even for very large numbers.

### Least Common Multiple

The LCM is the smallest positive integer divisible by both $a$ and $b$. It is related to GCD by:

$$\\text{lcm}(a, b) = \\frac{a \\cdot b}{\\gcd(a, b)}$$

Use integer division (\`//\`) to avoid floating-point issues:

\`\`\`python
def lcm(a, b):
    return a * b // gcd(a, b)

print(lcm(4, 6))  # 12
\`\`\`

### Why This Works

For any two integers, $a \\cdot b = \\gcd(a,b) \\cdot \\text{lcm}(a,b)$. This is because the prime factorizations of $a$ and $b$ interlock: GCD takes the **minimum** exponent of each prime, while LCM takes the **maximum**.

### Your Task

Implement \`gcd(a, b)\` using the Euclidean algorithm (do **not** use \`math.gcd\`), and \`lcm(a, b)\` using the formula above.`,

	starterCode: `def gcd(a, b):
    # Implement using the Euclidean algorithm
    pass

def lcm(a, b):
    # Use the formula: a * b // gcd(a, b)
    pass

print(gcd(48, 18))
print(lcm(4, 6))
`,

	solution: `def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

def lcm(a, b):
    return a * b // gcd(a, b)

print(gcd(48, 18))
print(lcm(4, 6))
`,

	tests: [
		{
			name: "gcd(48, 18) = 6, lcm(4, 6) = 12",
			expected: "6\n12\n",
		},
		{
			name: "gcd(100, 75) = 25",
			code: `{{FUNC}}
print(gcd(100, 75))`,
			expected: "25\n",
		},
		{
			name: "lcm(12, 18) = 36",
			code: `{{FUNC}}
print(lcm(12, 18))`,
			expected: "36\n",
		},
		{
			name: "gcd(7, 13) = 1 (coprime primes)",
			code: `{{FUNC}}
print(gcd(7, 13))`,
			expected: "1\n",
		},
		{
			name: "lcm(5, 7) = 35",
			code: `{{FUNC}}
print(lcm(5, 7))`,
			expected: "35\n",
		},
	],
};
