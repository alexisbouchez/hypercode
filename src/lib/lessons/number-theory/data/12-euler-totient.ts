import type { Lesson } from "../../types";

export const eulerTotient: Lesson = {
	id: "euler-totient",
	title: "Euler's Totient Function",
	chapterId: "modular",
	content: `## Euler's Totient Function

**Euler's totient function** $\\varphi(n)$ (phi of n) counts how many integers in $\\{1, 2, \\ldots, n\\}$ are **coprime** to $n$ (i.e., share no common factor with $n$ other than 1):

$$\\varphi(n) = \\#\\{k : 1 \\leq k \\leq n,\\; \\gcd(k, n) = 1\\}$$

### Examples

- $\\varphi(12) = 4$: the numbers $\\{1, 5, 7, 11\\}$ are coprime to 12
- $\\varphi(7) = 6$: all of $\\{1, 2, 3, 4, 5, 6\\}$ are coprime to 7 (7 is prime)
- For any prime $p$: $\\varphi(p) = p - 1$

### Computing the Totient

A direct approach: count $k$ from 1 to $n$ using the GCD.

\`\`\`python
def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

def euler_totient(n):
    return sum(1 for k in range(1, n + 1) if gcd(k, n) == 1)

print(euler_totient(12))  # 4
\`\`\`

There is also a product formula using the prime factorization of $n$:

$$\\varphi(n) = n \\prod_{p \\mid n} \\left(1 - \\frac{1}{p}\\right)$$

### Euler's Theorem

If $\\gcd(a, n) = 1$, then:

$$a^{\\varphi(n)} \\equiv 1 \\pmod{n}$$

This generalizes Fermat's Little Theorem (which applies only when $n$ is prime) and is the theoretical foundation of RSA encryption.

### Totient Sum

The sum of the totient over all integers up to $n$ has a neat relation to counting coprime pairs:

$$\\sum_{k=1}^{n} \\varphi(k) \\approx \\frac{3n^2}{\\pi^2}$$

\`\`\`python
def totient_sum(n):
    return sum(euler_totient(k) for k in range(1, n + 1))

print(totient_sum(5))   # 10
\`\`\`

### Your Task

Implement \`euler_totient(n)\` and \`totient_sum(n)\` using the GCD-based approach.`,

	starterCode: `def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

def euler_totient(n):
    # Count k in [1, n] with gcd(k, n) == 1
    pass

def totient_sum(n):
    # Sum of euler_totient(k) for k = 1 to n
    pass

print(euler_totient(12))
print(totient_sum(5))
`,

	solution: `def gcd(a, b):
    while b:
        a, b = b, a % b
    return a

def euler_totient(n):
    return sum(1 for k in range(1, n + 1) if gcd(k, n) == 1)

def totient_sum(n):
    return sum(euler_totient(k) for k in range(1, n + 1))

print(euler_totient(12))
print(totient_sum(5))
`,

	tests: [
		{
			name: "euler_totient(12) = 4, totient_sum(5) = 10",
			expected: "4\n10\n",
		},
		{
			name: "euler_totient(7) = 6 (prime: all 1..6 are coprime)",
			code: `{{FUNC}}
print(euler_totient(7))`,
			expected: "6\n",
		},
		{
			name: "euler_totient(1) = 1",
			code: `{{FUNC}}
print(euler_totient(1))`,
			expected: "1\n",
		},
		{
			name: "totient_sum(10) = 32",
			code: `{{FUNC}}
print(totient_sum(10))`,
			expected: "32\n",
		},
		{
			name: "euler_totient(36) = 12",
			code: `{{FUNC}}
print(euler_totient(36))`,
			expected: "12\n",
		},
	],
};
