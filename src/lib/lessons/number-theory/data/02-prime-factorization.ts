import type { Lesson } from "../../types";

export const primeFactorization: Lesson = {
	id: "prime-factorization",
	title: "Prime Factorization",
	chapterId: "divisibility",
	content: `## Prime Factorization

The **Fundamental Theorem of Arithmetic** states that every integer $n > 1$ can be written uniquely as a product of prime numbers:

$$n = p_1^{e_1} \\cdot p_2^{e_2} \\cdots p_k^{e_k}$$

For example: $60 = 2^2 \\cdot 3 \\cdot 5$, written as the sorted list \`[2, 2, 3, 5]\`.

### Trial Division

The simplest factorization algorithm tries dividing $n$ by each candidate divisor starting from 2. A key insight: if $n$ has no factor $\\leq \\sqrt{n}$, then $n$ itself is prime.

\`\`\`python
def prime_factors(n):
    factors = []
    d = 2
    while d * d <= n:
        while n % d == 0:
            factors.append(d)
            n //= d
        d += 1
    if n > 1:          # n is a remaining prime factor
        factors.append(n)
    return sorted(factors)

print(prime_factors(12))  # [2, 2, 3]
print(prime_factors(60))  # [2, 2, 3, 5]
\`\`\`

### How It Works

1. Try dividing by $d = 2, 3, 4, \\ldots$ up to $\\sqrt{n}$
2. Every time $d \\mid n$, append $d$ and divide $n$ by $d$ (repeat for repeated factors)
3. After the loop, if $n > 1$, then $n$ is a prime — append it

The time complexity is $O(\\sqrt{n})$, which is practical for numbers up to $\\sim 10^{12}$.

### Counting with Multiplicity

The list returned includes each prime factor **with repetition** (multiplicity). For example:
- $12 = 2^2 \\cdot 3$ → \`[2, 2, 3]\`
- $36 = 2^2 \\cdot 3^2\` → \`[2, 2, 3, 3]\`

### Your Task

Implement \`prime_factors(n)\` that returns a **sorted list** of prime factors of $n$ with repetition.`,

	starterCode: `def prime_factors(n):
    # Trial division: try d=2,3,... up to sqrt(n)
    # Return sorted list with repetition
    pass

print(prime_factors(12))
print(prime_factors(60))
`,

	solution: `def prime_factors(n):
    factors = []
    d = 2
    while d * d <= n:
        while n % d == 0:
            factors.append(d)
            n //= d
        d += 1
    if n > 1:
        factors.append(n)
    return sorted(factors)

print(prime_factors(12))
print(prime_factors(60))
`,

	tests: [
		{
			name: "prime_factors(12) = [2, 2, 3], prime_factors(60) = [2, 2, 3, 5]",
			expected: "[2, 2, 3]\n[2, 2, 3, 5]\n",
		},
		{
			name: "prime_factors(36) = [2, 2, 3, 3]",
			code: `{{FUNC}}
print(prime_factors(36))`,
			expected: "[2, 2, 3, 3]\n",
		},
		{
			name: "prime_factors(7) = [7] (prime itself)",
			code: `{{FUNC}}
print(prime_factors(7))`,
			expected: "[7]\n",
		},
		{
			name: "prime_factors(100) = [2, 2, 5, 5]",
			code: `{{FUNC}}
print(prime_factors(100))`,
			expected: "[2, 2, 5, 5]\n",
		},
	],
};
