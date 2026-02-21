import type { Lesson } from "../../types";

export const primeCounting: Lesson = {
	id: "prime-counting",
	title: "Prime Counting Function",
	chapterId: "primes",
	content: `## The Prime Counting Function

The **prime counting function** $\\pi(x)$ counts the number of primes less than or equal to $x$:

$$\\pi(x) = \\#\\{p \\leq x : p \\text{ is prime}\\}$$

For example: $\\pi(10) = 4$ (the primes 2, 3, 5, 7), $\\pi(100) = 25$.

### Prime Number Theorem

The **Prime Number Theorem** (proved independently by Hadamard and de la Vallée Poussin in 1896) gives the asymptotic behavior:

$$\\pi(x) \\sim \\frac{x}{\\ln x}$$

A better approximation is:

$$\\pi(x) \\approx \\frac{x}{\\ln x - 1}$$

This approximation is accurate to within a few percent for moderate values of $x$.

\`\`\`python
import math

def prime_pi_approx(n):
    if n < 2:
        return 0
    return int(round(n / (math.log(n) - 1), 0))

print(prime_pi_approx(100))  # 28  (exact: 25)
\`\`\`

### Exact Count with the Sieve

For the exact count, the Sieve of Eratosthenes gives $\\pi(n)$ in $O(n \\log \\log n)$ time:

\`\`\`python
def prime_pi(n):
    if n < 2:
        return 0
    is_p = [True] * (n + 1)
    is_p[0] = is_p[1] = False
    for i in range(2, int(n**0.5) + 1):
        if is_p[i]:
            for j in range(i*i, n+1, i):
                is_p[j] = False
    return sum(is_p)

print(prime_pi(100))   # 25
print(prime_pi(1000))  # 168
\`\`\`

### Error of the Approximation

| $n$ | $\\pi(n)$ exact | $\\approx n/(\\ln n - 1)$ | Error |
|-----|----------------|--------------------------|-------|
| 100 | 25 | 28 | +12% |
| 1000 | 168 | 169 | +0.6% |
| 10000 | 1229 | 1218 | −0.9% |

### Your Task

Implement:
- \`prime_pi(n)\` — exact count using the sieve
- \`prime_pi_approx(n)\` — approximation $\\text{int}(\\text{round}(n / (\\ln n - 1), 0))$`,

	starterCode: `import math

def prime_pi(n):
    # Exact count using sieve of Eratosthenes
    pass

def prime_pi_approx(n):
    # Approximate: int(round(n / (math.log(n) - 1), 0))
    pass

print(prime_pi(100))
print(prime_pi_approx(100))
`,

	solution: `import math

def prime_pi(n):
    if n < 2:
        return 0
    is_p = [True] * (n + 1)
    is_p[0] = is_p[1] = False
    for i in range(2, int(n**0.5) + 1):
        if is_p[i]:
            for j in range(i*i, n+1, i):
                is_p[j] = False
    return sum(is_p)

def prime_pi_approx(n):
    if n < 2:
        return 0
    return int(round(n / (math.log(n) - 1), 0))

print(prime_pi(100))
print(prime_pi_approx(100))
`,

	tests: [
		{
			name: "prime_pi(100) = 25, prime_pi_approx(100) = 28",
			expected: "25\n28\n",
		},
		{
			name: "prime_pi(20) = 8",
			code: `{{FUNC}}
print(prime_pi(20))`,
			expected: "8\n",
		},
		{
			name: "prime_pi(50) = 15",
			code: `{{FUNC}}
print(prime_pi(50))`,
			expected: "15\n",
		},
		{
			name: "prime_pi_approx(50) = 17",
			code: `{{FUNC}}
print(prime_pi_approx(50))`,
			expected: "17\n",
		},
		{
			name: "prime_pi(1000) = 168",
			code: `{{FUNC}}
print(prime_pi(1000))`,
			expected: "168\n",
		},
	],
};
