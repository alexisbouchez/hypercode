import type { Lesson } from "../../types";

export const goldbach: Lesson = {
	id: "goldbach",
	title: "Goldbach's Conjecture",
	chapterId: "primes",
	content: `## Goldbach's Conjecture

In 1742, Christian Goldbach wrote to Euler with the following conjecture:

> **Every even integer greater than 2 can be expressed as the sum of two prime numbers.**

For example:
- $4 = 2 + 2$
- $10 = 3 + 7 = 5 + 5$
- $28 = 5 + 23 = 11 + 17$

Despite being one of the oldest unsolved problems in mathematics, Goldbach's conjecture has been **verified for all even numbers up to $4 \\times 10^{18}$** — but never proved in general.

### Finding a Goldbach Pair

Given an even number $n > 2$, find the pair $(p, q)$ with $p \\leq q$ such that $p + q = n$ and both are prime. We want the **smallest** such $p$:

\`\`\`python
def goldbach_pair(n):
    primes = sieve(n)
    prime_set = set(primes)
    for p in primes:
        if n - p in prime_set and p <= n - p:
            return (p, n - p)
    return None

print(goldbach_pair(28))  # (5, 23)
print(goldbach_pair(10))  # (3, 7)
\`\`\`

### Counting Goldbach Pairs

The number of ways to write $n$ as a sum of two primes $p \\leq q$ varies: some numbers have many representations. This is related to the **Goldbach comet**.

\`\`\`python
def goldbach_count(n):
    primes = sieve(n)
    prime_set = set(primes)
    return sum(1 for p in primes if n - p in prime_set and p <= n - p)

print(goldbach_count(28))   # 2  (5+23, 11+17)
\`\`\`

### Your Task

Using a helper \`sieve(n)\` provided in the starter code, implement:
- \`goldbach_pair(n)\` → the pair \`(p, q)\` with $p \\leq q$, $p + q = n$, smallest $p$ first
- \`goldbach_count(n)\` → the number of such pairs`,

	starterCode: `def sieve(n):
    if n < 2:
        return []
    is_p = [True] * (n + 1)
    is_p[0] = is_p[1] = False
    for i in range(2, int(n**0.5) + 1):
        if is_p[i]:
            for j in range(i*i, n+1, i):
                is_p[j] = False
    return [i for i in range(2, n+1) if is_p[i]]

def goldbach_pair(n):
    # Find the smallest prime p such that n-p is also prime, p <= n-p
    pass

def goldbach_count(n):
    # Count all pairs (p, q) with p <= q, p+q=n, both prime
    pass

print(goldbach_pair(28))
print(goldbach_count(28))
`,

	solution: `def sieve(n):
    if n < 2:
        return []
    is_p = [True] * (n + 1)
    is_p[0] = is_p[1] = False
    for i in range(2, int(n**0.5) + 1):
        if is_p[i]:
            for j in range(i*i, n+1, i):
                is_p[j] = False
    return [i for i in range(2, n+1) if is_p[i]]

def goldbach_pair(n):
    primes = sieve(n)
    prime_set = set(primes)
    for p in primes:
        if n - p in prime_set and p <= n - p:
            return (p, n - p)
    return None

def goldbach_count(n):
    primes = sieve(n)
    prime_set = set(primes)
    return sum(1 for p in primes if n - p in prime_set and p <= n - p)

print(goldbach_pair(28))
print(goldbach_count(28))
`,

	tests: [
		{
			name: "goldbach_pair(28) = (5, 23), goldbach_count(28) = 2",
			expected: "(5, 23)\n2\n",
		},
		{
			name: "goldbach_pair(10) = (3, 7)",
			code: `{{FUNC}}
print(goldbach_pair(10))`,
			expected: "(3, 7)\n",
		},
		{
			name: "goldbach_count(10) = 2 (3+7 and 5+5)",
			code: `{{FUNC}}
print(goldbach_count(10))`,
			expected: "2\n",
		},
		{
			name: "goldbach_pair(4) = (2, 2)",
			code: `{{FUNC}}
print(goldbach_pair(4))`,
			expected: "(2, 2)\n",
		},
		{
			name: "goldbach_count(100) = number of pairs summing to 100",
			code: `{{FUNC}}
print(goldbach_count(100))`,
			expected: "6\n",
		},
	],
};
