import type { Lesson } from "../../types";

export const primality: Lesson = {
	id: "primality",
	title: "Primality Testing",
	chapterId: "primes",
	content: `## Testing Whether a Number is Prime

A **prime number** is a natural number greater than 1 that has no positive divisors other than 1 and itself. Primality testing is one of the most studied problems in number theory.

### Trial Division

The simplest primality test: check if any integer from 2 to $\\sqrt{n}$ divides $n$. We only need to test up to $\\sqrt{n}$ because if $n = a \\cdot b$ and $a \\leq b$, then $a \\leq \\sqrt{n}$.

\`\`\`python
def is_prime(n):
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    for i in range(3, int(n**0.5) + 1, 2):
        if n % i == 0:
            return False
    return True

print(is_prime(17))   # True
print(is_prime(15))   # False (3 × 5)
\`\`\`

Optimizations applied:
- Handle 2 specially (the only even prime)
- Skip even numbers in the loop (step 2, starting from 3)
- Stop at $\\sqrt{n}$

### Finding the Next Prime

To find the next prime after $n$, increment a candidate and test each:

\`\`\`python
def next_prime(n):
    candidate = n + 1
    while not is_prime(candidate):
        candidate += 1
    return candidate

print(next_prime(14))  # 17
\`\`\`

### Distribution of Primes

Primes become less frequent as numbers grow. The **Prime Number Theorem** tells us that the number of primes up to $x$ is approximately $x / \\ln x$. Yet there are infinitely many primes — this was proved by Euclid around 300 BCE.

### Your Task

Implement \`is_prime(n)\` using trial division, and \`next_prime(n)\` which returns the smallest prime strictly greater than $n$.`,

	starterCode: `def is_prime(n):
    # Handle edge cases, then trial divide up to sqrt(n)
    pass

def next_prime(n):
    # Find the smallest prime > n
    pass

print(is_prime(17))
print(next_prime(14))
`,

	solution: `def is_prime(n):
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    for i in range(3, int(n**0.5) + 1, 2):
        if n % i == 0:
            return False
    return True

def next_prime(n):
    candidate = n + 1
    while not is_prime(candidate):
        candidate += 1
    return candidate

print(is_prime(17))
print(next_prime(14))
`,

	tests: [
		{
			name: "is_prime(17) = True, next_prime(14) = 17",
			expected: "True\n17\n",
		},
		{
			name: "is_prime(15) = False",
			code: `{{FUNC}}
print(is_prime(15))`,
			expected: "False\n",
		},
		{
			name: "is_prime(2) = True",
			code: `{{FUNC}}
print(is_prime(2))`,
			expected: "True\n",
		},
		{
			name: "next_prime(17) = 19",
			code: `{{FUNC}}
print(next_prime(17))`,
			expected: "19\n",
		},
		{
			name: "next_prime(100) = 101",
			code: `{{FUNC}}
print(next_prime(100))`,
			expected: "101\n",
		},
	],
};
