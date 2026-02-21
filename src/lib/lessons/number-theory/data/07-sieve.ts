import type { Lesson } from "../../types";

export const sieveOfEratosthenes: Lesson = {
	id: "sieve",
	title: "Sieve of Eratosthenes",
	chapterId: "primes",
	content: `## Sieve of Eratosthenes

When you need **all primes up to some limit** $n$, running individual primality tests is inefficient. The **Sieve of Eratosthenes** (attributed to the Greek mathematician Eratosthenes, ~240 BCE) generates all primes up to $n$ in $O(n \\log \\log n)$ time.

### The Algorithm

1. Create a boolean array \`is_prime[0..n]\`, initially all \`True\`
2. Mark \`is_prime[0] = is_prime[1] = False\`
3. For each $i$ from 2 to $\\sqrt{n}$: if \`is_prime[i]\` is \`True\`, mark all multiples of $i$ starting from $i^2$ as \`False\`
4. Collect all indices $i$ where \`is_prime[i]\` is \`True\`

\`\`\`python
def sieve(n):
    if n < 2:
        return []
    is_p = [True] * (n + 1)
    is_p[0] = is_p[1] = False
    for i in range(2, int(n**0.5) + 1):
        if is_p[i]:
            for j in range(i*i, n+1, i):
                is_p[j] = False
    return [i for i in range(2, n+1) if is_p[i]]

print(sieve(20))  # [2, 3, 5, 7, 11, 13, 17, 19]
\`\`\`

### Why Start from $i^2$?

When we reach $i$, all composites of the form $k \\cdot i$ where $k < i$ have already been marked by earlier primes. So we start at $i^2$, saving significant work.

### Time & Space Complexity

| Method | Time per query | Space |
|--------|---------------|-------|
| Trial division (one number) | $O(\\sqrt{n})$ | $O(1)$ |
| Sieve (all primes up to $n$) | $O(n \\log \\log n)$ total | $O(n)$ |

The sieve wins when you need many primes. It is the go-to algorithm for primes up to $\\sim 10^8$.

### Your Task

Implement \`sieve(n)\` that returns a **list of all primes** up to and including $n$.`,

	starterCode: `def sieve(n):
    # Create boolean array, mark composites, return list of primes
    pass

print(sieve(20))
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

print(sieve(20))
`,

	tests: [
		{
			name: "sieve(20) = [2, 3, 5, 7, 11, 13, 17, 19]",
			expected: "[2, 3, 5, 7, 11, 13, 17, 19]\n",
		},
		{
			name: "sieve(10) = [2, 3, 5, 7]",
			code: `{{FUNC}}
print(sieve(10))`,
			expected: "[2, 3, 5, 7]\n",
		},
		{
			name: "sieve(30) = [2,3,5,7,11,13,17,19,23,29]",
			code: `{{FUNC}}
print(sieve(30))`,
			expected: "[2, 3, 5, 7, 11, 13, 17, 19, 23, 29]\n",
		},
		{
			name: "sieve(1) = [] (no primes)",
			code: `{{FUNC}}
print(sieve(1))`,
			expected: "[]\n",
		},
		{
			name: "len(sieve(100)) = 25",
			code: `{{FUNC}}
print(len(sieve(100)))`,
			expected: "25\n",
		},
	],
};
