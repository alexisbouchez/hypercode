import type { Lesson } from "../../types";

export const primeGaps: Lesson = {
	id: "prime-gaps",
	title: "Prime Gaps & Twin Primes",
	chapterId: "primes",
	content: `## Prime Gaps

A **prime gap** is the difference between two consecutive primes $p$ and $q$ where $q$ is the next prime after $p$:

$$g(p) = \\text{next\\_prime}(p) - p$$

The smallest possible gap is 2 (since consecutive integers differ by 1, and at least one must be even, so consecutive primes must differ by at least 2 — except for the pair (2, 3)).

### Twin Primes

Two primes $(p, p+2)$ are called **twin primes** — the smallest possible gap between odd primes. Examples: $(3,5)$, $(5,7)$, $(11,13)$, $(17,19)$, $(29,31)$, …

The **Twin Prime Conjecture** (unsolved!) states there are infinitely many twin prime pairs.

\`\`\`python
def is_prime(n):
    if n < 2: return False
    if n == 2: return True
    if n % 2 == 0: return False
    for i in range(3, int(n**0.5) + 1, 2):
        if n % i == 0: return False
    return True

def next_prime(n):
    candidate = n + 1
    while not is_prime(candidate):
        candidate += 1
    return candidate

def prime_gap(p):
    return next_prime(p) - p

def is_twin_prime(p):
    return is_prime(p) and prime_gap(p) == 2

print(prime_gap(11))      # 2  (next prime is 13)
print(is_twin_prime(11))  # True
print(prime_gap(7))       # 4  (next prime is 11)
print(is_twin_prime(7))   # False
\`\`\`

### Maximal Gaps

Prime gaps grow on average (by the Prime Number Theorem), but they are irregular. After $p = 23$, the next prime is $29$ — a gap of 6. After $p = 89$, the next is $97$ — a gap of 8.

### Polignac's Conjecture

Every even number $n$ occurs as a prime gap infinitely often. This is unsolved for every specific even value, including 2 (the Twin Prime Conjecture).

### Your Task

Using the \`is_prime\` and \`next_prime\` helpers provided in the starter code, implement:
- \`prime_gap(p)\`: gap from $p$ to the next prime
- \`is_twin_prime(p)\`: whether $p$ is the smaller of a twin prime pair`,

	starterCode: `def is_prime(n):
    if n < 2: return False
    if n == 2: return True
    if n % 2 == 0: return False
    for i in range(3, int(n**0.5) + 1, 2):
        if n % i == 0: return False
    return True

def next_prime(n):
    candidate = n + 1
    while not is_prime(candidate):
        candidate += 1
    return candidate

def prime_gap(p):
    # Return next_prime(p) - p
    pass

def is_twin_prime(p):
    # Return True if p is prime and prime_gap(p) == 2
    pass

print(prime_gap(11))
print(is_twin_prime(11))
`,

	solution: `def is_prime(n):
    if n < 2: return False
    if n == 2: return True
    if n % 2 == 0: return False
    for i in range(3, int(n**0.5) + 1, 2):
        if n % i == 0: return False
    return True

def next_prime(n):
    candidate = n + 1
    while not is_prime(candidate):
        candidate += 1
    return candidate

def prime_gap(p):
    return next_prime(p) - p

def is_twin_prime(p):
    return is_prime(p) and prime_gap(p) == 2

print(prime_gap(11))
print(is_twin_prime(11))
`,

	tests: [
		{
			name: "prime_gap(11) = 2, is_twin_prime(11) = True",
			expected: "2\nTrue\n",
		},
		{
			name: "prime_gap(7) = 4 (next prime is 11)",
			code: `{{FUNC}}
print(prime_gap(7))`,
			expected: "4\n",
		},
		{
			name: "is_twin_prime(7) = False",
			code: `{{FUNC}}
print(is_twin_prime(7))`,
			expected: "False\n",
		},
		{
			name: "is_twin_prime(5) = True (5 and 7)",
			code: `{{FUNC}}
print(is_twin_prime(5))`,
			expected: "True\n",
		},
		{
			name: "prime_gap(23) = 6 (next prime is 29)",
			code: `{{FUNC}}
print(prime_gap(23))`,
			expected: "6\n",
		},
	],
};
