import type { Lesson } from "../../types";

export const perfectNumbers: Lesson = {
	id: "perfect-numbers",
	title: "Perfect Numbers",
	chapterId: "divisibility",
	content: `## Perfect, Abundant & Deficient Numbers

The ancient Greeks classified numbers by comparing them to the **sum of their proper divisors** — all divisors except the number itself.

$$s(n) = \\sum_{\\substack{d \\mid n \\\\ d < n}} d$$

Based on this sum, every positive integer $n > 1$ falls into one of three categories:

| Category | Condition | Example |
|----------|-----------|---------|
| **Perfect** | $s(n) = n$ | $6$: $1+2+3 = 6$ |
| **Abundant** | $s(n) > n$ | $12$: $1+2+3+4+6 = 16 > 12$ |
| **Deficient** | $s(n) < n$ | $7$: $1 < 7$ |

### Perfect Numbers

A **perfect number** equals the sum of its proper divisors. The first four are: 6, 28, 496, 8128. Euler proved that all even perfect numbers have the form $2^{p-1}(2^p - 1)$ where $2^p - 1$ is a Mersenne prime. No odd perfect number has ever been found.

\`\`\`python
def proper_divisor_sum(n):
    return sum(i for i in range(1, n) if n % i == 0)

print(proper_divisor_sum(6))   # 6  → perfect
print(proper_divisor_sum(28))  # 28 → perfect
\`\`\`

### Classification

\`\`\`python
def is_perfect(n):
    return proper_divisor_sum(n) == n

def classify(n):
    s = proper_divisor_sum(n)
    if s == n:
        return "perfect"
    elif s > n:
        return "abundant"
    else:
        return "deficient"

print(classify(12))  # abundant
print(classify(7))   # deficient
\`\`\`

### Your Task

Implement \`proper_divisor_sum(n)\`, \`is_perfect(n)\`, and \`classify(n)\` which returns \`"perfect"\`, \`"abundant"\`, or \`"deficient"\`.`,

	starterCode: `def proper_divisor_sum(n):
    # Sum of all proper divisors of n (divisors < n)
    pass

def is_perfect(n):
    # Return True if n equals its proper divisor sum
    pass

def classify(n):
    # Return "perfect", "abundant", or "deficient"
    pass

print(proper_divisor_sum(6))
print(is_perfect(6))
print(classify(6))
`,

	solution: `def proper_divisor_sum(n):
    return sum(i for i in range(1, n) if n % i == 0)

def is_perfect(n):
    return proper_divisor_sum(n) == n

def classify(n):
    s = proper_divisor_sum(n)
    if s == n:
        return "perfect"
    elif s > n:
        return "abundant"
    else:
        return "deficient"

print(proper_divisor_sum(6))
print(is_perfect(6))
print(classify(6))
`,

	tests: [
		{
			name: "proper_divisor_sum(6) = 6, is_perfect(6) = True, classify(6) = 'perfect'",
			expected: "6\nTrue\nperfect\n",
		},
		{
			name: "classify(12) = abundant",
			code: `{{FUNC}}
print(classify(12))`,
			expected: "abundant\n",
		},
		{
			name: "classify(7) = deficient",
			code: `{{FUNC}}
print(classify(7))`,
			expected: "deficient\n",
		},
		{
			name: "is_perfect(28) = True",
			code: `{{FUNC}}
print(is_perfect(28))`,
			expected: "True\n",
		},
		{
			name: "proper_divisor_sum(28) = 28",
			code: `{{FUNC}}
print(proper_divisor_sum(28))`,
			expected: "28\n",
		},
	],
};
