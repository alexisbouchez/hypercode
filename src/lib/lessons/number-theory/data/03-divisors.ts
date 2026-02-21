import type { Lesson } from "../../types";

export const divisors: Lesson = {
	id: "divisors",
	title: "Divisors",
	chapterId: "divisibility",
	content: `## Divisors of a Number

A **divisor** (or factor) of $n$ is an integer $d$ such that $d \\mid n$ (i.e., $n \\bmod d = 0$). Finding all divisors of a number is a foundational task in number theory.

### Efficient Divisor Finding

A naive approach checks all integers from $1$ to $n$, which is $O(n)$. We can do better: divisors come in **pairs** $(d,\\, n/d)$ where $d \\leq \\sqrt{n}$, so we only need to check up to $\\sqrt{n}$:

\`\`\`python
def divisors(n):
    divs = []
    for i in range(1, int(n**0.5) + 1):
        if n % i == 0:
            divs.append(i)
            if i != n // i:       # avoid duplicating perfect-square root
                divs.append(n // i)
    return sorted(divs)

print(divisors(12))   # [1, 2, 3, 4, 6, 12]
\`\`\`

This runs in $O(\\sqrt{n})$ time.

### Number of Divisors

The **divisor function** $\\tau(n)$ (also written $d(n)$ or $\\sigma_0(n)$) counts how many divisors $n$ has. If $n = p_1^{e_1} \\cdots p_k^{e_k}$, then:

$$\\tau(n) = (e_1 + 1)(e_2 + 1) \\cdots (e_k + 1)$$

For example, $12 = 2^2 \\cdot 3^1$ so $\\tau(12) = (2+1)(1+1) = 6$.

\`\`\`python
def count_divisors(n):
    return len(divisors(n))

print(count_divisors(12))  # 6
\`\`\`

### Highly Composite Numbers

Numbers with unusually many divisors are called **highly composite numbers**: 1, 2, 4, 6, 12, 24, 36, 48, 60, …

### Your Task

Implement \`divisors(n)\` returning a **sorted list** of all divisors of $n$, and \`count_divisors(n)\` returning the count.`,

	starterCode: `def divisors(n):
    # Find all divisors using the pair trick (check up to sqrt(n))
    # Return sorted list
    pass

def count_divisors(n):
    # Return the number of divisors of n
    pass

print(divisors(12))
print(count_divisors(12))
`,

	solution: `def divisors(n):
    divs = []
    for i in range(1, int(n**0.5) + 1):
        if n % i == 0:
            divs.append(i)
            if i != n // i:
                divs.append(n // i)
    return sorted(divs)

def count_divisors(n):
    return len(divisors(n))

print(divisors(12))
print(count_divisors(12))
`,

	tests: [
		{
			name: "divisors(12) = [1,2,3,4,6,12], count_divisors(12) = 6",
			expected: "[1, 2, 3, 4, 6, 12]\n6\n",
		},
		{
			name: "divisors(28) = [1,2,4,7,14,28]",
			code: `{{FUNC}}
print(divisors(28))`,
			expected: "[1, 2, 4, 7, 14, 28]\n",
		},
		{
			name: "count_divisors(28) = 6",
			code: `{{FUNC}}
print(count_divisors(28))`,
			expected: "6\n",
		},
		{
			name: "divisors(7) = [1, 7] (prime)",
			code: `{{FUNC}}
print(divisors(7))`,
			expected: "[1, 7]\n",
		},
		{
			name: "count_divisors(36) = 9",
			code: `{{FUNC}}
print(count_divisors(36))`,
			expected: "9\n",
		},
	],
};
