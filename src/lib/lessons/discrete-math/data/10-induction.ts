import type { Lesson } from "../../types";

export const induction: Lesson = {
	id: "induction",
	title: "Mathematical Induction",
	chapterId: "induction-recurrences",
	content: `## Proof by Induction

**Mathematical induction** proves statements of the form $P(n)$ for all $n \\geq n_0$:

1. **Base case**: Prove $P(n_0)$.
2. **Inductive step**: Assume $P(k)$ (inductive hypothesis), prove $P(k+1)$.

### Classic Example: Sum Formula

**Theorem**: $\\displaystyle\\sum_{i=1}^{n} i = \\frac{n(n+1)}{2}$

**Proof**: Base case $n=1$: $1 = 1 \\cdot 2 / 2$ ✓. Inductive step: assume it holds for $k$. Then:
$$\\sum_{i=1}^{k+1} i = \\frac{k(k+1)}{2} + (k+1) = \\frac{(k+1)(k+2)}{2} \\checkmark$$

### Sum of Squares

$$\\sum_{i=1}^{n} i^2 = \\frac{n(n+1)(2n+1)}{6}$$

### Strong Induction

In **strong induction**, the hypothesis is $P(1) \\land P(2) \\land \\cdots \\land P(k)$, not just $P(k)$. This is equivalent in power but more convenient for some proofs (e.g., unique prime factorization).

\`\`\`python
def verify_sum_of_squares(n):
    actual = sum(i**2 for i in range(1, n+1))
    formula = n * (n + 1) * (2 * n + 1) // 6
    return actual == formula

print(verify_sum_of_squares(100))  # True
\`\`\`

### Your Task

Implement the three verification functions below.`,

	starterCode: `def verify_sum_formula(n):
    # Check sum(1..n) == n*(n+1)//2
    actual = sum(range(1, n + 1))
    formula = n * (n + 1) // 2
    return actual == formula

def verify_sum_of_squares(n):
    # Check sum(i^2 for i in 1..n) == n*(n+1)*(2n+1)//6
    pass

def verify_geometric_sum(n, r):
    # Check 1 + r + r^2 + ... + r^n == (r^(n+1) - 1) // (r - 1)
    pass

def strong_induction_fib(n):
    # Verify F(i) < 2^i for all i in 1..n (Fibonacci bound)
    pass

print(verify_sum_formula(1000))
`,

	solution: `def verify_sum_formula(n):
    actual = sum(range(1, n + 1))
    formula = n * (n + 1) // 2
    return actual == formula

def verify_sum_of_squares(n):
    actual = sum(i**2 for i in range(1, n+1))
    formula = n * (n + 1) * (2 * n + 1) // 6
    return actual == formula

def verify_geometric_sum(n, r):
    actual = sum(r**i for i in range(n + 1))
    formula = (r**(n+1) - 1) // (r - 1)
    return actual == formula

def strong_induction_fib(n):
    fibs = [0, 1]
    for i in range(2, n + 1):
        fibs.append(fibs[-1] + fibs[-2])
    return all(fibs[i] < 2**i for i in range(1, n + 1))

print(verify_sum_formula(1000))
`,

	tests: [
		{
			name: "sum formula holds up to n=1000",
			expected: "True\n",
		},
		{
			name: "sum of squares formula holds up to n=100",
			code: `{{FUNC}}
print(verify_sum_of_squares(100))`,
			expected: "True\n",
		},
		{
			name: "geometric sum formula holds for r=3, n=10",
			code: `{{FUNC}}
print(verify_geometric_sum(10, 3))`,
			expected: "True\n",
		},
		{
			name: "Fibonacci satisfies F(n) < 2^n for n=1..30",
			code: `{{FUNC}}
print(strong_induction_fib(30))`,
			expected: "True\n",
		},
	],
};
