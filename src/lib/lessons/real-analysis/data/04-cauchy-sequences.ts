import type { Lesson } from "../../types";

export const cauchySequences: Lesson = {
	id: "cauchy-sequences",
	title: "Cauchy Sequences",
	chapterId: "sequences",
	content: `## Cauchy Sequences

A Cauchy sequence is one where the terms get arbitrarily close to **each other**, without needing to know the limit in advance.

### Definition

A sequence \\((a_n)\\) is **Cauchy** if for every \\(\\varepsilon > 0\\), there exists \\(N\\) such that for all \\(m, n > N\\):

$$|a_m - a_n| < \\varepsilon$$

### Key Theorem

In \\(\\mathbb{R}\\), a sequence is Cauchy **if and only if** it converges. This is the **completeness** of the real numbers.

### Why Cauchy Matters

The Cauchy criterion lets us prove convergence without knowing the limit. This is crucial when the limit is hard to compute directly.

### Example

For \\(a_n = 1/n\\), given \\(\\varepsilon > 0\\) and \\(m, n > N = \\lceil 2/\\varepsilon \\rceil\\):

$$|1/m - 1/n| \\le 1/m + 1/n < \\varepsilon$$

### Numerical Check

We can check the Cauchy condition for a finite range:

\`\`\`python
def is_cauchy(f, epsilon, N, check=50):
    for m in range(N+1, N+1+check):
        for n in range(N+1, N+1+check):
            if abs(f(m) - f(n)) >= epsilon:
                return False
    return True
\`\`\`

### Your Task

Implement \`is_cauchy(f, epsilon, N)\` that checks whether \\(|f(m) - f(n)| < \\varepsilon\\) for all \\(m, n\\) in the range \\([N+1, N+50]\\).

Also implement \`find_cauchy_N(f, epsilon)\` that finds the smallest \\(N\\) (starting from 1) such that \`is_cauchy(f, epsilon, N)\` returns True.`,

	starterCode: `def is_cauchy(f, epsilon, N):
    # Check |f(m) - f(n)| < epsilon for all m, n in [N+1, N+50]
    pass

def find_cauchy_N(f, epsilon):
    # Find smallest N where is_cauchy returns True
    pass

harmonic = lambda n: 1 / n
print(is_cauchy(harmonic, 0.1, 20))
print(is_cauchy(harmonic, 0.1, 5))
print(find_cauchy_N(harmonic, 0.1))
`,

	solution: `def is_cauchy(f, epsilon, N):
    for m in range(N + 1, N + 51):
        for n in range(N + 1, N + 51):
            if abs(f(m) - f(n)) >= epsilon:
                return False
    return True

def find_cauchy_N(f, epsilon):
    N = 1
    while not is_cauchy(f, epsilon, N):
        N += 1
    return N

harmonic = lambda n: 1 / n
print(is_cauchy(harmonic, 0.1, 20))
print(is_cauchy(harmonic, 0.1, 5))
print(find_cauchy_N(harmonic, 0.1))
`,

	tests: [
		{
			name: "1/n is Cauchy with epsilon=0.1 beyond N=20",
			code: `{{FUNC}}
harmonic = lambda n: 1 / n
print(is_cauchy(harmonic, 0.1, 20))`,
			expected: "True\n",
		},
		{
			name: "1/n is not Cauchy with epsilon=0.1 at N=5",
			code: `{{FUNC}}
harmonic = lambda n: 1 / n
print(is_cauchy(harmonic, 0.1, 5))`,
			expected: "False\n",
		},
		{
			name: "find_cauchy_N for 1/n with epsilon=0.05",
			code: `{{FUNC}}
harmonic = lambda n: 1 / n
print(find_cauchy_N(harmonic, 0.05))`,
			expected: "15\n",
		},
	],
};
