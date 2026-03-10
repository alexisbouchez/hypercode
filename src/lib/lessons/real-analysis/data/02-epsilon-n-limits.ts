import type { Lesson } from "../../types";

export const epsilonNLimits: Lesson = {
	id: "epsilon-n-limits",
	title: "Epsilon-N Definition of Limits",
	chapterId: "sequences",
	content: `## The Epsilon-N Definition of Limits

The formal definition of a sequence limit is the cornerstone of real analysis.

### Definition

We say \\(\\lim_{n \\to \\infty} a_n = L\\) if for every \\(\\varepsilon > 0\\), there exists a natural number \\(N\\) such that for all \\(n > N\\):

$$|a_n - L| < \\varepsilon$$

### Intuition

No matter how small a tolerance \\(\\varepsilon\\) you choose, eventually the sequence gets and stays within that tolerance of \\(L\\).

### Finding N for a Given Epsilon

For \\(a_n = 1/n\\) with limit \\(L = 0\\):
- We need \\(|1/n - 0| < \\varepsilon\\)
- This means \\(1/n < \\varepsilon\\), so \\(n > 1/\\varepsilon\\)
- Therefore \\(N = \\lceil 1/\\varepsilon \\rceil\\)

\`\`\`python
import math
def find_N(epsilon):
    return math.ceil(1 / epsilon)
\`\`\`

### Verifying Convergence Numerically

We can check that all terms beyond \\(N\\) stay within \\(\\varepsilon\\) of \\(L\\):

\`\`\`python
def verify_limit(f, L, epsilon, N, check_terms=100):
    return all(abs(f(n) - L) < epsilon for n in range(N + 1, N + 1 + check_terms))
\`\`\`

### Your Task

Implement \`find_N(f, L, epsilon)\` that finds the smallest \\(N\\) such that \\(|f(n) - L| < \\varepsilon\\) for all \\(n > N\\) (check up to \\(N + 200\\) to be confident).

Also implement \`verify_limit(f, L, epsilon, N)\` that returns \`True\` if \\(|f(n) - L| < \\varepsilon\\) for all \\(n\\) from \\(N+1\\) to \\(N+200\\).`,

	starterCode: `def find_N(f, L, epsilon):
    # Find smallest N such that |f(n) - L| < epsilon for n > N
    # Check up to N + 200 to be confident
    pass

def verify_limit(f, L, epsilon, N):
    # Return True if |f(n) - L| < epsilon for n = N+1 to N+200
    pass

harmonic = lambda n: 1 / n
print(find_N(harmonic, 0, 0.1))
print(find_N(harmonic, 0, 0.01))
print(verify_limit(harmonic, 0, 0.1, 10))
`,

	solution: `def find_N(f, L, epsilon):
    N = 1
    while True:
        if all(abs(f(n) - L) < epsilon for n in range(N + 1, N + 201)):
            return N
        N += 1

def verify_limit(f, L, epsilon, N):
    return all(abs(f(n) - L) < epsilon for n in range(N + 1, N + 201))

harmonic = lambda n: 1 / n
print(find_N(harmonic, 0, 0.1))
print(find_N(harmonic, 0, 0.01))
print(verify_limit(harmonic, 0, 0.1, 10))
`,

	tests: [
		{
			name: "find_N for 1/n with epsilon=0.1",
			code: `{{FUNC}}
harmonic = lambda n: 1 / n
print(find_N(harmonic, 0, 0.1))`,
			expected: "10\n",
		},
		{
			name: "find_N for 1/n with epsilon=0.01",
			code: `{{FUNC}}
harmonic = lambda n: 1 / n
print(find_N(harmonic, 0, 0.01))`,
			expected: "100\n",
		},
		{
			name: "verify_limit succeeds for 1/n beyond N=10",
			code: `{{FUNC}}
harmonic = lambda n: 1 / n
print(verify_limit(harmonic, 0, 0.1, 10))`,
			expected: "True\n",
		},
		{
			name: "verify_limit fails for 1/n with too small N",
			code: `{{FUNC}}
harmonic = lambda n: 1 / n
print(verify_limit(harmonic, 0, 0.1, 5))`,
			expected: "False\n",
		},
	],
};
