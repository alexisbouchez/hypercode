import type { Lesson } from "../../types";

export const infiniteSeries: Lesson = {
	id: "infinite-series",
	title: "Infinite Series",
	chapterId: "series",
	content: `## Infinite Series

An **infinite series** is the sum of the terms of a sequence:

$$\\sum_{n=1}^{\\infty} a_n = a_1 + a_2 + a_3 + \\cdots$$

### Partial Sums

The \\(N\\)-th **partial sum** is:

$$S_N = \\sum_{n=1}^{N} a_n$$

A series **converges** if the sequence of partial sums \\((S_N)\\) converges. The limit is the sum of the series.

### Geometric Series

The geometric series \\(\\sum_{n=0}^{\\infty} r^n\\) converges to \\(\\frac{1}{1-r}\\) when \\(|r| < 1\\).

For example, \\(\\sum_{n=0}^{\\infty} (1/2)^n = 2\\).

### Harmonic Series

The harmonic series \\(\\sum_{n=1}^{\\infty} 1/n\\) **diverges** -- the partial sums grow without bound, even though the terms go to zero.

### The Divergence Test

If \\(\\lim_{n \\to \\infty} a_n \\ne 0\\), then \\(\\sum a_n\\) diverges. (The converse is false -- the harmonic series is a counterexample.)

### Your Task

Implement:
1. \`partial_sum(f, N)\` -- computes \\(\\sum_{n=1}^{N} f(n)\\)
2. \`partial_sums(f, N)\` -- returns a list \\([S_1, S_2, \\ldots, S_N]\\)`,

	starterCode: `def partial_sum(f, N):
    # Compute sum of f(1) + f(2) + ... + f(N)
    pass

def partial_sums(f, N):
    # Return list [S_1, S_2, ..., S_N] where S_k = sum of f(1)..f(k)
    pass

geometric = lambda n: (1/2) ** n
harmonic = lambda n: 1 / n

print(round(partial_sum(geometric, 20), 4))
print([round(s, 4) for s in partial_sums(harmonic, 5)])
print(round(partial_sum(lambda n: 1/n**2, 10000), 4))
`,

	solution: `def partial_sum(f, N):
    return sum(f(n) for n in range(1, N + 1))

def partial_sums(f, N):
    result = []
    s = 0
    for n in range(1, N + 1):
        s += f(n)
        result.append(s)
    return result

geometric = lambda n: (1/2) ** n
harmonic = lambda n: 1 / n

print(round(partial_sum(geometric, 20), 4))
print([round(s, 4) for s in partial_sums(harmonic, 5)])
print(round(partial_sum(lambda n: 1/n**2, 10000), 4))
`,

	tests: [
		{
			name: "geometric series partial sum approaches 1",
			code: `{{FUNC}}
geometric = lambda n: (1/2) ** n
print(round(partial_sum(geometric, 20), 4))`,
			expected: "1.0\n",
		},
		{
			name: "harmonic partial sums list",
			code: `{{FUNC}}
harmonic = lambda n: 1 / n
print([round(s, 4) for s in partial_sums(harmonic, 5)])`,
			expected: "[1.0, 1.5, 1.8333, 2.0833, 2.2833]\n",
		},
		{
			name: "Basel problem approximation (pi^2/6)",
			code: `{{FUNC}}
print(round(partial_sum(lambda n: 1/n**2, 10000), 4))`,
			expected: "1.6448\n",
		},
	],
};
