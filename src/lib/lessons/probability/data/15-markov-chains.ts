import type { Lesson } from "../../types";

export const markovChains: Lesson = {
	id: "markov-chains",
	title: "Markov Chains",
	chapterId: "limit-theorems",
	content: `## The Markov Property

A sequence $X_0, X_1, X_2, \\ldots$ is a **Markov chain** if the future depends only on the present, not the past:

$$P(X_{n+1} = j \\mid X_n = i, X_{n-1}, \\ldots, X_0) = P(X_{n+1} = j \\mid X_n = i)$$

### Transition Matrix

The **transition matrix** $P$ captures all single-step probabilities:

$$P_{ij} = P(X_{n+1} = j \\mid X_n = i)$$

Each row sums to 1. The $n$-step distribution is obtained by matrix-vector multiplication:

$$\\mathbf{v}^{(n)} = \\mathbf{v}^{(0)} \\cdot P^n$$

### Stationary Distribution

A distribution $\\pi$ is **stationary** if $\\pi P = \\pi$. It is the long-run fraction of time spent in each state.

For an ergodic chain, iterating $v \\leftarrow v P$ converges to $\\pi$ from any starting distribution.

\`\`\`python
# Weather model: sunny(0) or rainy(1)
P = [[0.9, 0.1],   # from sunny: 90% stay sunny
     [0.2, 0.8]]   # from rainy: 80% stay rainy

# Stationary distribution: solve πP = π
v = [0.5, 0.5]
for _ in range(1000):
    v = [sum(v[i]*P[i][j] for i in range(2)) for j in range(2)]
print([round(x, 4) for x in v])  # [0.6667, 0.3333]
\`\`\`

Analytically: $\\pi_0 \\cdot 0.1 = \\pi_1 \\cdot 0.2$ gives $\\pi_0/\\pi_1 = 2$, so $\\pi_0 = 2/3$.

### Your Task

Implement two functions:
- \`markov_stationary(P)\` — returns the stationary distribution by iterating 1000 steps from $[1/n, \\ldots, 1/n]$, rounded to 4 decimal places, one per line
- \`markov_n_step(initial, P, n)\` — returns the $n$-step distribution, rounded to 4 decimal places, one per line`,

	starterCode: `def markov_stationary(P):
    n = len(P)
    v = [1/n] * n
    for _ in range(1000):
        v = [sum(v[i]*P[i][j] for i in range(n)) for j in range(n)]
    for x in v:
        print(round(x, 4))

def markov_n_step(initial, P, n):
    v = initial[:]
    for _ in range(n):
        v = [sum(v[i]*P[i][j] for i in range(len(P))) for j in range(len(P))]
    for x in v:
        print(round(x, 4))

P = [[0.9, 0.1], [0.2, 0.8]]
markov_stationary(P)
`,

	solution: `def markov_stationary(P):
    n = len(P)
    v = [1/n] * n
    for _ in range(1000):
        v = [sum(v[i]*P[i][j] for i in range(n)) for j in range(n)]
    for x in v:
        print(round(x, 4))

def markov_n_step(initial, P, n):
    v = initial[:]
    for _ in range(n):
        v = [sum(v[i]*P[i][j] for i in range(len(P))) for j in range(len(P))]
    for x in v:
        print(round(x, 4))

P = [[0.9, 0.1], [0.2, 0.8]]
markov_stationary(P)
`,

	tests: [
		{
			name: "stationary of [[0.9,0.1],[0.2,0.8]] = [0.6667, 0.3333]",
			expected: "0.6667\n0.3333\n",
		},
		{
			name: "1-step from [1,0]: [0.9, 0.1]",
			code: `{{FUNC}}
markov_n_step([1.0, 0.0], [[0.9, 0.1], [0.2, 0.8]], 1)`,
			expected: "0.9\n0.1\n",
		},
		{
			name: "doubly stochastic [[0.5,0.5],[0.5,0.5]] → uniform stationary",
			code: `{{FUNC}}
markov_stationary([[0.5, 0.5], [0.5, 0.5]])`,
			expected: "0.5\n0.5\n",
		},
		{
			name: "10-step from [0.5,0.5] converges toward stationary",
			code: `{{FUNC}}
markov_n_step([0.5, 0.5], [[0.9, 0.1], [0.2, 0.8]], 10)`,
			expected: "0.662\n0.338\n",
		},
	],
};
