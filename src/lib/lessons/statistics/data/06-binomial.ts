import type { Lesson } from "../../types";

export const binomialDistribution: Lesson = {
	id: "binomial-distribution",
	title: "Binomial Distribution",
	chapterId: "distributions",
	content: `## Counting Successes

The **binomial distribution** models the number of successes in \`n\` independent trials, each with success probability \`p\`.

\`\`\`python
from scipy import stats

# Flip a fair coin 5 times. What is P(exactly 2 heads)?
n, p = 5, 0.5
dist = stats.binom(n=n, p=p)

print(round(dist.pmf(2), 4))   # 0.3125 — P(X = 2)
print(round(dist.cdf(2), 4))   # 0.5    — P(X ≤ 2)
\`\`\`

### PMF vs CDF

- **PMF** (probability mass function): P(X = k) — the probability of exactly k successes
- **CDF**: P(X ≤ k) — the probability of k or fewer successes

### Parameters

- **n** — number of trials
- **p** — probability of success per trial
- **k** — number of successes to query

### Example: Quality Control

In a factory, 10% of items are defective. If you inspect 20 items, what is the probability of finding exactly 2 defective ones?

\`\`\`python
print(round(stats.binom.pmf(2, n=20, p=0.1), 4))  # 0.2852
\`\`\`

### Your Task

Implement \`binomial_stats(n, p, k)\` that prints:
1. P(X = k) — the PMF (rounded to 4 decimal places)
2. P(X ≤ k) — the CDF (rounded to 4 decimal places)`,

	starterCode: `from scipy import stats

def binomial_stats(n, p, k):
    # Print PMF(k) and CDF(k) of Binomial(n, p), each rounded to 4 decimal places
    pass

binomial_stats(5, 0.5, 2)
`,

	solution: `from scipy import stats

def binomial_stats(n, p, k):
    dist = stats.binom(n=n, p=p)
    print(round(float(dist.pmf(k)), 4))
    print(round(float(dist.cdf(k)), 4))

binomial_stats(5, 0.5, 2)
`,

	tests: [
		{
			name: "binomial_stats(5,0.5,2) → PMF=0.3125, CDF=0.5",
			expected: "0.3125\n0.5\n",
		},
		{
			name: "binomial_stats(5,0.5,3) → PMF=0.3125, CDF=0.8125",
			code: `{{FUNC}}
binomial_stats(5, 0.5, 3)`,
			expected: "0.3125\n0.8125\n",
		},
		{
			name: "binomial_stats(10,1.0,10) → PMF=1.0, CDF=1.0",
			code: `{{FUNC}}
binomial_stats(10, 1.0, 10)`,
			expected: "1.0\n1.0\n",
		},
		{
			name: "binomial_stats(20,0.1,0) → PMF=0.1216, CDF=0.1216",
			code: `{{FUNC}}
binomial_stats(20, 0.1, 0)`,
			expected: "0.1216\n0.1216\n",
		},
	],
};
