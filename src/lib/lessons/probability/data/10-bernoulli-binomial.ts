import type { Lesson } from "../../types";

export const bernoulliBinomial: Lesson = {
	id: "bernoulli-binomial",
	title: "Bernoulli & Binomial Distributions",
	chapterId: "distributions",
	content: `## Counting Successes

A **Bernoulli($p$)** trial has two outcomes: success (probability $p$) and failure (probability $1-p$).

A **Binomial($n$, $p$)** random variable counts successes in $n$ independent Bernoulli trials:

$$P(X = k) = \\binom{n}{k} p^k (1-p)^{n-k}$$

$$E[X] = np, \\qquad \\text{Var}(X) = np(1-p)$$

The $\\binom{n}{k}$ factor counts the number of ways to arrange $k$ successes in $n$ positions.

\`\`\`python
import math

def binom_pmf(n, p, k):
    return math.comb(n, k) * p**k * (1 - p)**(n - k)

def binom_cdf(n, p, k):
    return sum(binom_pmf(n, p, i) for i in range(k + 1))

# P(exactly 3 heads in 10 fair flips)
print(round(binom_pmf(10, 0.5, 3), 4))   # 0.1172
print(round(binom_cdf(10, 0.5, 3), 4))   # 0.1719
\`\`\`

### Why This Distribution Is Everywhere

- Quality control: number of defective items in a batch
- Clinical trials: number of patients who respond to treatment
- Surveys: number of yes responses out of $n$ respondents
- A/B testing: number of conversions out of $n$ visitors

### Your Task

Implement \`binomial_full(n, p, k)\` that prints PMF$(k)$, CDF$(k)$, $E[X]$, and $\\text{Var}(X)$, each rounded to 4 decimal places.`,

	starterCode: `import math

def binomial_full(n, p, k):
    # Print PMF(k), CDF(k), mean np, variance np(1-p)
    # Each rounded to 4 decimal places
    pass

binomial_full(10, 0.3, 3)
`,

	solution: `import math

def binomial_full(n, p, k):
    pmf = math.comb(n, k) * p**k * (1 - p)**(n - k)
    cdf = sum(math.comb(n, i) * p**i * (1 - p)**(n - i) for i in range(k + 1))
    mean = n * p
    var  = n * p * (1 - p)
    print(round(pmf, 4))
    print(round(cdf, 4))
    print(round(mean, 4))
    print(round(var, 4))

binomial_full(10, 0.3, 3)
`,

	tests: [
		{
			name: "binomial_full(10, 0.3, 3): PMF=0.2668, CDF=0.6496, mean=3.0, var=2.1",
			expected: "0.2668\n0.6496\n3.0\n2.1\n",
		},
		{
			name: "binomial_full(5, 0.5, 2): PMF=0.3125, CDF=0.5, mean=2.5, var=1.25",
			code: `{{FUNC}}
binomial_full(5, 0.5, 2)`,
			expected: "0.3125\n0.5\n2.5\n1.25\n",
		},
		{
			name: "Bernoulli(0.7): binomial_full(1, 0.7, 1)",
			code: `{{FUNC}}
binomial_full(1, 0.7, 1)`,
			expected: "0.7\n1.0\n0.7\n0.21\n",
		},
		{
			name: "binomial_full(20, 0.1, 0): PMF=0.1216, CDF=0.1216, mean=2.0, var=1.8",
			code: `{{FUNC}}
binomial_full(20, 0.1, 0)`,
			expected: "0.1216\n0.1216\n2.0\n1.8\n",
		},
	],
};
