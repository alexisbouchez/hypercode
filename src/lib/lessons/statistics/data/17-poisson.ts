import type { Lesson } from "../../types";

export const poissonDistribution: Lesson = {
	id: "poisson",
	title: "Poisson Distribution",
	chapterId: "distributions",
	content: `## The Poisson Distribution

The **Poisson distribution** models the number of events occurring in a fixed interval of time or space, when events happen independently at a constant average rate $\\lambda$.

### PMF

The probability of observing exactly $k$ events:

$$P(X = k) = \\frac{e^{-\\lambda} \\lambda^k}{k!} \\quad k = 0, 1, 2, \\ldots$$

- **Mean**: $E[X] = \\lambda$
- **Variance**: $\\text{Var}(X) = \\lambda$

### Real-World Examples

| Scenario | $\\lambda$ |
|----------|-----------|
| Calls to a call centre per hour | e.g. 10 |
| Mutations per DNA strand | e.g. 0.001 |
| Accidents on a road per month | e.g. 2 |
| Emails received per minute | e.g. 3 |

### Examples

For $\\lambda = 1$:
$$P(X=0) = e^{-1} \\approx 0.3679 \\qquad P(X=1) = e^{-1} \\approx 0.3679$$

For $\\lambda = 3$:
$$P(X=3) = \\frac{e^{-3} \\cdot 27}{6} \\approx 0.2240$$

### Implementation

\`\`\`python
import math

def poisson_pmf(lam, k):
    return round(math.exp(-lam) * lam**k / math.factorial(k), 4)
\`\`\`

### Relationship to Binomial

When $n$ is large and $p$ is small with $np = \\lambda$, the binomial distribution $B(n,p)$ approaches Poisson$( \\lambda)$. This is why Poisson appears in rare-event scenarios.

### Your Task

Implement \`poisson_pmf(lam, k)\` that returns $P(X=k)$ for a Poisson distribution with rate $\\lambda$, rounded to 4 decimal places.`,

	starterCode: `import math

def poisson_pmf(lam, k):
    # P(X=k) = exp(-lam) * lam^k / k!
    return 0.0

print(poisson_pmf(1, 0))   # 0.3679
print(poisson_pmf(1, 1))   # 0.3679
print(poisson_pmf(3, 3))   # 0.2240
`,

	solution: `import math

def poisson_pmf(lam, k):
    return round(math.exp(-lam) * lam**k / math.factorial(k), 4)

print(poisson_pmf(1, 0))
print(poisson_pmf(1, 1))
print(poisson_pmf(3, 3))
`,

	tests: [
		{
			name: "P(0|λ=1)=0.3679, P(1|λ=1)=0.3679, P(3|λ=3)≈0.224",
			expected: "0.3679\n0.3679\n0.224\n",
		},
		{
			name: "P(0|λ=0) = 1 (no events, rate=0)",
			code: `{{FUNC}}
print(poisson_pmf(0, 0))`,
			expected: "1.0\n",
		},
		{
			name: "P(0|λ=2) = e^{-2} ≈ 0.1353",
			code: `{{FUNC}}
print(poisson_pmf(2, 0))`,
			expected: "0.1353\n",
		},
		{
			name: "P(2|λ=2) = 2e^{-2} ≈ 0.2707",
			code: `{{FUNC}}
print(poisson_pmf(2, 2))`,
			expected: "0.2707\n",
		},
		{
			name: "P(5|λ=5) = e^{-5}*5^5/120 ≈ 0.1755",
			code: `{{FUNC}}
print(poisson_pmf(5, 5))`,
			expected: "0.1755\n",
		},
	],
};
