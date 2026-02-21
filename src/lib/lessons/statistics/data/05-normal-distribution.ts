import type { Lesson } from "../../types";

export const normalDistribution: Lesson = {
	id: "normal-distribution",
	title: "Normal Distribution",
	chapterId: "distributions",
	content: `## The Bell Curve

The **normal distribution** is the most important distribution in statistics. It is symmetric around the mean and fully described by two parameters: mean $\mu$ and standard deviation $\sigma$. We write $X \sim \mathcal{N}(\mu, \sigma^2)$.

The probability density function is:

$$f(x) = \frac{1}{\sigma\sqrt{2\pi}}\exp\!\left(-\frac{(x-\mu)^2}{2\sigma^2}\right)$$

\`\`\`python
import math

def normal_cdf(x, mu=0, sigma=1):
    return 0.5 * (1 + math.erf((x - mu) / (sigma * math.sqrt(2))))

def normal_pdf(x, mu=0, sigma=1):
    return (1 / (sigma * math.sqrt(2 * math.pi))) * math.exp(-0.5 * ((x - mu) / sigma) ** 2)

# Standard normal: mu=0, sigma=1
print(normal_cdf(0))          # 0.5
print(round(normal_pdf(0), 4))  # 0.3989
\`\`\`

### CDF vs PDF

- **PDF** (probability density function): the height of the curve at $x$. Not a probability itself.
- **CDF** (cumulative distribution function): $P(X \leq x)$. This IS a probability (0 to 1).

### The 68-95-99.7 Rule

For a normal distribution $\mathcal{N}(\mu, \sigma^2)$:
- $P(\mu - \sigma \leq X \leq \mu + \sigma) \approx$ **68%**
- $P(\mu - 2\sigma \leq X \leq \mu + 2\sigma) \approx$ **95%**
- $P(\mu - 3\sigma \leq X \leq \mu + 3\sigma) \approx$ **99.7%**

### Your Task

Implement \`normal_stats(mu, sigma, x)\` that prints:
1. \`CDF(x)\` — the probability that $X \leq x$ (rounded to 4 decimal places)
2. \`PDF(x)\` — the density at $x$ (rounded to 4 decimal places)`,

	starterCode: `import math

def normal_stats(mu, sigma, x):
    # Print CDF(x) and PDF(x) of N(mu, sigma), each rounded to 4 decimal places
    pass

normal_stats(0, 1, 0)
`,

	solution: `import math

def normal_stats(mu, sigma, x):
    cdf = 0.5 * (1 + math.erf((x - mu) / (sigma * math.sqrt(2))))
    pdf = (1 / (sigma * math.sqrt(2 * math.pi))) * math.exp(-0.5 * ((x - mu) / sigma) ** 2)
    print(round(cdf, 4))
    print(round(pdf, 4))

normal_stats(0, 1, 0)
`,

	tests: [
		{
			name: "normal_stats(0,1,0) → CDF=0.5, PDF=0.3989",
			expected: "0.5\n0.3989\n",
		},
		{
			name: "normal_stats(0,1,1) → CDF=0.8413, PDF=0.242",
			code: `{{FUNC}}
normal_stats(0, 1, 1)`,
			expected: "0.8413\n0.242\n",
		},
		{
			name: "CDF at mean is always 0.5",
			code: `{{FUNC}}
normal_stats(10, 2, 10)`,
			expected: "0.5\n0.1995\n",
		},
		{
			name: "normal_stats(0,1,-1) → CDF=0.1587, PDF=0.242",
			code: `{{FUNC}}
normal_stats(0, 1, -1)`,
			expected: "0.1587\n0.242\n",
		},
	],
};
