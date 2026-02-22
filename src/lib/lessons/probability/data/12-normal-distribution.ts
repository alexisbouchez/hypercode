import type { Lesson } from "../../types";

export const normalDistribution: Lesson = {
	id: "normal-distribution",
	title: "Normal Distribution",
	chapterId: "distributions",
	content: `## The Bell Curve

$X \\sim N(\\mu, \\sigma^2)$ is the most important distribution in probability. Its PDF is:

$$f(x) = \\frac{1}{\\sigma\\sqrt{2\\pi}} \\exp\\left(-\\frac{(x-\\mu)^2}{2\\sigma^2}\\right)$$

$$E[X] = \\mu, \\qquad \\text{Var}(X) = \\sigma^2$$

### Standard Normal

$Z \\sim N(0, 1)$. Any normal can be standardized: $Z = (X - \\mu)/\\sigma$.

### CDF via Error Function

The CDF has no closed form in elementary functions, but Python provides it via \`math.erf\`:

$$\\Phi(x) = P(Z \\leq x) = \\frac{1}{2}\\left[1 + \\text{erf}\\left(\\frac{x}{\\sqrt{2}}\\right)\\right]$$

\`\`\`python
import math

def normal_cdf(mu, sigma, x):
    return 0.5 * (1 + math.erf((x - mu) / (sigma * math.sqrt(2))))

def normal_pdf(mu, sigma, x):
    return (1 / (sigma * math.sqrt(2 * math.pi))) * math.exp(-0.5 * ((x - mu) / sigma)**2)
\`\`\`

### The 68-95-99.7 Rule

| Range | Probability |
|---|---|
| $\\mu \\pm \\sigma$ | 68.27% |
| $\\mu \\pm 2\\sigma$ | 95.45% |
| $\\mu \\pm 3\\sigma$ | 99.73% |

### Why It's Everywhere

The **Central Limit Theorem** (next lesson) guarantees that the sum of many independent random variables converges to a normal distribution, regardless of the original distribution.

### Your Task

Implement \`normal_full(mu, sigma, x)\` that prints CDF$(x)$ then PDF$(x)$ of $N(\\mu, \\sigma^2)$, each rounded to 4 decimal places.`,

	starterCode: `import math

def normal_full(mu, sigma, x):
    # Print CDF(x) then PDF(x) of N(mu, sigma^2), rounded to 4 decimal places
    pass

normal_full(0, 1, 0)  # CDF=0.5, PDF=0.3989
`,

	solution: `import math

def normal_full(mu, sigma, x):
    cdf = 0.5 * (1 + math.erf((x - mu) / (sigma * math.sqrt(2))))
    pdf = (1 / (sigma * math.sqrt(2 * math.pi))) * math.exp(-0.5 * ((x - mu) / sigma)**2)
    print(round(cdf, 4))
    print(round(pdf, 4))

normal_full(0, 1, 0)
`,

	tests: [
		{
			name: "N(0,1) at x=0: CDF=0.5, PDF=0.3989",
			expected: "0.5\n0.3989\n",
		},
		{
			name: "N(0,1) at x=1 (one sigma above): CDF=0.8413, PDF=0.242",
			code: `{{FUNC}}
normal_full(0, 1, 1)`,
			expected: "0.8413\n0.242\n",
		},
		{
			name: "N(10,2) at mean: CDF=0.5, PDF=0.1995",
			code: `{{FUNC}}
normal_full(10, 2, 10)`,
			expected: "0.5\n0.1995\n",
		},
		{
			name: "N(0,1) at x=-1 (one sigma below): CDF=0.1587, PDF=0.242",
			code: `{{FUNC}}
normal_full(0, 1, -1)`,
			expected: "0.1587\n0.242\n",
		},
	],
};
