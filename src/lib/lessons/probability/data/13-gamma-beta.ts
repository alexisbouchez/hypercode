import type { Lesson } from "../../types";

export const gammaBeta: Lesson = {
	id: "gamma-beta",
	title: "Gamma & Beta Distributions",
	chapterId: "distributions",
	content: `## Two Distributions with Flexible Shapes

### Gamma Distribution

$X \\sim \\text{Gamma}(\\alpha, \\beta)$ generalizes the exponential. It is the distribution of the waiting time for $\\alpha$ events in a Poisson process with rate $\\beta$:

$$f(x) = \\frac{\\beta^\\alpha}{\\Gamma(\\alpha)} x^{\\alpha-1} e^{-\\beta x}, \\quad x > 0$$

$$E[X] = \\frac{\\alpha}{\\beta}, \\qquad \\text{Var}(X) = \\frac{\\alpha}{\\beta^2}$$

Special case: $\\text{Gamma}(1, \\beta) = \\text{Exponential}(\\beta)$.

### Beta Distribution

$X \\sim \\text{Beta}(\\alpha, \\beta)$ lives on $[0, 1]$ and models probabilities and proportions:

$$f(x) = \\frac{x^{\\alpha-1}(1-x)^{\\beta-1}}{B(\\alpha,\\beta)}, \\quad 0 \\leq x \\leq 1$$

$$E[X] = \\frac{\\alpha}{\\alpha+\\beta}, \\qquad \\text{Var}(X) = \\frac{\\alpha\\beta}{(\\alpha+\\beta)^2(\\alpha+\\beta+1)}$$

**Bayesian statistics**: the Beta distribution is the conjugate prior for the Bernoulli/Binomial likelihood. After observing $k$ successes in $n$ trials with a $\\text{Beta}(1,1)$ (uniform) prior, the posterior is $\\text{Beta}(k+1, n-k+1)$.

\`\`\`python
# Beta(2, 5): modeling a conversion rate
alpha, beta = 2, 5
mean = alpha / (alpha + beta)
var  = alpha * beta / ((alpha + beta)**2 * (alpha + beta + 1))
print(round(mean, 4))  # 0.2857
print(round(var, 4))   # 0.0255
\`\`\`

### Your Task

Implement two functions:
- \`gamma_stats(alpha, beta)\` — prints $E[X]$ and $\\text{Var}(X)$ of $\\text{Gamma}(\\alpha, \\beta)$
- \`beta_stats(alpha, beta)\` — prints $E[X]$ and $\\text{Var}(X)$ of $\\text{Beta}(\\alpha, \\beta)$

Call both in sequence: first \`gamma_stats(2.0, 1.0)\`, then \`beta_stats(2.0, 2.0)\`.`,

	starterCode: `def gamma_stats(alpha, beta):
    # E[X] = alpha/beta, Var(X) = alpha/beta^2
    # Print both rounded to 4 decimal places
    pass

def beta_stats(alpha, beta):
    # E[X] = alpha/(alpha+beta)
    # Var(X) = alpha*beta / ((alpha+beta)^2 * (alpha+beta+1))
    # Print both rounded to 4 decimal places
    pass

gamma_stats(2.0, 1.0)
beta_stats(2.0, 2.0)
`,

	solution: `def gamma_stats(alpha, beta):
    print(round(alpha / beta, 4))
    print(round(alpha / beta**2, 4))

def beta_stats(alpha, beta):
    mean = alpha / (alpha + beta)
    var  = alpha * beta / ((alpha + beta)**2 * (alpha + beta + 1))
    print(round(mean, 4))
    print(round(var, 4))

gamma_stats(2.0, 1.0)
beta_stats(2.0, 2.0)
`,

	tests: [
		{
			name: "gamma(2,1) then beta(2,2): 2.0,2.0,0.5,0.05",
			expected: "2.0\n2.0\n0.5\n0.05\n",
		},
		{
			name: "gamma(3, 2): mean=1.5, var=0.75",
			code: `{{FUNC}}
gamma_stats(3.0, 2.0)`,
			expected: "1.5\n0.75\n",
		},
		{
			name: "beta(1,1) = Uniform(0,1): mean=0.5, var=0.0833",
			code: `{{FUNC}}
beta_stats(1.0, 1.0)`,
			expected: "0.5\n0.0833\n",
		},
		{
			name: "beta(2, 5): mean=0.2857, var=0.0255",
			code: `{{FUNC}}
beta_stats(2.0, 5.0)`,
			expected: "0.2857\n0.0255\n",
		},
	],
};
