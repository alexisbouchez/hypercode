import type { Lesson } from "../../types";

export const poissonExponential: Lesson = {
	id: "poisson-exponential",
	title: "Poisson & Exponential Distributions",
	chapterId: "distributions",
	content: `## Rare Events and Waiting Times

### Poisson Distribution

$X \\sim \\text{Poisson}(\\lambda)$ counts the number of rare events in a fixed interval, where $\\lambda$ is the average rate:

$$P(X = k) = \\frac{e^{-\\lambda} \\lambda^k}{k!}$$

$$E[X] = \\lambda, \\qquad \\text{Var}(X) = \\lambda$$

Examples: customers arriving per hour, mutations per genome, photons hitting a detector.

### Exponential Distribution

$T \\sim \\text{Exponential}(\\lambda)$ models the *waiting time* between consecutive Poisson events:

$$f(t) = \\lambda e^{-\\lambda t}, \\quad t \\geq 0$$

$$F(t) = P(T \\leq t) = 1 - e^{-\\lambda t}$$

$$E[T] = \\frac{1}{\\lambda}, \\qquad \\text{Var}(T) = \\frac{1}{\\lambda^2}$$

**Memoryless property**: $P(T > s + t \\mid T > s) = P(T > t)$. The distribution forgets how long you have already waited.

\`\`\`python
import math

lam = 2.0  # 2 events per hour on average

# P(exactly 2 events in one hour)
pmf = math.exp(-lam) * lam**2 / math.factorial(2)
print(round(pmf, 4))   # 0.2707

# P(wait ≤ 1 hour for next event)
cdf = 1 - math.exp(-lam * 1.0)
print(round(cdf, 4))   # 0.8647
\`\`\`

### Your Task

Implement \`poisson_and_exponential(lam, k, x)\` that prints:
1. Poisson PMF: $P(K=k)$
2. Exponential CDF: $P(T \\leq x)$
3. Exponential mean: $1/\\lambda$

All rounded to 4 decimal places.`,

	starterCode: `import math

def poisson_and_exponential(lam, k, x):
    # Print Poisson PMF P(K=k), Exponential CDF P(T<=x), mean 1/lam
    # All rounded to 4 decimal places
    pass

poisson_and_exponential(2.0, 2, 1.0)
`,

	solution: `import math

def poisson_and_exponential(lam, k, x):
    pmf  = math.exp(-lam) * lam**k / math.factorial(k)
    cdf  = 1 - math.exp(-lam * x)
    mean = 1 / lam
    print(round(pmf, 4))
    print(round(cdf, 4))
    print(round(mean, 4))

poisson_and_exponential(2.0, 2, 1.0)
`,

	tests: [
		{
			name: "lam=2: PMF(2)=0.2707, CDF(1.0)=0.8647, mean=0.5",
			expected: "0.2707\n0.8647\n0.5\n",
		},
		{
			name: "lam=1: PMF(0)=0.3679, CDF(0.0)=0.0, mean=1.0",
			code: `{{FUNC}}
poisson_and_exponential(1.0, 0, 0.0)`,
			expected: "0.3679\n0.0\n1.0\n",
		},
		{
			name: "lam=3: PMF(3)=0.224, CDF(0.5)=0.7769, mean=0.3333",
			code: `{{FUNC}}
poisson_and_exponential(3.0, 3, 0.5)`,
			expected: "0.224\n0.7769\n0.3333\n",
		},
		{
			name: "lam=0.5: PMF(1)=0.3033, CDF(2.0)=0.6321, mean=2.0",
			code: `{{FUNC}}
poisson_and_exponential(0.5, 1, 2.0)`,
			expected: "0.3033\n0.6321\n2.0\n",
		},
	],
};
