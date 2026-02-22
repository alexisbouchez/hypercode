import type { Lesson } from "../../types";

export const lawOfLargeNumbers: Lesson = {
	id: "law-of-large-numbers",
	title: "Law of Large Numbers & CLT",
	chapterId: "limit-theorems",
	content: `## Convergence of Sample Averages

### Law of Large Numbers (LLN)

If $X_1, X_2, \\ldots, X_n$ are i.i.d. with mean $\\mu$, then the sample mean converges to $\\mu$ in probability as $n \\to \\infty$:

$$\\bar{X}_n = \\frac{1}{n}\\sum_{i=1}^n X_i \\xrightarrow{P} \\mu$$

The larger your sample, the closer the average is to the true mean.

### Central Limit Theorem (CLT)

Even more remarkable: regardless of the original distribution, the standardized sample mean converges to $N(0,1)$:

$$\\frac{\\bar{X}_n - \\mu}{\\sigma / \\sqrt{n}} \\xrightarrow{d} N(0, 1)$$

This is why the normal distribution appears everywhere — it is the universal limiting shape of averages.

### Box-Muller Transform

To generate $Z \\sim N(0,1)$ from uniform samples $U_1, U_2 \\sim \\text{Uniform}(0,1)$:

$$Z = \\sqrt{-2 \\ln U_1} \\cdot \\cos(2\\pi U_2)$$

\`\`\`python
import math, random

rng = random.Random(42)
u1, u2 = rng.random(), rng.random()
z = math.sqrt(-2 * math.log(u1)) * math.cos(2 * math.pi * u2)
# z is a single standard normal sample
\`\`\`

### Your Task

Implement \`sample_mean(n, mu, sigma, seed)\` using Box-Muller with \`random.Random(seed)\`. Return the sample mean of $n$ draws from $N(\\mu, \\sigma^2)$, rounded to 2 decimal places.`,

	starterCode: `import math
import random

def sample_mean(n, mu, sigma, seed):
    # Generate n samples from N(mu, sigma^2) using Box-Muller
    # Use random.Random(seed) for reproducibility
    # Return the sample mean rounded to 2 decimal places
    return 0.0

print(sample_mean(1000, 5.0, 1.0, 42))  # close to 5.0
`,

	solution: `import math
import random

def sample_mean(n, mu, sigma, seed):
    rng = random.Random(seed)
    vals = []
    for _ in range(n):
        u1 = rng.random()
        u2 = rng.random()
        z = math.sqrt(-2 * math.log(u1)) * math.cos(2 * math.pi * u2)
        vals.append(mu + sigma * z)
    return round(sum(vals) / len(vals), 2)

print(sample_mean(1000, 5.0, 1.0, 42))
`,

	tests: [
		{
			name: "n=1000, mu=5.0, sigma=1.0, seed=42 → 4.98",
			expected: "4.98\n",
		},
		{
			name: "n=100, mu=0.0, sigma=1.0, seed=7 → -0.03",
			code: `{{FUNC}}
print(sample_mean(100, 0.0, 1.0, 7))`,
			expected: "-0.03\n",
		},
		{
			name: "n=10000, mu=2.0, sigma=0.5, seed=99 → 2.0 (LLN convergence)",
			code: `{{FUNC}}
print(sample_mean(10000, 2.0, 0.5, 99))`,
			expected: "2.0\n",
		},
		{
			name: "n=100, mu=0.0, sigma=1.0, seed=123 → -0.07",
			code: `{{FUNC}}
print(sample_mean(100, 0.0, 1.0, 123))`,
			expected: "-0.07\n",
		},
	],
};
