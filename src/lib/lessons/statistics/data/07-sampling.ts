import type { Lesson } from "../../types";

export const sampling: Lesson = {
	id: "sampling",
	title: "Random Sampling",
	chapterId: "distributions",
	content: `## Sampling from a Population

We rarely have access to an entire population. Instead, we take a **sample** — a random subset — and use it to draw conclusions about the population.

\`\`\`python
import numpy as np

population = list(range(1, 101))   # integers 1 to 100
rng = np.random.default_rng(seed=42)

sample = rng.choice(population, size=10, replace=False)
print(sample)
print(round(np.mean(sample), 2))   # close to 50.5 (true mean)
\`\`\`

### Why Use a Seed?

Setting a random seed makes results **reproducible** — running the same code gives the same "random" numbers. Use \`np.random.default_rng(seed)\` for modern NumPy.

### Sampling With vs Without Replacement

- **Without replacement** (\`replace=False\`): each element can only be chosen once. Used for surveys.
- **With replacement** (\`replace=True\`): elements can be chosen multiple times. Used for bootstrapping.

### Law of Large Numbers

As sample size increases, the sample mean converges to the true population mean.

\`\`\`python
# Larger samples are more accurate
for n in [5, 20, 100]:
    sample = rng.choice(population, size=n, replace=False)
    print(f"n={n}: mean={round(np.mean(sample), 1)}")
\`\`\`

### Your Task

Implement \`sample_mean(population, n, seed)\` that takes a random sample of size \`n\` (without replacement) using the given seed and returns the sample mean rounded to 2 decimal places.`,

	starterCode: `import numpy as np

def sample_mean(population, n, seed):
    # Take random sample of size n (without replacement) and return its mean (round 2)
    pass

pop = list(range(1, 11))  # 1 to 10, true mean = 5.5
print(sample_mean(pop, 10, 42) == 5.5)   # True: sample of all 10 = population
`,

	solution: `import numpy as np

def sample_mean(population, n, seed):
    rng = np.random.default_rng(seed)
    sample = rng.choice(population, size=n, replace=False)
    return round(float(np.mean(sample)), 2)

pop = list(range(1, 11))  # 1 to 10, true mean = 5.5
print(sample_mean(pop, 10, 42) == 5.5)   # True: sample of all 10 = population
`,

	tests: [
		{
			name: "sample of full population has exact mean 5.5",
			expected: "True\n",
		},
		{
			name: "sample mean is within reasonable range",
			code: `{{FUNC}}
m = sample_mean(list(range(1, 101)), 50, 42)
print(20 <= m <= 80)`,
			expected: "True\n",
		},
		{
			name: "different seed gives different sample mean",
			code: `{{FUNC}}
m1 = sample_mean(list(range(1, 101)), 5, 42)
m2 = sample_mean(list(range(1, 101)), 5, 99)
print(isinstance(m1, float) and isinstance(m2, float))`,
			expected: "True\n",
		},
		{
			name: "sample of size 1 returns that element as mean",
			code: `{{FUNC}}
pop = [7]
print(sample_mean(pop, 1, 0) == 7.0)`,
			expected: "True\n",
		},
	],
};
