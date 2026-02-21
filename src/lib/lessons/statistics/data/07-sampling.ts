import type { Lesson } from "../../types";

export const sampling: Lesson = {
	id: "sampling",
	title: "Random Sampling",
	chapterId: "distributions",
	content: `## Sampling from a Population

We rarely have access to an entire population. Instead, we take a **sample** — a random subset — and use it to draw conclusions about the population.

\`\`\`python
import random

population = list(range(1, 101))   # integers 1 to 100
rng = random.Random(42)

sample = rng.sample(population, 10)   # without replacement
print(round(sum(sample) / len(sample), 2))   # close to 50.5 (true mean)
\`\`\`

### Why Use a Seed?

Setting a random seed makes results **reproducible** — running the same code gives the same "random" numbers. Use \`random.Random(seed)\` to create a seeded instance.

### Sampling With vs Without Replacement

- **Without replacement** (\`random.sample\`): each element can only be chosen once. Used for surveys.
- **With replacement** (\`random.choices\`): elements can be chosen multiple times. Used for bootstrapping.

### Law of Large Numbers

As sample size $n$ increases, the sample mean $\bar{x}$ converges to the true population mean $\mu$:

$$\bar{x} = \frac{1}{n}\sum_{i=1}^n x_i \xrightarrow{n \to \infty} \mu$$

\`\`\`python
# Larger samples are more accurate
for n in [5, 20, 100]:
    s = rng.sample(population, n)
    print(f"n={n}: mean={round(sum(s)/len(s), 1)}")
\`\`\`

### Your Task

Implement \`sample_mean(population, n, seed)\` that takes a random sample of size $n$ (without replacement) using the given seed and returns the sample mean rounded to 2 decimal places.`,

	starterCode: `import random

def sample_mean(population, n, seed):
    # Take random sample of size n (without replacement) and return its mean (round 2)
    pass

pop = list(range(1, 11))  # 1 to 10, true mean = 5.5
print(sample_mean(pop, 10, 42) == 5.5)   # True: sample of all 10 = population
`,

	solution: `import random

def sample_mean(population, n, seed):
    rng = random.Random(seed)
    sample = rng.sample(population, n)
    return round(sum(sample) / len(sample), 2)

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
