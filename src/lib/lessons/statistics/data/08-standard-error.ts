import type { Lesson } from "../../types";

export const standardError: Lesson = {
	id: "standard-error",
	title: "Standard Error & CLT",
	chapterId: "distributions",
	content: `## Standard Error

The **standard error (SE)** measures the variability of a sample mean. It tells you how much sample means vary from the true population mean:

\`\`\`
SE = σ / √n
\`\`\`

\`\`\`python
from scipy import stats

data = [1, 2, 3, 4, 5]
se = stats.sem(data)
print(round(se, 4))   # 0.7071
\`\`\`

### Central Limit Theorem (CLT)

One of the most important results in statistics: **regardless of the shape of the population distribution**, the distribution of sample means approaches a **normal distribution** as n increases.

This means:
- The mean of sample means ≈ population mean
- The std of sample means ≈ SE = σ/√n

### Implications

A larger sample size → smaller SE → more precise estimates.

\`\`\`python
import numpy as np

# SE decreases as sample grows
for n in [10, 100, 1000]:
    se = 10 / np.sqrt(n)   # σ=10
    print(f"n={n}: SE={round(se, 2)}")
# n=10: SE=3.16
# n=100: SE=1.0
# n=1000: SE=0.32
\`\`\`

### Your Task

Implement \`std_error(data)\` that computes the **standard error of the mean** using \`scipy.stats.sem\`, returned as a float rounded to 4 decimal places.`,

	starterCode: `from scipy import stats

def std_error(data):
    # Return the standard error of the mean, rounded to 4 decimal places
    pass

print(std_error([1, 2, 3, 4, 5]))
`,

	solution: `from scipy import stats

def std_error(data):
    return round(float(stats.sem(data)), 4)

print(std_error([1, 2, 3, 4, 5]))
`,

	tests: [
		{
			name: "std_error([1,2,3,4,5]) = 0.7071",
			expected: "0.7071\n",
		},
		{
			name: "std_error([10,20,30,40,50]) = 7.0711",
			code: `{{FUNC}}
print(std_error([10, 20, 30, 40, 50]))`,
			expected: "7.0711\n",
		},
		{
			name: "larger sample (same distribution) → smaller SE",
			code: `{{FUNC}}
small = std_error([1, 2, 3, 4, 5])
large = std_error([1, 2, 3, 4, 5] * 3)
print(large < small)`,
			expected: "True\n",
		},
		{
			name: "std_error of constant data is 0.0",
			code: `{{FUNC}}
print(std_error([5, 5, 5, 5, 5]))`,
			expected: "0.0\n",
		},
	],
};
