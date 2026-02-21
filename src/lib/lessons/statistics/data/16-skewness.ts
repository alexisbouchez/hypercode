import type { Lesson } from "../../types";

export const skewness: Lesson = {
	id: "skewness",
	title: "Skewness",
	chapterId: "descriptive",
	content: `## Skewness

**Skewness** measures the asymmetry of a distribution. It tells you whether the data leans left or right relative to the mean.

### Formula

The **population skewness** is the third standardized moment:

$$\\text{skewness} = \\frac{\\frac{1}{n}\\sum_{i=1}^n (x_i - \\bar{x})^3}{\\sigma^3}$$

where $\\bar{x}$ is the mean and $\\sigma$ is the **population** standard deviation (divide by $n$, not $n-1$).

### Interpretation

- **Positive skew** (right-tailed): the tail extends to the right; mean > median
  - Example: income distributions, waiting times
- **Negative skew** (left-tailed): the tail extends to the left; mean < median
  - Example: exam scores when most students do well
- **Zero skew**: symmetric distribution (e.g., normal distribution)

\`\`\`
Right-skewed:  ▓▓▓▓▓▓▓░░░░     (long tail to the right)
Symmetric:     ░░░▓▓▓▓▓░░░
Left-skewed:   ░░░░▓▓▓▓▓▓▓     (long tail to the left)
\`\`\`

### Example

\`\`\`python
data = [0, 0, 3]
mean = 1.0
deviations = [-1, -1, 2]
m3 = ((-1)**3 + (-1)**3 + 2**3) / 3  # = 2.0
std = (2.0) ** 0.5                    # population std
skewness = 2.0 / std**3               # = 0.7071
\`\`\`

### Your Task

Implement \`skewness(data)\` that returns the population skewness rounded to 4 decimal places.`,

	starterCode: `def skewness(data):
    n = len(data)
    mean = sum(data) / n
    # population std (divide by n)
    # third standardized moment
    return 0.0

print(skewness([1, 2, 3]))   # symmetric → 0.0
print(skewness([0, 0, 3]))   # right-skewed → 0.7071
`,

	solution: `def skewness(data):
    n = len(data)
    mean = sum(data) / n
    variance = sum((x - mean)**2 for x in data) / n
    std = variance ** 0.5
    if std == 0:
        return 0.0
    m3 = sum((x - mean)**3 for x in data) / n
    return round(m3 / std**3, 4)

print(skewness([1, 2, 3]))
print(skewness([0, 0, 3]))
`,

	tests: [
		{
			name: "symmetric [1,2,3] → skewness 0.0",
			expected: "0.0\n0.7071\n",
		},
		{
			name: "constant data → skewness 0.0",
			code: `{{FUNC}}
print(skewness([5, 5, 5, 5]))`,
			expected: "0.0\n",
		},
		{
			name: "right-skewed [0,0,3] → 0.7071",
			code: `{{FUNC}}
print(skewness([0, 0, 3]))`,
			expected: "0.7071\n",
		},
		{
			name: "left-skewed [0,3,3] → -0.7071",
			code: `{{FUNC}}
print(skewness([0, 3, 3]))`,
			expected: "-0.7071\n",
		},
		{
			name: "longer symmetric [1,2,3,4,5] → 0.0",
			code: `{{FUNC}}
print(skewness([1, 2, 3, 4, 5]))`,
			expected: "0.0\n",
		},
	],
};
