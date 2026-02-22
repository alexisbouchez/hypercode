import type { Lesson } from "../../types";

export const discreteRv: Lesson = {
	id: "discrete-rv",
	title: "Discrete Random Variables",
	chapterId: "random-variables",
	content: `## From Events to Numbers

A **random variable** $X$ is a function that assigns a number to each outcome in $\\Omega$. A **discrete** random variable takes countably many values.

### Probability Mass Function (PMF)

$p(x) = P(X = x)$ — the probability of each value. Must satisfy:
$$\\sum_x p(x) = 1, \\quad p(x) \\geq 0$$

### Expectation

$$E[X] = \\sum_x x \\cdot P(X = x)$$

The long-run average value of $X$ over many repetitions.

### Variance

$$\\text{Var}(X) = E[X^2] - (E[X])^2 = \\sum_x x^2 \\cdot P(X=x) - \\left(\\sum_x x \\cdot P(X=x)\\right)^2$$

Variance measures spread around the mean.

\`\`\`python
# Fair die: values 1-6, each with prob 1/6
values = [1, 2, 3, 4, 5, 6]
probs  = [1/6] * 6

mean = sum(x * p for x, p in zip(values, probs))
var  = sum(x**2 * p for x, p in zip(values, probs)) - mean**2

print(round(mean, 4))  # 3.5
print(round(var, 4))   # 2.9167
\`\`\`

### Your Task

Implement \`discrete_stats(values, probs)\` that prints $E[X]$ and $\\text{Var}(X)$, each rounded to 4 decimal places.`,

	starterCode: `def discrete_stats(values, probs):
    # Print E[X] and Var(X), each rounded to 4 decimal places
    pass

discrete_stats([0, 1], [0.5, 0.5])  # Bernoulli(0.5)
`,

	solution: `def discrete_stats(values, probs):
    mean = sum(x * p for x, p in zip(values, probs))
    var  = sum(x**2 * p for x, p in zip(values, probs)) - mean**2
    print(round(mean, 4))
    print(round(var, 4))

discrete_stats([0, 1], [0.5, 0.5])
`,

	tests: [
		{
			name: "Bernoulli(0.5): mean=0.5, var=0.25",
			expected: "0.5\n0.25\n",
		},
		{
			name: "uniform {1,2,3}: mean=2.0, var=0.6667",
			code: `{{FUNC}}
discrete_stats([1, 2, 3], [1/3, 1/3, 1/3])`,
			expected: "2.0\n0.6667\n",
		},
		{
			name: "symmetric {0,1,2}: mean=1.0, var=0.5",
			code: `{{FUNC}}
discrete_stats([0, 1, 2], [0.25, 0.5, 0.25])`,
			expected: "1.0\n0.5\n",
		},
		{
			name: "degenerate (constant): mean=1.0, var=0.0",
			code: `{{FUNC}}
discrete_stats([1], [1.0])`,
			expected: "1.0\n0.0\n",
		},
	],
};
