import type { Lesson } from "../../types";

export const expectationVariance: Lesson = {
	id: "expectation-variance",
	title: "Expectation & Variance Properties",
	chapterId: "random-variables",
	content: `## Rules for Transforming Random Variables

### Linearity of Expectation

For any constants $a$ and $b$:

$$E[aX + b] = a \\cdot E[X] + b$$

This holds for **all** random variables, regardless of dependence or distribution. It is the most useful fact in probability.

### Variance Under Linear Transformation

$$\\text{Var}(aX + b) = a^2 \\cdot \\text{Var}(X)$$

Adding a constant shifts the distribution but does not change spread. Scaling by $a$ stretches spread by $|a|$, so variance scales by $a^2$.

\`\`\`python
# X ~ Bernoulli(0.5): E[X] = 0.5, Var(X) = 0.25
# Y = 2X + 3
# E[Y] = 2*0.5 + 3 = 4.0
# Var(Y) = 4 * 0.25 = 1.0
mean_x, var_x = 0.5, 0.25
a, b = 2, 3
print(round(a * mean_x + b, 4))   # 4.0
print(round(a**2 * var_x, 4))     # 1.0
\`\`\`

### Sum of Independent Random Variables

$$E[X + Y] = E[X] + E[Y] \\quad \\text{(always)}$$
$$\\text{Var}(X + Y) = \\text{Var}(X) + \\text{Var}(Y) \\quad \\text{(if independent)}$$

### Your Task

Implement \`linear_transform(mean_x, var_x, a, b)\` that prints $E[aX+b]$ and $\\text{Var}(aX+b)$, each rounded to 4 decimal places.`,

	starterCode: `def linear_transform(mean_x, var_x, a, b):
    # E[aX+b] = a*mean_x + b
    # Var(aX+b) = a^2 * var_x
    # Print both rounded to 4 decimal places
    pass

linear_transform(2.0, 1.0, 3.0, 4.0)  # E=10.0, Var=9.0
`,

	solution: `def linear_transform(mean_x, var_x, a, b):
    print(round(a * mean_x + b, 4))
    print(round(a**2 * var_x, 4))

linear_transform(2.0, 1.0, 3.0, 4.0)
`,

	tests: [
		{
			name: "linear_transform(2.0, 1.0, 3.0, 4.0): E=10.0, Var=9.0",
			expected: "10.0\n9.0\n",
		},
		{
			name: "identity transform: linear_transform(0.0, 1.0, 1.0, 0.0)",
			code: `{{FUNC}}
linear_transform(0.0, 1.0, 1.0, 0.0)`,
			expected: "0.0\n1.0\n",
		},
		{
			name: "linear_transform(5.0, 4.0, 2.0, -1.0): E=9.0, Var=16.0",
			code: `{{FUNC}}
linear_transform(5.0, 4.0, 2.0, -1.0)`,
			expected: "9.0\n16.0\n",
		},
		{
			name: "constant shift only: linear_transform(3.0, 2.0, 0.0, 7.0)",
			code: `{{FUNC}}
linear_transform(3.0, 2.0, 0.0, 7.0)`,
			expected: "7.0\n0.0\n",
		},
	],
};
