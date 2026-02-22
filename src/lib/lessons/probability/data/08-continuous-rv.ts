import type { Lesson } from "../../types";

export const continuousRv: Lesson = {
	id: "continuous-rv",
	title: "Continuous Random Variables",
	chapterId: "random-variables",
	content: `## Density Instead of Mass

A **continuous random variable** is described by a **probability density function** (PDF) $f(x)$, where:

$$P(a \\leq X \\leq b) = \\int_a^b f(x)\\, dx$$

Note that $P(X = x) = 0$ for any single point — only intervals have positive probability.

### Cumulative Distribution Function

$$F(x) = P(X \\leq x) = \\int_{-\\infty}^x f(t)\\, dt$$

### The Uniform Distribution

$X \\sim \\text{Uniform}(a, b)$ assigns equal density to every point in $[a, b]$:

$$f(x) = \\frac{1}{b-a}, \\quad a \\leq x \\leq b$$

$$E[X] = \\frac{a+b}{2}, \\qquad \\text{Var}(X) = \\frac{(b-a)^2}{12}$$

\`\`\`python
a, b = 0, 1
mean = (a + b) / 2
var  = (b - a)**2 / 12

print(round(mean, 4))  # 0.5
print(round(var, 4))   # 0.0833
\`\`\`

### Your Task

Implement \`uniform_stats(a, b)\` that prints $E[X]$ and $\\text{Var}(X)$ of $\\text{Uniform}(a, b)$, each rounded to 4 decimal places.`,

	starterCode: `def uniform_stats(a, b):
    # E[X] = (a+b)/2, Var(X) = (b-a)^2 / 12
    # Print both rounded to 4 decimal places
    pass

uniform_stats(0, 1)  # 0.5 then 0.0833
`,

	solution: `def uniform_stats(a, b):
    mean = (a + b) / 2
    var  = (b - a)**2 / 12
    print(round(mean, 4))
    print(round(var, 4))

uniform_stats(0, 1)
`,

	tests: [
		{
			name: "Uniform(0,1): mean=0.5, var=0.0833",
			expected: "0.5\n0.0833\n",
		},
		{
			name: "Uniform(0,2): mean=1.0, var=0.3333",
			code: `{{FUNC}}
uniform_stats(0, 2)`,
			expected: "1.0\n0.3333\n",
		},
		{
			name: "Uniform(1,5): mean=3.0, var=1.3333",
			code: `{{FUNC}}
uniform_stats(1, 5)`,
			expected: "3.0\n1.3333\n",
		},
		{
			name: "Uniform(2,8): mean=5.0, var=3.0",
			code: `{{FUNC}}
uniform_stats(2, 8)`,
			expected: "5.0\n3.0\n",
		},
	],
};
