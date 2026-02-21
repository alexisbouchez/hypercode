import type { Lesson } from "../../types";

export const logisticGrowth: Lesson = {
	id: "logistic-growth",
	title: "Logistic Growth",
	chapterId: "first-order-models",
	content: `## Logistic Growth

Pure exponential growth is unrealistic — populations cannot grow forever. The **logistic equation** adds a carrying capacity $K$:

$$\\frac{dy}{dt} = r \\cdot y \\cdot \\left(1 - \\frac{y}{K}\\right)$$

- When $y \\ll K$: growth is approximately exponential ($1 - y/K \\approx 1$)
- When $y = K$: growth stops ($1 - y/K = 0$)
- When $y > K$: growth is negative (population declines toward $K$)

### Exact Solution

$$y(t) = \\frac{K}{1 + \\dfrac{K - y_0}{y_0}\\, e^{-rt}}$$

The solution forms an **S-shaped curve** (sigmoid), starting slow, accelerating, then leveling off at $K$.

### Applications

- Population ecology: bacteria, fish stocks, human populations
- Epidemiology: spread of diseases before herd immunity
- Technology adoption: product diffusion models (Bass model)
- Neural networks: sigmoid activation function

### Equilibria

The logistic equation has two equilibria:
- $y = 0$: unstable (any small population grows)
- $y = K$: stable (perturbations return to carrying capacity)

### Your Task

Implement \`logistic(r, K, y0, t_end, n)\` using Euler's method.`,

	starterCode: `def logistic(r, K, y0, t_end, n):
    h = t_end / n
    y = float(y0)
    for _ in range(n):
        y = y + h * r * y * (1 - y / K)
    return y

# Starting well below K=100, growth approaches K
print(logistic(2, 100, 10, 10, 10000) > 95)  # True

# At carrying capacity, no change
print(round(logistic(1, 100, 100, 10, 1000), 5))  # 100.0
`,

	solution: `def logistic(r, K, y0, t_end, n):
    h = t_end / n
    y = float(y0)
    for _ in range(n):
        y = y + h * r * y * (1 - y / K)
    return y

print(logistic(2, 100, 10, 10, 10000) > 95)
print(round(logistic(1, 100, 100, 10, 1000), 5))
`,

	tests: [
		{
			name: "at carrying capacity, y stays at K",
			code: `{{FUNC}}
print(round(logistic(1, 100, 100.0, 10, 1000), 5))`,
			expected: "100.0\n",
		},
		{
			name: "below K, population grows toward K",
			code: `{{FUNC}}
print(logistic(2, 100, 10, 10, 10000) > 95)`,
			expected: "True\n",
		},
		{
			name: "above K, population declines toward K",
			code: `{{FUNC}}
print(logistic(2, 100, 200, 10, 10000) < 105)`,
			expected: "True\n",
		},
		{
			name: "tiny population eventually reaches K",
			code: `{{FUNC}}
print(logistic(1, 1000, 1, 100, 100000) > 999)`,
			expected: "True\n",
		},
	],
};
