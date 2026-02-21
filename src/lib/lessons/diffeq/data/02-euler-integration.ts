import type { Lesson } from "../../types";

export const eulerIntegration: Lesson = {
	id: "euler-integration",
	title: "Euler Integration",
	chapterId: "numerical-methods",
	content: `## Euler Integration

A single Euler step gives one point. To approximate an entire trajectory, we repeat the step $n$ times from $t_0$ to $t_{\\text{end}}$.

### Algorithm

\`\`\`python
def euler(f, t0, y0, t_end, n):
    h = (t_end - t0) / n
    t, y = t0, y0
    ys = [y0]
    for _ in range(n):
        y = y + h * f(t, y)
        t += h
        ys.append(y)
    return ys
\`\`\`

The step size is $h = (t_{\\text{end}} - t_0) / n$. Smaller $h$ (more steps) gives higher accuracy, at the cost of more computation.

### Accuracy vs. Step Size

Euler's method has **first-order** accuracy: if you halve $h$, the global error halves too. For the ODE $\\frac{dy}{dt} = y$ with $y(0) = 1$:

| n steps | y(1) approximation | exact e ≈ 2.71828 |
|---------|-------------------|-------------------|
| 10      | 2.5937            | 2.71828           |
| 100     | 2.7048            | 2.71828           |
| 1000    | 2.7169            | 2.71828           |

### Your Task

Implement \`euler(f, t0, y0, t_end, n)\` that returns a list of $n+1$ values (including the initial value).`,

	starterCode: `def euler(f, t0, y0, t_end, n):
    h = (t_end - t0) / n
    t, y = t0, y0
    ys = [y0]
    for _ in range(n):
        y = y + h * f(t, y)
        t += h
        ys.append(y)
    return ys

# Exponential growth: dy/dt = y, y(0) = 1
result = euler(lambda t, y: y, 0, 1.0, 1.0, 10)
print(len(result))         # 11 values
print(round(result[-1], 3))  # ~2.594 (Euler approx of e)
`,

	solution: `def euler(f, t0, y0, t_end, n):
    h = (t_end - t0) / n
    t, y = t0, y0
    ys = [y0]
    for _ in range(n):
        y = y + h * f(t, y)
        t += h
        ys.append(y)
    return ys

result = euler(lambda t, y: y, 0, 1.0, 1.0, 10)
print(len(result))
print(round(result[-1], 3))
`,

	tests: [
		{
			name: "returns n+1 values",
			code: `{{FUNC}}
print(len(euler(lambda t, y: 0, 0, 0.0, 1.0, 4)))`,
			expected: "5\n",
		},
		{
			name: "first element is y0",
			code: `{{FUNC}}
print(euler(lambda t, y: y, 0, 3.0, 1.0, 10)[0])`,
			expected: "3.0\n",
		},
		{
			name: "constant derivative reaches t_end",
			code: `{{FUNC}}
print(round(euler(lambda t, y: 1, 0, 0.0, 1.0, 10)[-1], 6))`,
			expected: "1.0\n",
		},
		{
			name: "approximates e^(-1) for decay",
			code: `{{FUNC}}
print(round(euler(lambda t, y: -y, 0, 1.0, 1.0, 10000)[-1], 3))`,
			expected: "0.368\n",
		},
	],
};
