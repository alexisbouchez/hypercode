import type { Lesson } from "../../types";

export const rk4Integration: Lesson = {
	id: "rk4-integration",
	title: "RK4 Integration",
	chapterId: "numerical-methods",
	content: `## RK4 Integration

Just as with Euler's method, we can chain multiple RK4 steps together to integrate over an interval.

### Algorithm

\`\`\`python
def rk4(f, t0, y0, t_end, n):
    h = (t_end - t0) / n
    t, y = t0, y0
    ys = [y0]
    for _ in range(n):
        k1 = f(t, y)
        k2 = f(t + h/2, y + h/2 * k1)
        k3 = f(t + h/2, y + h/2 * k2)
        k4 = f(t + h, y + h * k3)
        y = y + h/6 * (k1 + 2*k2 + 2*k3 + k4)
        t += h
        ys.append(y)
    return ys
\`\`\`

### RK4 vs Euler: A Comparison

For \`dy/dt = y\`, \`y(0) = 1\`, approximate \`y(1) = e ≈ 2.71828\`:

| Method  | n=10    | n=100   |
|---------|---------|---------|
| Euler   | 2.5937  | 2.7048  |
| RK4     | 2.7183  | 2.71828 |

With just 10 steps, RK4 matches \`e\` to 4 decimal places. Euler needs thousands of steps for similar accuracy.

### Your Task

Implement \`rk4(f, t0, y0, t_end, n)\` that applies \`n\` RK4 steps and returns a list of \`n+1\` values.`,

	starterCode: `def rk4(f, t0, y0, t_end, n):
    h = (t_end - t0) / n
    t, y = t0, y0
    ys = [y0]
    for _ in range(n):
        k1 = f(t, y)
        k2 = f(t + h/2, y + h/2 * k1)
        k3 = f(t + h/2, y + h/2 * k2)
        k4 = f(t + h, y + h * k3)
        y = y + h/6 * (k1 + 2*k2 + 2*k3 + k4)
        t += h
        ys.append(y)
    return ys

# dy/dt = y, y(0) = 1 → y(1) should equal e ≈ 2.71828
result = rk4(lambda t, y: y, 0, 1.0, 1.0, 10)
print(round(result[-1], 3))  # 2.718
`,

	solution: `def rk4(f, t0, y0, t_end, n):
    h = (t_end - t0) / n
    t, y = t0, y0
    ys = [y0]
    for _ in range(n):
        k1 = f(t, y)
        k2 = f(t + h/2, y + h/2 * k1)
        k3 = f(t + h/2, y + h/2 * k2)
        k4 = f(t + h, y + h * k3)
        y = y + h/6 * (k1 + 2*k2 + 2*k3 + k4)
        t += h
        ys.append(y)
    return ys

result = rk4(lambda t, y: y, 0, 1.0, 1.0, 10)
print(round(result[-1], 3))
`,

	tests: [
		{
			name: "returns n+1 values",
			code: `{{FUNC}}
print(len(rk4(lambda t, y: 0, 0, 0.0, 1.0, 5)))`,
			expected: "6\n",
		},
		{
			name: "first element is y0",
			code: `{{FUNC}}
print(rk4(lambda t, y: y, 0, 1.0, 1.0, 10)[0])`,
			expected: "1.0\n",
		},
		{
			name: "constant ODE stays fixed",
			code: `{{FUNC}}
print(rk4(lambda t, y: 0, 0, 7.0, 5.0, 100)[-1])`,
			expected: "7.0\n",
		},
		{
			name: "approximates e with 10 steps",
			code: `{{FUNC}}
print(round(rk4(lambda t, y: y, 0, 1.0, 1.0, 10)[-1], 3))`,
			expected: "2.718\n",
		},
	],
};
