import type { Lesson } from "../../types";

export const eulerSystem: Lesson = {
	id: "euler-system",
	title: "Systems of ODEs",
	chapterId: "systems-and-oscillations",
	content: `## Systems of ODEs

A single ODE describes one unknown. Many real problems involve multiple quantities that interact — requiring a **system** of ODEs:

\`\`\`
dy1/dt = f1(t, y1, y2, ...)
dy2/dt = f2(t, y1, y2, ...)
...
\`\`\`

We represent the state as a vector \`y = [y1, y2, ...]\` and the derivative function returns a vector too:

\`\`\`
dy/dt = f(t, y)   where y ∈ ℝⁿ
\`\`\`

### Vector Euler's Method

Exactly the same formula, applied component-wise:

\`\`\`python
def euler_system(f, t0, y0, t_end, n):
    h = (t_end - t0) / n
    t = t0
    y = list(y0)
    ys = [list(y)]
    for _ in range(n):
        dydt = f(t, y)
        y = [yi + h * di for yi, di in zip(y, dydt)]
        t += h
        ys.append(list(y))
    return ys
\`\`\`

### Example: Predator Growth

\`\`\`python
# Rabbit grows, fox grows proportionally
# dy1/dt = 2*y1,  dy2/dt = y1
def f(t, y):
    return [2*y[0], y[0]]
\`\`\`

All future lessons use this vector pattern: simple harmonic motion, predator-prey, epidemic models.

### Your Task

Implement \`euler_system(f, t0, y0, t_end, n)\` where \`f(t, y)\` returns a list of derivatives and \`y0\` is a list of initial values. Return a list of \`n+1\` state vectors.`,

	starterCode: `def euler_system(f, t0, y0, t_end, n):
    h = (t_end - t0) / n
    t = float(t0)
    y = list(y0)
    ys = [list(y)]
    for _ in range(n):
        dydt = f(t, y)
        y = [yi + h * di for yi, di in zip(y, dydt)]
        t += h
        ys.append(list(y))
    return ys

# Zero derivatives: state stays constant
f = lambda t, y: [0.0] * len(y)
result = euler_system(f, 0, [3.0, 4.0], 1.0, 10)
print(result[-1])  # [3.0, 4.0]
`,

	solution: `def euler_system(f, t0, y0, t_end, n):
    h = (t_end - t0) / n
    t = float(t0)
    y = list(y0)
    ys = [list(y)]
    for _ in range(n):
        dydt = f(t, y)
        y = [yi + h * di for yi, di in zip(y, dydt)]
        t += h
        ys.append(list(y))
    return ys

f = lambda t, y: [0.0] * len(y)
result = euler_system(f, 0, [3.0, 4.0], 1.0, 10)
print(result[-1])
`,

	tests: [
		{
			name: "returns n+1 state vectors",
			code: `{{FUNC}}
f = lambda t, y: [0.0] * len(y)
print(len(euler_system(f, 0, [1.0, 2.0], 1.0, 5)))`,
			expected: "6\n",
		},
		{
			name: "zero derivatives keep state fixed",
			code: `{{FUNC}}
f = lambda t, y: [0.0] * len(y)
print(euler_system(f, 0, [3.0, 4.0], 1.0, 10)[-1])`,
			expected: "[3.0, 4.0]\n",
		},
		{
			name: "first element is y0",
			code: `{{FUNC}}
f = lambda t, y: [1.0, -1.0]
print(euler_system(f, 0, [0.0, 0.0], 1.0, 10)[0])`,
			expected: "[0.0, 0.0]\n",
		},
		{
			name: "one step with constant derivative",
			code: `{{FUNC}}
f = lambda t, y: [1.0, 2.0]
print(euler_system(f, 0, [0.0, 0.0], 1.0, 1)[-1])`,
			expected: "[1.0, 2.0]\n",
		},
	],
};
