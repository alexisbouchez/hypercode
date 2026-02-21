import type { Lesson } from "../../types";

export const eulerStep: Lesson = {
	id: "euler-step",
	title: "Euler's Method",
	chapterId: "numerical-methods",
	content: `## Euler's Method

Differential equations describe how quantities change over time. A first-order ODE has the form:

$$\\frac{dy}{dt} = f(t, y), \\quad y(t_0) = y_0$$

We cannot always solve this analytically. **Euler's method** is the simplest numerical approach: approximate the solution by taking small steps along the tangent line.

### The Formula

Given the current state $(t, y)$, take a small step of size $h$:

$$y_{\\text{next}} = y + h \\cdot f(t, y)$$

The idea: $f(t, y)$ is the slope (derivative) at the current point. Moving $h$ forward in time, we step $h \\times \\text{slope}$ in the y-direction.

### Example

For $\\frac{dy}{dt} = y$ (exponential growth) with $y(0) = 1$ and $h = 0.1$:

$$y_{\\text{next}} = 1 + 0.1 \\times 1 = 1.1$$

The exact answer at $t = 0.1$ is $e^{0.1} \\approx 1.10517$. Euler's method gives $1.1$ — a reasonable approximation for a single step.

### Your Task

Implement \`euler_step(f, t, y, h)\` that returns the next \`y\` value using one Euler step.`,

	starterCode: `def euler_step(f, t, y, h):
    return y + h * f(t, y)

# Test: dy/dt = y (exponential growth), y(0) = 1, h = 0.1
f = lambda t, y: y
print(euler_step(f, 0, 1.0, 0.1))  # 1.1

# Test: dy/dt = -y (exponential decay), y(0) = 2, h = 0.1
g = lambda t, y: -y
print(euler_step(g, 0, 2.0, 0.1))  # 1.8
`,

	solution: `def euler_step(f, t, y, h):
    return y + h * f(t, y)

f = lambda t, y: y
print(euler_step(f, 0, 1.0, 0.1))

g = lambda t, y: -y
print(euler_step(g, 0, 2.0, 0.1))
`,

	tests: [
		{
			name: "zero derivative leaves y unchanged",
			code: `{{FUNC}}
print(euler_step(lambda t, y: 0, 0, 5.0, 0.1))`,
			expected: "5.0\n",
		},
		{
			name: "constant derivative adds h * f",
			code: `{{FUNC}}
print(euler_step(lambda t, y: 1, 0, 0.0, 0.5))`,
			expected: "0.5\n",
		},
		{
			name: "dy/dt = y step",
			code: `{{FUNC}}
print(euler_step(lambda t, y: y, 0, 1.0, 0.1))`,
			expected: "1.1\n",
		},
		{
			name: "dy/dt = -y decay step",
			code: `{{FUNC}}
print(euler_step(lambda t, y: -y, 0, 2.0, 0.1))`,
			expected: "1.8\n",
		},
	],
};
