import type { Lesson } from "../../types";

export const rk4Step: Lesson = {
	id: "rk4-step",
	title: "Runge-Kutta 4 (RK4)",
	chapterId: "numerical-methods",
	content: `## Runge-Kutta 4th Order

Euler's method uses only the slope at the start of each step. The **Runge-Kutta 4** method (RK4) samples the slope at four points within the step and takes a weighted average — achieving **fourth-order** accuracy.

### The Formula

$$k_1 = f(t,\\; y)$$
$$k_2 = f\\!\\left(t + \\tfrac{h}{2},\\; y + \\tfrac{h}{2} k_1\\right)$$
$$k_3 = f\\!\\left(t + \\tfrac{h}{2},\\; y + \\tfrac{h}{2} k_2\\right)$$
$$k_4 = f(t + h,\\; y + h\\, k_3)$$

$$y_{\\text{next}} = y + \\frac{h}{6}\\left(k_1 + 2k_2 + 2k_3 + k_4\\right)$$

- $k_1$: slope at the start
- $k_2$: slope at the midpoint using $k_1$
- $k_3$: slope at the midpoint using $k_2$ (refined)
- $k_4$: slope at the end using $k_3$

The weights $(1, 2, 2, 1)$ follow Simpson's rule for numerical integration.

### Why RK4?

With the same step size, RK4 is dramatically more accurate than Euler. For $\\frac{dy}{dt} = y$ with $h = 0.1$:

- Euler: error ~0.005 per step
- RK4: error ~0.000000002 per step

RK4 is the workhorse of scientific computing. It is used in physics simulations, orbital mechanics, and engineering systems everywhere Euler is too inaccurate.

### Your Task

Implement \`rk4_step(f, t, y, h)\` that returns the next \`y\` value using one RK4 step.`,

	starterCode: `def rk4_step(f, t, y, h):
    k1 = f(t, y)
    k2 = f(t + h/2, y + h/2 * k1)
    k3 = f(t + h/2, y + h/2 * k2)
    k4 = f(t + h, y + h * k3)
    return y + h/6 * (k1 + 2*k2 + 2*k3 + k4)

# dy/dt = t, exact solution y = t^2/2
# At t=0, y=0, h=1: exact y(1) = 0.5
print(rk4_step(lambda t, y: t, 0, 0.0, 1.0))

# dy/dt = y, y(0)=1, h=0.1: exact y(0.1) = e^0.1 ≈ 1.10517
print(round(rk4_step(lambda t, y: y, 0, 1.0, 0.1), 5))
`,

	solution: `def rk4_step(f, t, y, h):
    k1 = f(t, y)
    k2 = f(t + h/2, y + h/2 * k1)
    k3 = f(t + h/2, y + h/2 * k2)
    k4 = f(t + h, y + h * k3)
    return y + h/6 * (k1 + 2*k2 + 2*k3 + k4)

print(rk4_step(lambda t, y: t, 0, 0.0, 1.0))
print(round(rk4_step(lambda t, y: y, 0, 1.0, 0.1), 5))
`,

	tests: [
		{
			name: "zero derivative leaves y unchanged",
			code: `{{FUNC}}
print(rk4_step(lambda t, y: 0, 0, 5.0, 1.0))`,
			expected: "5.0\n",
		},
		{
			name: "constant derivative adds h exactly",
			code: `{{FUNC}}
print(rk4_step(lambda t, y: 1, 0, 0.0, 1.0))`,
			expected: "1.0\n",
		},
		{
			name: "dy/dt = t is exact (RK4 integrates polynomials exactly)",
			code: `{{FUNC}}
print(rk4_step(lambda t, y: t, 0, 0.0, 1.0))`,
			expected: "0.5\n",
		},
		{
			name: "dy/dt = y matches e^0.1 to 5 decimal places",
			code: `{{FUNC}}
print(round(rk4_step(lambda t, y: y, 0, 1.0, 0.1), 5))`,
			expected: "1.10517\n",
		},
	],
};
