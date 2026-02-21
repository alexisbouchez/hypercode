import type { Lesson } from "../../types";

export const lotkaVolterra: Lesson = {
	id: "lotka-volterra",
	title: "Lotka-Volterra (Predator-Prey)",
	chapterId: "systems-and-oscillations",
	content: `## Lotka-Volterra Equations

The **predator-prey model** describes two interacting populations: prey $x$ (rabbits) and predators $y$ (foxes):

$$\\frac{dx}{dt} = \\alpha x - \\beta x y \quad \\text{(prey grow, but die when meeting predators)}$$
$$\\frac{dy}{dt} = \\delta x y - \\gamma y \quad \\text{(predators grow by eating prey, die naturally)}$$

- $\\alpha$: prey birth rate
- $\\beta$: predation rate (prey killed per encounter)
- $\\delta$: predator growth rate per prey eaten
- $\\gamma$: predator death rate

### Oscillations

The populations oscillate: when prey are abundant, predators multiply. More predators reduce prey. Fewer prey starve predators. Then prey recover, and the cycle repeats.

### Equilibrium

There is a non-trivial equilibrium where both populations are constant:

$$x^* = \\frac{\\gamma}{\\delta}, \\quad y^* = \\frac{\\alpha}{\\beta}$$

Starting exactly at this point, populations stay fixed forever.

### Conservation Law

The Lotka-Volterra system has a conserved quantity (a "first integral"):

$$V = \\delta x - \\gamma \\ln x + \\beta y - \\alpha \\ln y = \\text{constant}$$

This means the phase-plane trajectories are closed curves.

### Your Task

Implement \`lotka_volterra(alpha, beta, delta, gamma, x0, y0, t_end, n)\` using Euler's method. Return \`(x, y)\`.`,

	starterCode: `def lotka_volterra(alpha, beta, delta, gamma, x0, y0, t_end, n):
    h = t_end / n
    x, y = float(x0), float(y0)
    for _ in range(n):
        dx = alpha * x - beta * x * y
        dy = delta * x * y - gamma * y
        x = x + h * dx
        y = y + h * dy
    return x, y

# At equilibrium (x*=10, y*=10 for alpha=beta=delta=gamma=1)
x, y = lotka_volterra(1, 0.1, 0.1, 1, 10.0, 10.0, 10, 100)
print(round(x, 1), round(y, 1))

# No predators: prey grows exponentially
x, y = lotka_volterra(1, 1, 1, 1, 10.0, 0.0, 1, 100)
print(round(y, 1))
`,

	solution: `def lotka_volterra(alpha, beta, delta, gamma, x0, y0, t_end, n):
    h = t_end / n
    x, y = float(x0), float(y0)
    for _ in range(n):
        dx = alpha * x - beta * x * y
        dy = delta * x * y - gamma * y
        x = x + h * dx
        y = y + h * dy
    return x, y

x, y = lotka_volterra(1, 0.1, 0.1, 1, 10.0, 10.0, 10, 100)
print(round(x, 1), round(y, 1))

x, y = lotka_volterra(1, 1, 1, 1, 10.0, 0.0, 1, 100)
print(round(y, 1))
`,

	tests: [
		{
			name: "at equilibrium, populations stay fixed",
			code: `{{FUNC}}
x, y = lotka_volterra(1, 0.1, 0.1, 1, 10.0, 10.0, 10, 100)
print(round(x, 1), round(y, 1))`,
			expected: "10.0 10.0\n",
		},
		{
			name: "no predators: y stays 0",
			code: `{{FUNC}}
x, y = lotka_volterra(1, 1, 1, 1, 10.0, 0.0, 1, 100)
print(round(y, 1))`,
			expected: "0.0\n",
		},
		{
			name: "no prey: predators die off",
			code: `{{FUNC}}
x, y = lotka_volterra(1, 1, 1, 1, 0.0, 10.0, 1, 100)
print(round(x, 1))`,
			expected: "0.0\n",
		},
		{
			name: "no prey: predator count declines",
			code: `{{FUNC}}
x, y = lotka_volterra(1, 1, 1, 1, 0.0, 10.0, 3, 1000)
print(y < 1)`,
			expected: "True\n",
		},
	],
};
