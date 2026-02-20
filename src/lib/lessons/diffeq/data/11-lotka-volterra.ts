import type { Lesson } from "../../types";

export const lotkaVolterra: Lesson = {
	id: "lotka-volterra",
	title: "Lotka-Volterra (Predator-Prey)",
	chapterId: "systems-and-oscillations",
	content: `## Lotka-Volterra Equations

The **predator-prey model** describes two interacting populations: prey \`x\` (rabbits) and predators \`y\` (foxes):

\`\`\`
dx/dt = α·x - β·x·y     (prey grow, but die when meeting predators)
dy/dt = δ·x·y - γ·y     (predators grow by eating prey, die naturally)
\`\`\`

- \`α\`: prey birth rate
- \`β\`: predation rate (prey killed per encounter)
- \`δ\`: predator growth rate per prey eaten
- \`γ\`: predator death rate

### Oscillations

The populations oscillate: when prey are abundant, predators multiply. More predators reduce prey. Fewer prey starve predators. Then prey recover, and the cycle repeats.

### Equilibrium

There is a non-trivial equilibrium where both populations are constant:

\`\`\`
x* = γ/δ,   y* = α/β
\`\`\`

Starting exactly at this point, populations stay fixed forever.

### Conservation Law

The Lotka-Volterra system has a conserved quantity (a "first integral"):

\`\`\`
V = δ·x - γ·ln(x) + β·y - α·ln(y) = constant
\`\`\`

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
