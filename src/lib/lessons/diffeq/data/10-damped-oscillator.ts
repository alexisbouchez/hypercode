import type { Lesson } from "../../types";

export const dampedOscillator: Lesson = {
	id: "damped-oscillator",
	title: "Damped Oscillator",
	chapterId: "systems-and-oscillations",
	content: `## Damped Oscillator

Real oscillators lose energy to friction and air resistance. The damped oscillator adds a velocity-proportional damping term:

$$x'' + 2\\zeta\\omega\\, x' + \\omega^2 x = 0$$

- $\\omega$: natural frequency (rad/s)
- $\\zeta$ (zeta): damping ratio (dimensionless)

### Three Regimes

| Condition | Behavior |
|-----------|----------|
| $\\zeta < 1$ | **Underdamped**: oscillates with decaying amplitude |
| $\\zeta = 1$ | **Critically damped**: fastest return to 0 without oscillating |
| $\\zeta > 1$ | **Overdamped**: slowly returns to 0 without oscillating |

### As a System

Let $v = x'$:

$$\\frac{dx}{dt} = v$$
$$\\frac{dv}{dt} = -2\\zeta\\omega\\, v - \\omega^2 x$$

### Numerical Solution (Symplectic Euler)

\`\`\`python
a = -2*zeta*omega*v - omega**2 * x
v = v + h * a
x = x + h * v
\`\`\`

### Applications

- Suspension systems in cars (want $\\zeta \\approx 0.7$ for comfortable ride)
- Building dampers for earthquake resistance
- RLC circuits in electronics
- Atomic force microscopy cantilevers

### Your Task

Implement \`damped_oscillator(omega, zeta, x0, v0, t_end, n)\`. Return \`(x, v)\`.`,

	starterCode: `def damped_oscillator(omega, zeta, x0, v0, t_end, n):
    h = t_end / n
    x, v = float(x0), float(v0)
    for _ in range(n):
        a = -2 * zeta * omega * v - omega**2 * x
        v = v + h * a
        x = x + h * v
    return x, v

# At rest stays at rest
print(damped_oscillator(1, 0.5, 0.0, 0.0, 10, 1000))

# Overdamped: decays to zero
x, v = damped_oscillator(1, 2, 1.0, 0.0, 20, 100000)
print(x < 0.01)
`,

	solution: `def damped_oscillator(omega, zeta, x0, v0, t_end, n):
    h = t_end / n
    x, v = float(x0), float(v0)
    for _ in range(n):
        a = -2 * zeta * omega * v - omega**2 * x
        v = v + h * a
        x = x + h * v
    return x, v

print(damped_oscillator(1, 0.5, 0.0, 0.0, 10, 1000))
x, v = damped_oscillator(1, 2, 1.0, 0.0, 20, 100000)
print(x < 0.01)
`,

	tests: [
		{
			name: "at rest stays at rest",
			code: `{{FUNC}}
print(damped_oscillator(1, 0.5, 0.0, 0.0, 10, 1000))`,
			expected: "(0.0, 0.0)\n",
		},
		{
			name: "overdamped (zeta=2) decays to zero",
			code: `{{FUNC}}
x, v = damped_oscillator(1, 2, 1.0, 0.0, 20, 100000)
print(x < 0.01)`,
			expected: "True\n",
		},
		{
			name: "critically damped (zeta=1) decays to zero",
			code: `{{FUNC}}
x, v = damped_oscillator(1, 1, 1.0, 0.0, 20, 100000)
print(x < 0.01)`,
			expected: "True\n",
		},
		{
			name: "underdamped (zeta=0.3) amplitude decays",
			code: `{{FUNC}}
x, v = damped_oscillator(1, 0.3, 1.0, 0.0, 20, 100000)
print(abs(x) < 0.1)`,
			expected: "True\n",
		},
	],
};
