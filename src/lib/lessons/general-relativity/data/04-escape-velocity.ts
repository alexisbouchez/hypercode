import type { Lesson } from "../../types";

export const escapeVelocity: Lesson = {
	id: "escape-velocity",
	title: "Relativistic Escape Velocity",
	chapterId: "schwarzschild",
	content: `## Relativistic Escape Velocity

In Newtonian gravity, the escape velocity from radius $r$ is $v_{\\text{esc}} = \\sqrt{2GM/r}$. General Relativity modifies this slightly, but the deeper insight is that the Schwarzschild radius marks the point where even light — moving at $c$ — cannot escape.

### The GR Escape Velocity

From the Schwarzschild metric, the escape velocity from radius $r$ is:

$$v_{\\text{esc}} = c \\sqrt{\\frac{r_s}{r}} = c \\sqrt{\\frac{2GM}{c^2 r}}$$

As a fraction of $c$:

$$\\frac{v_{\\text{esc}}}{c} = \\sqrt{\\frac{2GM}{c^2 r}}$$

At $r = r_s$, this equals 1 — the escape velocity equals the speed of light, which is why nothing escapes from inside the event horizon.

### Event Horizon Check

A radius $r$ is inside (or at) the event horizon if $r \\leq r_s = 2GM/c^2$.

### Physical Values

| Object | Radius | $v_{\\text{esc}}$ |
|--------|--------|------------------|
| Earth | $6{,}371$ km | $\\approx 11.2$ km/s |
| Sun | $696{,}000$ km | $\\approx 618$ km/s |
| Neutron star (1.4 $M_\\odot$, 10 km) | $10$ km | $\\approx 64\\%$ of $c$ |
| Black hole | $r_s$ | $= c$ |

### Your Task

Implement these functions with all constants defined **inside** each function:

- \`escape_velocity(M, r)\` — returns $v_{\\text{esc}} = c\\sqrt{2GM/(c^2 r)}$ in m/s
- \`escape_velocity_fraction(M, r)\` — returns $v_{\\text{esc}}/c = \\sqrt{2GM/(c^2 r)}$
- \`event_horizon_check(M, r)\` — returns \`True\` if $r \\leq 2GM/c^2$, else \`False\``,

	starterCode: `import math

def escape_velocity(M, r):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return c * math.sqrt(2*G*M / (c**2 * r))
    pass

def escape_velocity_fraction(M, r):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return math.sqrt(2*G*M / (c**2 * r))
    pass

def event_horizon_check(M, r):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return r <= 2*G*M / c**2
    pass

M_earth = 5.972e24
print(round(escape_velocity(M_earth, 6.371e6), 2))
print(round(escape_velocity_fraction(M_earth, 6.371e6), 8))
print(round(escape_velocity(1.989e30, 6.96e8), 2))
print(round(escape_velocity_fraction(1.4 * 1.989e30, 10e3), 6))
`,

	solution: `import math

def escape_velocity(M, r):
    G = 6.674e-11
    c = 299792458.0
    return c * math.sqrt(2 * G * M / (c**2 * r))

def escape_velocity_fraction(M, r):
    G = 6.674e-11
    c = 299792458.0
    return math.sqrt(2 * G * M / (c**2 * r))

def event_horizon_check(M, r):
    G = 6.674e-11
    c = 299792458.0
    return r <= 2 * G * M / c**2

M_earth = 5.972e24
print(round(escape_velocity(M_earth, 6.371e6), 2))
print(round(escape_velocity_fraction(M_earth, 6.371e6), 8))
print(round(escape_velocity(1.989e30, 6.96e8), 2))
print(round(escape_velocity_fraction(1.4 * 1.989e30, 10e3), 6))
`,

	tests: [
		{
			name: "escape_velocity from Earth surface ≈ 11185.73 m/s",
			code: `{{FUNC}}
print(round(escape_velocity(5.972e24, 6.371e6), 2))`,
			expected: "11185.73\n",
		},
		{
			name: "escape_velocity_fraction from Earth surface ≈ 3.731e-05",
			code: `{{FUNC}}
print(round(escape_velocity_fraction(5.972e24, 6.371e6), 8))`,
			expected: "3.731e-05\n",
		},
		{
			name: "escape_velocity from solar surface ≈ 617619.32 m/s",
			code: `{{FUNC}}
print(round(escape_velocity(1.989e30, 6.96e8), 2))`,
			expected: "617619.32\n",
		},
		{
			name: "escape_velocity_fraction from neutron star (1.4 solar masses, r=10 km) ≈ 0.643086",
			code: `{{FUNC}}
print(round(escape_velocity_fraction(1.4 * 1.989e30, 10e3), 6))`,
			expected: "0.643086\n",
		},
	],
};
