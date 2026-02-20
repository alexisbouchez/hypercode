import type { Lesson } from "../../types";

export const vanDerPol: Lesson = {
	id: "van-der-pol",
	title: "Van der Pol Oscillator",
	chapterId: "applications",
	content: `## Van der Pol Oscillator

The **Van der Pol oscillator** is a nonlinear system that models self-sustaining oscillations:

\`\`\`
x'' - μ(1 - x²)x' + x = 0
\`\`\`

- \`μ = 0\`: reduces to simple harmonic motion
- \`μ > 0\`: nonlinear damping

### Nonlinear Damping

The term \`-μ(1 - x²)x'\` is the key:

- When \`|x| < 1\`: the factor \`(1 - x²) > 0\`, so damping is **negative** — the system adds energy. Small oscillations grow.
- When \`|x| > 1\`: the factor \`(1 - x²) < 0\`, so damping is **positive** — the system removes energy. Large oscillations shrink.

This creates a **limit cycle**: regardless of starting conditions (except at rest), the system settles into a specific periodic orbit with amplitude ≈ 2.

### As a System

Let \`v = x'\`:

\`\`\`
dx/dt = v
dv/dt = μ(1 - x²)v - x
\`\`\`

### Historical Context

The Van der Pol equation was originally developed to model vacuum tube circuits in early radio transmitters (1920s). It now models:

- Heartbeat rhythms (cardiac pacemakers)
- Circadian rhythms in biology
- Electrical oscillators
- Seizure dynamics in neuroscience

### Your Task

Implement \`van_der_pol(mu, x0, v0, t_end, n)\` using forward Euler. Return \`(x, v)\`.`,

	starterCode: `def van_der_pol(mu, x0, v0, t_end, n):
    h = t_end / n
    x, v = float(x0), float(v0)
    for _ in range(n):
        dx = v
        dv = mu * (1 - x**2) * v - x
        x = x + h * dx
        v = v + h * dv
    return x, v

# At rest stays at rest
print(van_der_pol(1, 0.0, 0.0, 10, 1000))

# mu=0 is SHM: starting at (0,1), x(π/2) ≈ 1
print(round(van_der_pol(0, 0.0, 1.0, 1.5708, 100000)[0], 1))

# Large amplitude decays toward limit cycle
x, v = van_der_pol(1, 3.0, 0.0, 50, 100000)
print(abs(x) < 3)
`,

	solution: `def van_der_pol(mu, x0, v0, t_end, n):
    h = t_end / n
    x, v = float(x0), float(v0)
    for _ in range(n):
        dx = v
        dv = mu * (1 - x**2) * v - x
        x = x + h * dx
        v = v + h * dv
    return x, v

print(van_der_pol(1, 0.0, 0.0, 10, 1000))
print(round(van_der_pol(0, 0.0, 1.0, 1.5708, 100000)[0], 1))
x, v = van_der_pol(1, 3.0, 0.0, 50, 100000)
print(abs(x) < 3)
`,

	tests: [
		{
			name: "at rest stays at rest",
			code: `{{FUNC}}
print(van_der_pol(1, 0.0, 0.0, 10, 1000))`,
			expected: "(0.0, 0.0)\n",
		},
		{
			name: "mu=0 is SHM: x(π/2) ≈ 1",
			code: `{{FUNC}}
print(round(van_der_pol(0, 0.0, 1.0, 1.5708, 100000)[0], 1))`,
			expected: "1.0\n",
		},
		{
			name: "large amplitude contracts toward limit cycle",
			code: `{{FUNC}}
x, v = van_der_pol(1, 3.0, 0.0, 50, 100000)
print(abs(x) < 3)`,
			expected: "True\n",
		},
		{
			name: "small amplitude grows toward limit cycle",
			code: `{{FUNC}}
x, v = van_der_pol(1, 0.1, 0.0, 50, 100000)
print(abs(x) > 0.1)`,
			expected: "True\n",
		},
	],
};
