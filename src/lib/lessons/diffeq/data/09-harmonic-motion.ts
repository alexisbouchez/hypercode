import type { Lesson } from "../../types";

export const harmonicMotion: Lesson = {
	id: "harmonic-motion",
	title: "Simple Harmonic Motion",
	chapterId: "systems-and-oscillations",
	content: `## Simple Harmonic Motion

A mass on a spring oscillates. The restoring force is proportional to displacement:

\`\`\`
F = -k * x   →   m * x'' = -k * x
\`\`\`

Dividing by mass and letting \`ω² = k/m\`:

\`\`\`
x'' = -ω² * x
\`\`\`

### Reducing to a System

A second-order ODE like this is converted to a **first-order system** by introducing velocity \`v = x'\`:

\`\`\`
dx/dt = v
dv/dt = -ω² * x
\`\`\`

State vector: \`[x, v]\`. This is the standard trick — any nth-order ODE becomes a first-order system of n equations.

### Symplectic Euler

For oscillatory problems, the standard Euler method adds energy over time (unstable). The **symplectic** (semi-implicit) variant updates velocity first, then uses the updated velocity for position:

\`\`\`python
v = v + h * (-ω² * x)   # update v first
x = x + h * v            # use new v for x
\`\`\`

This conserves energy long-term, making it suitable for simulating oscillations.

### Exact Solution

\`\`\`
x(t) = x0 * cos(ωt) + (v0/ω) * sin(ωt)
\`\`\`

The motion is sinusoidal with period \`T = 2π/ω\`.

### Your Task

Implement \`harmonic_motion(omega, x0, v0, t_end, n)\` using the symplectic Euler update. Return \`(x, v)\`.`,

	starterCode: `def harmonic_motion(omega, x0, v0, t_end, n):
    if n == 0:
        return float(x0), float(v0)
    h = t_end / n
    x, v = float(x0), float(v0)
    for _ in range(n):
        v = v + h * (-omega**2 * x)   # update velocity
        x = x + h * v                  # update position
    return x, v

# At rest, stays at rest
print(harmonic_motion(1, 0.0, 0.0, 10, 1000))

# Half period (t = π): x(0)=1 → x(π) = -1
x, v = harmonic_motion(1, 1.0, 0.0, 3.14159265, 10000)
print(round(x, 1))
`,

	solution: `def harmonic_motion(omega, x0, v0, t_end, n):
    if n == 0:
        return float(x0), float(v0)
    h = t_end / n
    x, v = float(x0), float(v0)
    for _ in range(n):
        v = v + h * (-omega**2 * x)
        x = x + h * v
    return x, v

print(harmonic_motion(1, 0.0, 0.0, 10, 1000))
x, v = harmonic_motion(1, 1.0, 0.0, 3.14159265, 10000)
print(round(x, 1))
`,

	tests: [
		{
			name: "at rest stays at rest",
			code: `{{FUNC}}
print(harmonic_motion(1, 0.0, 0.0, 10, 1000))`,
			expected: "(0.0, 0.0)\n",
		},
		{
			name: "energy conservation: x^2 + v^2 stays near 1",
			code: `{{FUNC}}
x, v = harmonic_motion(1, 1.0, 0.0, 10, 10000)
print(round(x**2 + v**2, 1))`,
			expected: "1.0\n",
		},
		{
			name: "half period: x(0)=1 → x(π) ≈ -1",
			code: `{{FUNC}}
x, v = harmonic_motion(1, 1.0, 0.0, 3.14159265, 10000)
print(round(x, 1))`,
			expected: "-1.0\n",
		},
		{
			name: "quarter period: x(0)=1 → x(π/2) ≈ 0",
			code: `{{FUNC}}
x, v = harmonic_motion(1, 1.0, 0.0, 1.5707963, 10000)
print(abs(x) < 0.01)`,
			expected: "True\n",
		},
	],
};
