import type { Lesson } from "../../types";

export const blochSphere: Lesson = {
	id: "bloch-sphere",
	title: "Bloch Sphere Coordinates",
	chapterId: "advanced-gates",
	content: `## The Bloch Sphere

Every pure single-qubit state can be represented as a point on the surface of the **Bloch sphere** — a unit sphere in 3D space.

### Parametrization

Any normalized qubit state can be written as:

$$|\\psi\\rangle = \\cos\\frac{\\theta}{2}|0\\rangle + e^{i\\phi}\\sin\\frac{\\theta}{2}|1\\rangle$$

where:
- $\\theta \\in [0, \\pi]$ is the **polar angle** (latitude from the north pole)
- $\\phi \\in [0, 2\\pi)$ is the **azimuthal angle** (longitude)

### Key Points on the Bloch Sphere

| State | $\\theta$ | $\\phi$ | Location |
|-------|-----------|---------|----------|
| $|0\\rangle$ | $0$ | any | North pole |
| $|1\\rangle$ | $\\pi$ | any | South pole |
| $|+\\rangle = \\frac{|0\\rangle+|1\\rangle}{\\sqrt{2}}$ | $\\frac{\\pi}{2}$ | $0$ | $+X$ axis |
| $|-\\rangle = \\frac{|0\\rangle-|1\\rangle}{\\sqrt{2}}$ | $\\frac{\\pi}{2}$ | $\\pi$ | $-X$ axis |
| $|L\\rangle = \\frac{|0\\rangle+i|1\\rangle}{\\sqrt{2}}$ | $\\frac{\\pi}{2}$ | $\\frac{\\pi}{2}$ | $+Y$ axis |
| $|R\\rangle = \\frac{|0\\rangle-i|1\\rangle}{\\sqrt{2}}$ | $\\frac{\\pi}{2}$ | $\\frac{3\\pi}{2}$ | $-Y$ axis |

### Extracting Angles from a State

Given a normalized state $[\\alpha, \\beta]$:

$$\\theta = 2\\arccos(|\\alpha|)$$

$$\\phi = \\arg(\\beta) - \\arg(\\alpha) \\pmod{2\\pi}$$

When $|\\alpha| \\approx 0$ (i.e., the state is near $|1\\rangle$), $\\alpha$ has no well-defined argument, so we set $\\phi = 0$ by convention.

### Implementation

\`\`\`python
import cmath, math

def bloch_angles(state):
    alpha = complex(state[0])
    beta = complex(state[1])
    theta = 2 * math.acos(min(abs(alpha), 1.0))
    if abs(alpha) < 1e-10:
        phi = 0.0
    else:
        phi = (cmath.phase(beta) - cmath.phase(alpha)) % (2 * math.pi)
    return (theta, phi)
\`\`\`

### Why the Bloch Sphere Matters

- **Gate actions** become geometric rotations: X is a $\\pi$-rotation around the X axis, H is a $\\pi$-rotation around the $(X+Z)/\\sqrt{2}$ axis.
- **Decoherence** contracts the Bloch sphere inward — mixed states live inside the sphere.
- **Visualization** makes it easy to reason about the effect of gate sequences.

### Your Task

Implement \`bloch_angles(state)\` that returns \`(theta, phi)\` for a normalized qubit state.`,

	starterCode: `import cmath, math

def bloch_angles(state):
    # theta = 2 * acos(|alpha|)
    # phi = (arg(beta) - arg(alpha)) mod 2*pi  (0.0 if |alpha| < 1e-10)
    pass

theta, phi = bloch_angles([1, 0])
print(round(theta, 4))
print(round(phi, 4))
`,

	solution: `import cmath, math

def bloch_angles(state):
    alpha = complex(state[0])
    beta = complex(state[1])
    theta = 2 * math.acos(min(abs(alpha), 1.0))
    if abs(alpha) < 1e-10:
        phi = 0.0
    else:
        phi = (cmath.phase(beta) - cmath.phase(alpha)) % (2 * math.pi)
    return (theta, phi)

theta, phi = bloch_angles([1, 0])
print(round(theta, 4))
print(round(phi, 4))
`,

	tests: [
		{
			name: "|0⟩ is at the north pole (θ=0, φ=0)",
			expected: "0.0\n0.0\n",
		},
		{
			name: "|1⟩ is at the south pole (θ=π)",
			code: `{{FUNC}}
theta, phi = bloch_angles([0, 1])
print(round(theta, 4))
print(round(phi, 4))`,
			expected: "3.1416\n0.0\n",
		},
		{
			name: "|+⟩ lies on the equator (θ=π/2, φ=0)",
			code: `{{FUNC}}
import math
sq2 = 1/math.sqrt(2)
theta, phi = bloch_angles([sq2, sq2])
print(round(theta, 4))
print(round(phi, 4))`,
			expected: "1.5708\n0.0\n",
		},
		{
			name: "|L⟩ = (|0⟩+i|1⟩)/√2 has φ=π/2",
			code: `{{FUNC}}
import math
sq2 = 1/math.sqrt(2)
theta, phi = bloch_angles([sq2, 1j*sq2])
print(round(theta, 4))
print(round(phi, 4))`,
			expected: "1.5708\n1.5708\n",
		},
	],
};
