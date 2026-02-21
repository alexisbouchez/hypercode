import type { Lesson } from "../../types";

export const rotationGates: Lesson = {
	id: "rotation-gates",
	title: "Rotation Gates Rx, Ry, Rz",
	chapterId: "advanced-gates",
	content: `## Rotation Gates

The **rotation gates** $R_x$, $R_y$, and $R_z$ rotate the qubit state around the X, Y, and Z axes of the Bloch sphere by an angle $\\theta$.

### Rx Gate

$$R_x(\\theta) = \\begin{pmatrix} \\cos\\frac{\\theta}{2} & -i\\sin\\frac{\\theta}{2} \\\\ -i\\sin\\frac{\\theta}{2} & \\cos\\frac{\\theta}{2} \\end{pmatrix}$$

Applied to $[\\alpha, \\beta]$:

$$R_x(\\theta)[\\alpha, \\beta] = \\left[\\cos\\tfrac{\\theta}{2}\\,\\alpha - i\\sin\\tfrac{\\theta}{2}\\,\\beta,\\ -i\\sin\\tfrac{\\theta}{2}\\,\\alpha + \\cos\\tfrac{\\theta}{2}\\,\\beta\\right]$$

### Ry Gate

$$R_y(\\theta) = \\begin{pmatrix} \\cos\\frac{\\theta}{2} & -\\sin\\frac{\\theta}{2} \\\\ \\sin\\frac{\\theta}{2} & \\cos\\frac{\\theta}{2} \\end{pmatrix}$$

Applied to $[\\alpha, \\beta]$:

$$R_y(\\theta)[\\alpha, \\beta] = \\left[\\cos\\tfrac{\\theta}{2}\\,\\alpha - \\sin\\tfrac{\\theta}{2}\\,\\beta,\\ \\sin\\tfrac{\\theta}{2}\\,\\alpha + \\cos\\tfrac{\\theta}{2}\\,\\beta\\right]$$

### Rz Gate

$$R_z(\\theta) = \\begin{pmatrix} e^{-i\\theta/2} & 0 \\\\ 0 & e^{i\\theta/2} \\end{pmatrix}$$

Applied to $[\\alpha, \\beta]$:

$$R_z(\\theta)[\\alpha, \\beta] = \\left[e^{-i\\theta/2}\\,\\alpha,\\ e^{i\\theta/2}\\,\\beta\\right]$$

### Special Cases

At $\\theta = \\pi$, these gates recover familiar Pauli gates (up to global phase):

- $R_x(\\pi) \\propto X$ (bit flip with imaginary phase)
- $R_y(\\pi) \\propto Y$ (bit flip, real entries)
- $R_z(\\pi) \\propto Z$ (phase flip, no amplitude change)

Note that $R_z$ only changes phases, never measurement probabilities.

### Implementation

\`\`\`python
import cmath, math

def rx(theta, state):
    c = math.cos(theta / 2)
    s = math.sin(theta / 2)
    return [
        c * complex(state[0]) + (-1j * s) * complex(state[1]),
        (-1j * s) * complex(state[0]) + c * complex(state[1])
    ]

def ry(theta, state):
    c = math.cos(theta / 2)
    s = math.sin(theta / 2)
    return [
        c * complex(state[0]) + (-s) * complex(state[1]),
        s * complex(state[0]) + c * complex(state[1])
    ]

def rz(theta, state):
    return [
        cmath.exp(-1j * theta / 2) * complex(state[0]),
        cmath.exp(1j * theta / 2) * complex(state[1])
    ]
\`\`\`

### Your Task

Implement \`rx(theta, state)\`, \`ry(theta, state)\`, and \`rz(theta, state)\`.`,

	starterCode: `import cmath, math

def rx(theta, state):
    # Rx(theta) = [[cos(t/2), -i*sin(t/2)], [-i*sin(t/2), cos(t/2)]]
    pass

def ry(theta, state):
    # Ry(theta) = [[cos(t/2), -sin(t/2)], [sin(t/2), cos(t/2)]]
    pass

def rz(theta, state):
    # Rz(theta) = [[e^(-i*t/2), 0], [0, e^(i*t/2)]]
    pass

result = ry(math.pi, [1, 0])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))
`,

	solution: `import cmath, math

def rx(theta, state):
    c = math.cos(theta / 2)
    s = math.sin(theta / 2)
    return [
        c * complex(state[0]) + (-1j * s) * complex(state[1]),
        (-1j * s) * complex(state[0]) + c * complex(state[1])
    ]

def ry(theta, state):
    c = math.cos(theta / 2)
    s = math.sin(theta / 2)
    return [
        c * complex(state[0]) + (-s) * complex(state[1]),
        s * complex(state[0]) + c * complex(state[1])
    ]

def rz(theta, state):
    return [
        cmath.exp(-1j * theta / 2) * complex(state[0]),
        cmath.exp(1j * theta / 2) * complex(state[1])
    ]

result = ry(math.pi, [1, 0])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))
`,

	tests: [
		{
			name: "Ry(π)|0⟩ = |1⟩",
			expected: "0.0\n1.0\n",
		},
		{
			name: "Rx(π)|0⟩ flips to |1⟩ (with phase -i)",
			code: `{{FUNC}}
result = rx(math.pi, [1, 0])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))`,
			expected: "0.0\n1.0\n",
		},
		{
			name: "Rz(π)|0⟩ changes phase only, amplitude unchanged",
			code: `{{FUNC}}
result = rz(math.pi, [1, 0])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))`,
			expected: "1.0\n0.0\n",
		},
		{
			name: "Rx(π/2)|0⟩ creates equal superposition",
			code: `{{FUNC}}
result = rx(math.pi / 2, [1, 0])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))`,
			expected: "0.7071\n0.7071\n",
		},
	],
};
