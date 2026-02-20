import type { Lesson } from "../../types";

export const magneticFluxLesson: Lesson = {
	id: "magnetic-flux",
	title: "Magnetic Flux",
	chapterId: "magnetism",
	content: `## Magnetic Flux

**Magnetic flux** Φ quantifies how much of a magnetic field passes through a surface:

\`\`\`
Φ = B × A × cos(θ)
\`\`\`

- **Φ** — magnetic flux (weber, Wb = V·s)
- **B** — magnetic field strength (T)
- **A** — area of the surface (m²)
- **θ** — angle between B and the surface normal

### The Cosine Factor

| θ | cos(θ) | Flux |
|---|--------|------|
| 0° | 1 | maximum — field perpendicular to surface |
| 45° | √2/2 | intermediate |
| 90° | 0 | zero — field parallel to surface |

### Why It Matters

Flux is the key quantity in **Faraday's law of induction**: a changing flux induces an EMF. It is also conserved — magnetic field lines that enter a closed surface must exit it (Gauss's law for magnetism: div B = 0).

### Examples

| B (T) | A (m²) | θ | Φ (Wb) |
|-------|--------|---|--------|
| 1 | 1 | 0° | **1.0000** |
| 1 | 1 | 90° | **0.0000** |
| 2 | 0.5 | 60° | **0.5000** |
| 0.1 | 2 | 45° | **0.1414** |

### Your Task

Implement \`magnetic_flux(B, A, theta_deg)\` returning Φ in webers.`,

	starterCode: `import math

def magnetic_flux(B, A, theta_deg):
    # Phi = B * A * cos(theta_rad)
    return 0

print(f"{magnetic_flux(1, 1, 0):.4f}")      # 1.0000
print(f"{magnetic_flux(1, 1, 90):.4f}")     # 0.0000
print(f"{magnetic_flux(2, 0.5, 60):.4f}")   # 0.5000
print(f"{magnetic_flux(0.1, 2, 45):.4f}")   # 0.1414
`,

	solution: `import math

def magnetic_flux(B, A, theta_deg):
    theta = theta_deg * math.pi / 180
    return B * A * math.cos(theta)

print(f"{magnetic_flux(1, 1, 0):.4f}")      # 1.0000
print(f"{magnetic_flux(1, 1, 90):.4f}")     # 0.0000
print(f"{magnetic_flux(2, 0.5, 60):.4f}")   # 0.5000
print(f"{magnetic_flux(0.1, 2, 45):.4f}")   # 0.1414
`,

	tests: [
		{
			name: "B=1 T, A=1 m², θ=0° → Φ=1.0000 Wb",
			code: `{{FUNC}}
print(f"{magnetic_flux(1, 1, 0):.4f}")`,
			expected: "1.0000\n",
		},
		{
			name: "B=1 T, A=1 m², θ=90° → Φ=0.0000 Wb",
			code: `{{FUNC}}
print(f"{magnetic_flux(1, 1, 90):.4f}")`,
			expected: "0.0000\n",
		},
		{
			name: "B=2 T, A=0.5 m², θ=60° → Φ=0.5000 Wb",
			code: `{{FUNC}}
print(f"{magnetic_flux(2, 0.5, 60):.4f}")`,
			expected: "0.5000\n",
		},
		{
			name: "B=0.1 T, A=2 m², θ=45° → Φ=0.1414 Wb",
			code: `{{FUNC}}
print(f"{magnetic_flux(0.1, 2, 45):.4f}")`,
			expected: "0.1414\n",
		},
	],
};
