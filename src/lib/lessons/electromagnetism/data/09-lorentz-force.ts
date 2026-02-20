import type { Lesson } from "../../types";

export const lorentzForceLesson: Lesson = {
	id: "lorentz-force",
	title: "Lorentz Force",
	chapterId: "magnetism",
	content: `## The Lorentz Force

A charged particle moving through a magnetic field experiences a force perpendicular to both its velocity and the field:

\`\`\`
F = q × v × B  (when v ⊥ B)
\`\`\`

- **F** — magnetic force (N)
- **q** — charge (C)
- **v** — speed (m/s)
- **B** — magnetic field strength (T)

### Direction

The force direction follows the **right-hand rule** (or left-hand for negative charges): fingers point along v, curl toward B, thumb points along F.

### Full Vector Form

\`\`\`
F = q (v × B)
\`\`\`

The cross product means the force is always perpendicular to velocity — a magnetic field does no work on a charge. Instead it deflects moving charges into circular orbits, which is how cyclotrons and mass spectrometers work.

### Circular Motion in a Field

For circular orbit of radius r:

\`\`\`
r = m × v / (q × B)
\`\`\`

### Examples (v ⊥ B)

| q (C) | v (m/s) | B (T) | F (N) |
|-------|---------|-------|-------|
| 1×10⁻⁶ | 1000 | 1 | **0.0010** |
| 1×10⁻³ | 100 | 0.5 | **0.0500** |
| 1 | 1 | 1 | **1.0000** |
| 2×10⁻⁶ | 2000 | 0.1 | **0.0004** |

### Your Task

Implement \`lorentz_force(q, v, B)\` returning the force magnitude (assuming v ⊥ B).`,

	starterCode: `def lorentz_force(q, v, B):
    # F = q * v * B  (when v perpendicular to B)
    return 0

print(f"{lorentz_force(1e-6, 1000, 1):.4f}")    # 0.0010
print(f"{lorentz_force(1e-3, 100, 0.5):.4f}")   # 0.0500
print(f"{lorentz_force(1, 1, 1):.4f}")          # 1.0000
print(f"{lorentz_force(2e-6, 2000, 0.1):.4f}")  # 0.0004
`,

	solution: `def lorentz_force(q, v, B):
    return q * v * B

print(f"{lorentz_force(1e-6, 1000, 1):.4f}")    # 0.0010
print(f"{lorentz_force(1e-3, 100, 0.5):.4f}")   # 0.0500
print(f"{lorentz_force(1, 1, 1):.4f}")          # 1.0000
print(f"{lorentz_force(2e-6, 2000, 0.1):.4f}")  # 0.0004
`,

	tests: [
		{
			name: "q=1 μC, v=1000 m/s, B=1 T → 0.0010 N",
			code: `{{FUNC}}
print(f"{lorentz_force(1e-6, 1000, 1):.4f}")`,
			expected: "0.0010\n",
		},
		{
			name: "q=1 mC, v=100 m/s, B=0.5 T → 0.0500 N",
			code: `{{FUNC}}
print(f"{lorentz_force(1e-3, 100, 0.5):.4f}")`,
			expected: "0.0500\n",
		},
		{
			name: "q=1 C, v=1 m/s, B=1 T → 1.0000 N",
			code: `{{FUNC}}
print(f"{lorentz_force(1, 1, 1):.4f}")`,
			expected: "1.0000\n",
		},
		{
			name: "q=2 μC, v=2000 m/s, B=0.1 T → 0.0004 N",
			code: `{{FUNC}}
print(f"{lorentz_force(2e-6, 2000, 0.1):.4f}")`,
			expected: "0.0004\n",
		},
	],
};
