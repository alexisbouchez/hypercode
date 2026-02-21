import type { Lesson } from "../../types";

export const wireMagneticFieldLesson: Lesson = {
	id: "wire-magnetic-field",
	title: "Magnetic Field of a Wire",
	chapterId: "magnetism",
	content: `## Long Straight Wire

A current-carrying wire creates a magnetic field that wraps around it in concentric circles. The field strength at distance r from an infinite straight wire:

$$B = \frac{\mu_0 I}{2\pi r}$$

- **B** — magnetic field (tesla, T)
- $\mu_0 = 4\pi \times 10^{-7}$ T·m/A — permeability of free space
- **I** — current (amperes)
- **r** — perpendicular distance from wire (m)

### The Direction

Use the **right-hand rule**: point your thumb in the direction of current flow; your fingers curl in the direction of the magnetic field.

### Simplification

Since $\mu_0 / (2\pi) = 2 \times 10^{-7}$ T·m/A exactly, the formula simplifies to:

$$B = \frac{2 \times 10^{-7} \cdot I}{r}$$

### Examples

| I (A) | r (m) | B (T) |
|-------|-------|-------|
| 1 | 1 | **2.00e-07** |
| 10 | 1 | **2.00e-06** |
| 1 | 0.1 | **2.00e-06** |
| 100 | 0.5 | **4.00e-05** |

### Your Task

Implement \`wire_magnetic_field(I, r)\` returning B in tesla ($\mu_0 = 4\pi \times 10^{-7}$).`,

	starterCode: `import math

MU0 = 4 * math.pi * 1e-7

def wire_magnetic_field(I, r):
    # B = MU0 * I / (2 * pi * r)
    return 0

print(f"{wire_magnetic_field(1, 1):.2e}")       # 2.00e-07
print(f"{wire_magnetic_field(10, 1):.2e}")      # 2.00e-06
print(f"{wire_magnetic_field(1, 0.1):.2e}")     # 2.00e-06
print(f"{wire_magnetic_field(100, 0.5):.2e}")   # 4.00e-05
`,

	solution: `import math

MU0 = 4 * math.pi * 1e-7

def wire_magnetic_field(I, r):
    return MU0 * I / (2 * math.pi * r)

print(f"{wire_magnetic_field(1, 1):.2e}")       # 2.00e-07
print(f"{wire_magnetic_field(10, 1):.2e}")      # 2.00e-06
print(f"{wire_magnetic_field(1, 0.1):.2e}")     # 2.00e-06
print(f"{wire_magnetic_field(100, 0.5):.2e}")   # 4.00e-05
`,

	tests: [
		{
			name: "I=1 A, r=1 m → 2.00e-07 T",
			code: `{{FUNC}}
print(f"{wire_magnetic_field(1, 1):.2e}")`,
			expected: "2.00e-07\n",
		},
		{
			name: "I=10 A, r=1 m → 2.00e-06 T",
			code: `{{FUNC}}
print(f"{wire_magnetic_field(10, 1):.2e}")`,
			expected: "2.00e-06\n",
		},
		{
			name: "I=1 A, r=0.1 m → 2.00e-06 T",
			code: `{{FUNC}}
print(f"{wire_magnetic_field(1, 0.1):.2e}")`,
			expected: "2.00e-06\n",
		},
		{
			name: "I=100 A, r=0.5 m → 4.00e-05 T",
			code: `{{FUNC}}
print(f"{wire_magnetic_field(100, 0.5):.2e}")`,
			expected: "4.00e-05\n",
		},
	],
};
