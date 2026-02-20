import type { Lesson } from "../../types";

export const electricPotentialLesson: Lesson = {
	id: "electric-potential",
	title: "Electric Potential",
	chapterId: "electric-fields",
	content: `## Electric Potential

**Electric potential** V is the potential energy per unit charge at a point in space. For a point charge:

\`\`\`
V = k × q / r
\`\`\`

- **V** — electric potential (volts, J/C)
- **k** = 8.99 × 10⁹ N·m²/C²
- **q** — source charge (C)
- **r** — distance from charge (m)

### Field vs Potential

| Quantity | Falls off as | Units |
|----------|-------------|-------|
| Electric field E | 1/r² | N/C = V/m |
| Electric potential V | 1/r | V (volts) |

The field is the (negative) gradient of the potential: E = −dV/dr.

### Potential Energy of Two Charges

The energy stored in a pair of charges is:

\`\`\`
U = k × q₁ × q₂ / r
\`\`\`

Positive for like charges (repulsive — you must do work to push them together), negative for opposite charges (attractive — they release energy as they approach).

### Examples (q = 1 μC)

| r (m) | V (volts) |
|-------|----------|
| 1 | **8990.0000** |
| 2 | **4495.0000** |
| 0.1 | **89900.0000** |

### Your Task

Implement \`electric_potential(q, r)\` returning V in volts.`,

	starterCode: `K = 8.99e9

def electric_potential(q, r):
    # V = K * q / r
    return 0

print(f"{electric_potential(1e-6, 1):.4f}")     # 8990.0000
print(f"{electric_potential(1e-6, 2):.4f}")     # 4495.0000
print(f"{electric_potential(2e-6, 1):.4f}")     # 17980.0000
print(f"{electric_potential(1e-6, 0.1):.4f}")   # 89900.0000
`,

	solution: `K = 8.99e9

def electric_potential(q, r):
    return K * q / r

print(f"{electric_potential(1e-6, 1):.4f}")     # 8990.0000
print(f"{electric_potential(1e-6, 2):.4f}")     # 4495.0000
print(f"{electric_potential(2e-6, 1):.4f}")     # 17980.0000
print(f"{electric_potential(1e-6, 0.1):.4f}")   # 89900.0000
`,

	tests: [
		{
			name: "q=1 μC, r=1 m → 8990.0000 V",
			code: `{{FUNC}}
print(f"{electric_potential(1e-6, 1):.4f}")`,
			expected: "8990.0000\n",
		},
		{
			name: "q=1 μC, r=2 m → 4495.0000 V (half distance = double potential)",
			code: `{{FUNC}}
print(f"{electric_potential(1e-6, 2):.4f}")`,
			expected: "4495.0000\n",
		},
		{
			name: "q=2 μC, r=1 m → 17980.0000 V",
			code: `{{FUNC}}
print(f"{electric_potential(2e-6, 1):.4f}")`,
			expected: "17980.0000\n",
		},
		{
			name: "q=1 μC, r=0.1 m → 89900.0000 V",
			code: `{{FUNC}}
print(f"{electric_potential(1e-6, 0.1):.4f}")`,
			expected: "89900.0000\n",
		},
	],
};
