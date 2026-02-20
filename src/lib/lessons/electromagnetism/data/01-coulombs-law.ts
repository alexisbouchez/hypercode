import type { Lesson } from "../../types";

export const coulombsLawLesson: Lesson = {
	id: "coulombs-law",
	title: "Coulomb's Law",
	chapterId: "electric-fields",
	content: `## Coulomb's Law

Every charged particle exerts a force on every other. Charles-Augustin de Coulomb measured this in 1785:

\`\`\`
F = k × q₁ × q₂ / r²
\`\`\`

- **k** = 8.99 × 10⁹ N·m²/C² — Coulomb's constant
- **q₁, q₂** — charges in Coulombs (positive or negative)
- **r** — distance between charges (m)
- **F** — force in Newtons (positive = repulsive, negative = attractive)

### The Inverse-Square Law

Coulomb's law has the same r² dependence as Newton's gravity — both are long-range forces that never reach zero. The crucial difference: gravity is always attractive, while electric force can attract or repel.

### Examples (q₁ = q₂ = 1 μC)

| r (m) | F (N) |
|-------|-------|
| 1 | **0.008990** |
| 2 | **0.002248** |
| 3 | **0.000999** |

### Your Task

Implement \`coulomb_force(q1, q2, r)\` returning the force in Newtons (k = 8.99 × 10⁹).`,

	starterCode: `import math

K = 8.99e9

def coulomb_force(q1, q2, r):
    # F = K * q1 * q2 / r^2
    return 0

print(f"{coulomb_force(1e-6, 1e-6, 1):.6f}")    # 0.008990
print(f"{coulomb_force(2e-6, 1e-6, 1):.6f}")    # 0.017980
print(f"{coulomb_force(1e-6, 1e-6, 2):.6f}")    # 0.002248
print(f"{coulomb_force(1e-6, 1e-6, 3):.6f}")    # 0.000999
`,

	solution: `import math

K = 8.99e9

def coulomb_force(q1, q2, r):
    return K * q1 * q2 / (r * r)

print(f"{coulomb_force(1e-6, 1e-6, 1):.6f}")    # 0.008990
print(f"{coulomb_force(2e-6, 1e-6, 1):.6f}")    # 0.017980
print(f"{coulomb_force(1e-6, 1e-6, 2):.6f}")    # 0.002248
print(f"{coulomb_force(1e-6, 1e-6, 3):.6f}")    # 0.000999
`,

	tests: [
		{
			name: "q₁=q₂=1 μC, r=1 m → 0.008990 N",
			code: `{{FUNC}}
print(f"{coulomb_force(1e-6, 1e-6, 1):.6f}")`,
			expected: "0.008990\n",
		},
		{
			name: "q₁=2 μC, q₂=1 μC, r=1 m → 0.017980 N",
			code: `{{FUNC}}
print(f"{coulomb_force(2e-6, 1e-6, 1):.6f}")`,
			expected: "0.017980\n",
		},
		{
			name: "q₁=q₂=1 μC, r=2 m → 0.002248 N (¼ force)",
			code: `{{FUNC}}
print(f"{coulomb_force(1e-6, 1e-6, 2):.6f}")`,
			expected: "0.002248\n",
		},
		{
			name: "q₁=q₂=1 μC, r=3 m → 0.000999 N",
			code: `{{FUNC}}
print(f"{coulomb_force(1e-6, 1e-6, 3):.6f}")`,
			expected: "0.000999\n",
		},
	],
};
