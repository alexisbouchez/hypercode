import type { Lesson } from "../../types";

export const electricFieldLesson: Lesson = {
	id: "electric-field",
	title: "Electric Field",
	chapterId: "electric-fields",
	content: `## Electric Field

The **electric field** at a point in space describes the force per unit charge that would act on a positive test charge placed there:

\`\`\`
E = k × q / r²
\`\`\`

- **E** — electric field strength (N/C = V/m)
- **k** = 8.99 × 10⁹ N·m²/C²
- **q** — source charge (C)
- **r** — distance from the charge (m)

### Field vs Force

| Quantity | Formula | Units |
|----------|---------|-------|
| Force | F = k q₁q₂/r² | N |
| Field | E = k q/r² | N/C |
| Relation | F = q × E | — |

The field is a property of the source charge alone. Any other charge q₀ placed in the field experiences F = q₀ × E.

### Examples (q = 1 μC)

| r (m) | E (N/C) |
|-------|---------|
| 1 | **8990.0000** |
| 2 | **2247.5000** |
| 3 | **998.8889** |

### Your Task

Implement \`electric_field(q, r)\` returning E in N/C (k = 8.99 × 10⁹).`,

	starterCode: `K = 8.99e9

def electric_field(q, r):
    # E = K * q / r^2
    return 0

print(f"{electric_field(1e-6, 1):.4f}")    # 8990.0000
print(f"{electric_field(1e-6, 2):.4f}")    # 2247.5000
print(f"{electric_field(2e-6, 1):.4f}")    # 17980.0000
print(f"{electric_field(1e-6, 3):.4f}")    # 998.8889
`,

	solution: `K = 8.99e9

def electric_field(q, r):
    return K * q / (r * r)

print(f"{electric_field(1e-6, 1):.4f}")    # 8990.0000
print(f"{electric_field(1e-6, 2):.4f}")    # 2247.5000
print(f"{electric_field(2e-6, 1):.4f}")    # 17980.0000
print(f"{electric_field(1e-6, 3):.4f}")    # 998.8889
`,

	tests: [
		{
			name: "q=1 μC, r=1 m → 8990.0000 N/C",
			code: `{{FUNC}}
print(f"{electric_field(1e-6, 1):.4f}")`,
			expected: "8990.0000\n",
		},
		{
			name: "q=1 μC, r=2 m → 2247.5000 N/C",
			code: `{{FUNC}}
print(f"{electric_field(1e-6, 2):.4f}")`,
			expected: "2247.5000\n",
		},
		{
			name: "q=2 μC, r=1 m → 17980.0000 N/C",
			code: `{{FUNC}}
print(f"{electric_field(2e-6, 1):.4f}")`,
			expected: "17980.0000\n",
		},
		{
			name: "q=1 μC, r=3 m → 998.8889 N/C",
			code: `{{FUNC}}
print(f"{electric_field(1e-6, 3):.4f}")`,
			expected: "998.8889\n",
		},
	],
};
