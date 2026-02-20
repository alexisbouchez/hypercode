import type { Lesson } from "../../types";

export const electricPowerLesson: Lesson = {
	id: "electric-power",
	title: "Electric Power",
	chapterId: "dc-circuits",
	content: `## Electrical Power

Power is the rate of energy transfer. For an electrical component:

\`\`\`
P = V × I
\`\`\`

Combined with Ohm's law (V = IR), three equivalent forms emerge:

\`\`\`
P = V × I = I² × R = V² / R
\`\`\`

- **P** — power (watts, W)
- **V** — voltage (V)
- **I** — current (A)
- **R** — resistance (Ω)

### The Power Triangle

| Known | Want P |
|-------|--------|
| V, I | V × I |
| I, R | I² × R |
| V, R | V² / R |

### Examples

| V (V) | I (A) | P (W) |
|-------|-------|-------|
| 12 | 2 | **24.0000** |
| 230 | 10 | **2300.0000** |
| 5 | 0.5 | **2.5000** |
| 9 | 0.1 | **0.9000** |

### Your Task

Implement \`electric_power(V, I)\` returning power in watts.`,

	starterCode: `def electric_power(V, I):
    # P = V * I
    return 0

print(f"{electric_power(12, 2):.4f}")      # 24.0000
print(f"{electric_power(230, 10):.4f}")    # 2300.0000
print(f"{electric_power(5, 0.5):.4f}")    # 2.5000
print(f"{electric_power(9, 0.1):.4f}")    # 0.9000
`,

	solution: `def electric_power(V, I):
    return V * I

print(f"{electric_power(12, 2):.4f}")      # 24.0000
print(f"{electric_power(230, 10):.4f}")    # 2300.0000
print(f"{electric_power(5, 0.5):.4f}")    # 2.5000
print(f"{electric_power(9, 0.1):.4f}")    # 0.9000
`,

	tests: [
		{
			name: "V=12 V, I=2 A → P=24.0000 W",
			code: `{{FUNC}}
print(f"{electric_power(12, 2):.4f}")`,
			expected: "24.0000\n",
		},
		{
			name: "V=230 V, I=10 A → P=2300.0000 W",
			code: `{{FUNC}}
print(f"{electric_power(230, 10):.4f}")`,
			expected: "2300.0000\n",
		},
		{
			name: "V=5 V, I=0.5 A → P=2.5000 W",
			code: `{{FUNC}}
print(f"{electric_power(5, 0.5):.4f}")`,
			expected: "2.5000\n",
		},
		{
			name: "V=9 V, I=0.1 A → P=0.9000 W",
			code: `{{FUNC}}
print(f"{electric_power(9, 0.1):.4f}")`,
			expected: "0.9000\n",
		},
	],
};
