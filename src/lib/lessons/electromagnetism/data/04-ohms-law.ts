import type { Lesson } from "../../types";

export const ohmsLawLesson: Lesson = {
	id: "ohms-law",
	title: "Ohm's Law",
	chapterId: "dc-circuits",
	content: `## Ohm's Law

The most fundamental relationship in circuit analysis: **voltage, current, and resistance are proportional**.

\`\`\`
V = I × R      →      I = V / R      →      R = V / I
\`\`\`

- **V** — voltage (volts, V)
- **I** — current (amperes, A)
- **R** — resistance (ohms, Ω)

### Intuition

Think of current as water flow, voltage as pressure, and resistance as pipe narrowness. Doubling the pressure doubles the flow; doubling the pipe narrowness halves it.

### Ohm's Law Triangle

$$V = IR$$

Cover the quantity you want to find; the remaining two show how to compute it.

### Examples

| V (V) | R (Ω) | I (A) |
|-------|-------|-------|
| 12 | 4 | **3.0000** |
| 5 | 100 | **0.0500** |
| 230 | 46 | **5.0000** |
| 9 | 1000 | **0.0090** |

### Your Task

Implement \`ohms_current(V, R)\` returning current in amperes.`,

	starterCode: `def ohms_current(V, R):
    # I = V / R
    return 0

print(f"{ohms_current(12, 4):.4f}")      # 3.0000
print(f"{ohms_current(5, 100):.4f}")     # 0.0500
print(f"{ohms_current(230, 46):.4f}")    # 5.0000
print(f"{ohms_current(9, 1000):.4f}")    # 0.0090
`,

	solution: `def ohms_current(V, R):
    return V / R

print(f"{ohms_current(12, 4):.4f}")      # 3.0000
print(f"{ohms_current(5, 100):.4f}")     # 0.0500
print(f"{ohms_current(230, 46):.4f}")    # 5.0000
print(f"{ohms_current(9, 1000):.4f}")    # 0.0090
`,

	tests: [
		{
			name: "V=12 V, R=4 Ω → I=3.0000 A",
			code: `{{FUNC}}
print(f"{ohms_current(12, 4):.4f}")`,
			expected: "3.0000\n",
		},
		{
			name: "V=5 V, R=100 Ω → I=0.0500 A",
			code: `{{FUNC}}
print(f"{ohms_current(5, 100):.4f}")`,
			expected: "0.0500\n",
		},
		{
			name: "V=230 V, R=46 Ω → I=5.0000 A",
			code: `{{FUNC}}
print(f"{ohms_current(230, 46):.4f}")`,
			expected: "5.0000\n",
		},
		{
			name: "V=9 V, R=1000 Ω → I=0.0090 A",
			code: `{{FUNC}}
print(f"{ohms_current(9, 1000):.4f}")`,
			expected: "0.0090\n",
		},
	],
};
