import type { Lesson } from "../../types";

export const seriesResistanceLesson: Lesson = {
	id: "series-resistance",
	title: "Series Resistance",
	chapterId: "dc-circuits",
	content: `## Resistors in Series

When resistors are connected end-to-end (in series), the same current flows through all of them. The total resistance is simply their sum:

\`\`\`
R_total = R₁ + R₂ + R₃ + …
\`\`\`

### Why?

Each resistor impedes the current. Placing two resistors in series is like adding a second narrow section to a pipe — the total obstruction doubles.

### Voltage Divider

The voltage across each resistor is proportional to its resistance:

\`\`\`
Vₙ = V_total × Rₙ / R_total
\`\`\`

This **voltage divider** principle is used everywhere — from sensor circuits to audio volume controls.

### Examples

| Resistors (Ω) | R_total (Ω) |
|---------------|-------------|
| 10, 20, 30 | **60.0000** |
| 100, 200 | **300.0000** |
| 1, 1, 1, 1 | **4.0000** |
| 47, 33, 10 | **90.0000** |

### Your Task

Implement \`series_resistance(resistances)\` taking a list of resistances and returning their total.`,

	starterCode: `def series_resistance(resistances):
    # R_total = sum of all resistances
    return 0

print(f"{series_resistance([10, 20, 30]):.4f}")    # 60.0000
print(f"{series_resistance([100, 200]):.4f}")      # 300.0000
print(f"{series_resistance([1, 1, 1, 1]):.4f}")    # 4.0000
print(f"{series_resistance([47, 33, 10]):.4f}")    # 90.0000
`,

	solution: `def series_resistance(resistances):
    return sum(resistances)

print(f"{series_resistance([10, 20, 30]):.4f}")    # 60.0000
print(f"{series_resistance([100, 200]):.4f}")      # 300.0000
print(f"{series_resistance([1, 1, 1, 1]):.4f}")    # 4.0000
print(f"{series_resistance([47, 33, 10]):.4f}")    # 90.0000
`,

	tests: [
		{
			name: "[10, 20, 30] Ω → 60.0000 Ω",
			code: `{{FUNC}}
print(f"{series_resistance([10, 20, 30]):.4f}")`,
			expected: "60.0000\n",
		},
		{
			name: "[100, 200] Ω → 300.0000 Ω",
			code: `{{FUNC}}
print(f"{series_resistance([100, 200]):.4f}")`,
			expected: "300.0000\n",
		},
		{
			name: "[1, 1, 1, 1] Ω → 4.0000 Ω",
			code: `{{FUNC}}
print(f"{series_resistance([1, 1, 1, 1]):.4f}")`,
			expected: "4.0000\n",
		},
		{
			name: "[47, 33, 10] Ω → 90.0000 Ω",
			code: `{{FUNC}}
print(f"{series_resistance([47, 33, 10]):.4f}")`,
			expected: "90.0000\n",
		},
	],
};
