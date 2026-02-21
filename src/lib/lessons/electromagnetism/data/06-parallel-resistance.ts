import type { Lesson } from "../../types";

export const parallelResistanceLesson: Lesson = {
	id: "parallel-resistance",
	title: "Parallel Resistance",
	chapterId: "dc-circuits",
	content: `## Resistors in Parallel

When resistors are connected side-by-side (in parallel), they share the same voltage. The total resistance is always **less** than the smallest individual resistor:

$$\frac{1}{R_\text{total}} = \frac{1}{R_1} + \frac{1}{R_2} + \frac{1}{R_3} + \cdots$$

### Why?

Each parallel path gives current another route through the circuit. Adding a parallel resistor is like opening another lane on a highway — total throughput increases, so effective resistance decreases.

### For Two Resistors

A useful shorthand: **product over sum**

$$R_\text{total} = \frac{R_1 R_2}{R_1 + R_2}$$

### Examples

| Resistors (Ω) | R_total (Ω) |
|---------------|-------------|
| 10, 10 | **5.0000** |
| 4, 6 | **2.4000** |
| 100, 100, 100 | **33.3333** |
| 1000, 1000 | **500.0000** |

### Your Task

Implement \`parallel_resistance(resistances)\` taking a list and returning the combined parallel resistance.`,

	starterCode: `def parallel_resistance(resistances):
    # 1/R = sum(1/r for r in resistances)
    return 0

print(f"{parallel_resistance([10, 10]):.4f}")          # 5.0000
print(f"{parallel_resistance([4, 6]):.4f}")            # 2.4000
print(f"{parallel_resistance([100, 100, 100]):.4f}")   # 33.3333
print(f"{parallel_resistance([1000, 1000]):.4f}")      # 500.0000
`,

	solution: `def parallel_resistance(resistances):
    return 1 / sum(1 / r for r in resistances)

print(f"{parallel_resistance([10, 10]):.4f}")          # 5.0000
print(f"{parallel_resistance([4, 6]):.4f}")            # 2.4000
print(f"{parallel_resistance([100, 100, 100]):.4f}")   # 33.3333
print(f"{parallel_resistance([1000, 1000]):.4f}")      # 500.0000
`,

	tests: [
		{
			name: "[10, 10] Ω → 5.0000 Ω",
			code: `{{FUNC}}
print(f"{parallel_resistance([10, 10]):.4f}")`,
			expected: "5.0000\n",
		},
		{
			name: "[4, 6] Ω → 2.4000 Ω",
			code: `{{FUNC}}
print(f"{parallel_resistance([4, 6]):.4f}")`,
			expected: "2.4000\n",
		},
		{
			name: "[100, 100, 100] Ω → 33.3333 Ω",
			code: `{{FUNC}}
print(f"{parallel_resistance([100, 100, 100]):.4f}")`,
			expected: "33.3333\n",
		},
		{
			name: "[1000, 1000] Ω → 500.0000 Ω",
			code: `{{FUNC}}
print(f"{parallel_resistance([1000, 1000]):.4f}")`,
			expected: "500.0000\n",
		},
	],
};
