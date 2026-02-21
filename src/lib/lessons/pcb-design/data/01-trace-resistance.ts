import type { Lesson } from "../../types";

export const traceResistance: Lesson = {
	id: "trace-resistance",
	title: "Trace Resistance",
	chapterId: "trace-design",
	content: `## Trace Resistance

Every copper trace on a PCB has electrical resistance. Understanding this is crucial for power delivery: too much resistance drops voltage across the trace, causing components to receive less power than expected.

The resistance formula is:

\`\`\`
R = ρ × L / (W × T)
\`\`\`

Where:
- **ρ** = resistivity of copper = **1.72 × 10⁻⁸ Ω·m**
- **L** = trace length (meters)
- **W** = trace width (meters)
- **T** = trace thickness (meters)

Common copper weights:
| Oz | Thickness (μm) |
|----|----------------|
| 0.5 oz | 17.5 μm |
| 1 oz | 35 μm |
| 2 oz | 70 μm |

**Example:** A 100 mm long, 0.5 mm wide, 1 oz (35 μm) trace:
\`R = 1.72×10⁻⁸ × 0.1 / (0.0005 × 0.000035) = 98.3 mΩ\`

### Your Task

Implement \`traceResistance(lengthMm, widthMm, thicknessMm)\` that returns resistance in **milliohms** (mΩ).`,

	starterCode: `function traceResistance(lengthMm, widthMm, thicknessMm) {
  const rho = 1.72e-8; // copper resistivity in Ω·m
  // Convert mm to m, compute R in ohms, return in milliohms
}

// 100mm long, 0.5mm wide, 35μm thick (1 oz copper)
console.log(traceResistance(100, 0.5, 0.035).toFixed(2));
// 50mm long, 1mm wide, 35μm thick
console.log(traceResistance(50, 1, 0.035).toFixed(2));
`,

	solution: `function traceResistance(lengthMm, widthMm, thicknessMm) {
  const rho = 1.72e-8;
  const L = lengthMm / 1000;
  const W = widthMm / 1000;
  const T = thicknessMm / 1000;
  return (rho * L / (W * T)) * 1000;
}

console.log(traceResistance(100, 0.5, 0.035).toFixed(2));
console.log(traceResistance(50, 1, 0.035).toFixed(2));
`,

	tests: [
		{
			name: "100mm×0.5mm×35μm and 50mm×1mm×35μm",
			expected: "98.29\n24.57\n",
		},
		{
			name: "1mm wide 1oz 100mm trace",
			code: `{{FUNC}}
console.log(traceResistance(100, 1, 0.035).toFixed(2));`,
			expected: "49.14\n",
		},
		{
			name: "2oz copper reduces resistance by half",
			code: `{{FUNC}}
const r1oz = traceResistance(100, 0.5, 0.035);
const r2oz = traceResistance(100, 0.5, 0.070);
console.log((r1oz / r2oz).toFixed(1));`,
			expected: "2.0\n",
		},
	],
};
