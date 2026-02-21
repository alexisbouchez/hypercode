import type { Lesson } from "../../types";

export const traceCurrentCapacity: Lesson = {
	id: "trace-current-capacity",
	title: "Trace Current Capacity",
	chapterId: "trace-design",
	content: `## Trace Current Capacity (IPC-2221)

How much current can a trace carry before overheating? The IPC-2221 standard provides empirical formulas based on temperature rise.

\`\`\`
I = K × ΔT^0.44 × A^0.725
\`\`\`

Where:
- **I** = maximum current (Amps)
- **K** = 0.048 for external traces, 0.024 for internal traces
- **ΔT** = allowed temperature rise above ambient (°C)
- **A** = cross-sectional area of the trace (mils²) — 1 mil = 0.0254 mm

The cross-section area in mils²:
\`\`\`
A = (width in mils) × (thickness in mils)
\`\`\`

Common trace thicknesses in mils: 1 oz = 1.378 mils, 2 oz = 2.756 mils.

**Example:** External 10 mil wide, 1 oz trace, 10°C rise:
- A = 10 × 1.378 = 13.78 mils²
- I = 0.048 × 10^0.44 × 13.78^0.725 ≈ 0.048 × 2.754 × 6.68 ≈ 0.884 A

### Your Task

Implement \`traceCurrentCapacity(widthMm, thicknessMm, deltaT, external)\`:
- \`widthMm\`, \`thicknessMm\`: dimensions in mm
- \`deltaT\`: temperature rise in °C
- \`external\`: boolean (true = external trace)
- Returns max current in Amps`,

	starterCode: `function traceCurrentCapacity(widthMm, thicknessMm, deltaT, external) {
  const mmToMils = 1 / 0.0254;
  const K = external ? 0.048 : 0.024;
  // Compute area in mils², apply IPC-2221 formula
}

// 0.254mm (10mil) wide, 1oz (0.0350mm) thick, 10°C rise, external
console.log(traceCurrentCapacity(0.254, 0.0350, 10, true).toFixed(3));
// Internal trace same dimensions
console.log(traceCurrentCapacity(0.254, 0.0350, 10, false).toFixed(3));
`,

	solution: `function traceCurrentCapacity(widthMm, thicknessMm, deltaT, external) {
  const mmToMils = 1 / 0.0254;
  const K = external ? 0.048 : 0.024;
  const widthMils = widthMm * mmToMils;
  const thickMils = thicknessMm * mmToMils;
  const A = widthMils * thickMils;
  return K * Math.pow(deltaT, 0.44) * Math.pow(A, 0.725);
}

console.log(traceCurrentCapacity(0.254, 0.0350, 10, true).toFixed(3));
console.log(traceCurrentCapacity(0.254, 0.0350, 10, false).toFixed(3));
`,

	tests: [
		{
			name: "external and internal 10mil 1oz 10C rise",
			expected: "0.885\n0.443\n",
		},
		{
			name: "doubling width increases current",
			code: `{{FUNC}}
const i1 = traceCurrentCapacity(0.254, 0.035, 10, true);
const i2 = traceCurrentCapacity(0.508, 0.035, 10, true);
console.log(i2 > i1 ? "yes" : "no");`,
			expected: "yes\n",
		},
		{
			name: "internal is half of external",
			code: `{{FUNC}}
const ext = traceCurrentCapacity(0.5, 0.035, 20, true);
const int_ = traceCurrentCapacity(0.5, 0.035, 20, false);
console.log((ext / int_).toFixed(1));`,
			expected: "2.0\n",
		},
	],
};
