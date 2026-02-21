import type { Lesson } from "../../types";

export const viaResistance: Lesson = {
	id: "via-resistance",
	title: "Via Resistance",
	chapterId: "via-design",
	content: `## Via Resistance

A **via** is a plated hole that connects traces on different PCB layers. Like a trace, a via has electrical resistance due to the thin copper plating on its barrel walls.

The via resistance formula:
\`\`\`
R = ρ × H / (π × D × T_plating)
\`\`\`

Where:
- **ρ** = copper resistivity = 1.72 × 10⁻⁸ Ω·m
- **H** = via height (board thickness), meters
- **D** = via drill diameter, meters
- **T_plating** = copper plating thickness on via wall, meters

A typical via:
- Board thickness: 1.6 mm
- Drill diameter: 0.3 mm
- Plating thickness: 25 μm (standard spec)

\`R = 1.72×10⁻⁸ × 0.0016 / (π × 0.0003 × 0.000025) ≈ 1.17 mΩ\`

Vias are very low resistance — typically < 5 mΩ — so they rarely cause voltage drop problems.

### Your Task

Implement \`viaResistance(H_mm, D_mm, T_mm)\` returning resistance in **milliohms** (mΩ).`,

	starterCode: `function viaResistance(H_mm, D_mm, T_mm) {
  const rho = 1.72e-8;
  // Convert mm to m, apply formula, return in milliohms
}

// Standard via: 1.6mm board, 0.3mm drill, 25μm plating
console.log(viaResistance(1.6, 0.3, 0.025).toFixed(3));
// Thin board: 0.8mm, 0.2mm drill, 25μm plating
console.log(viaResistance(0.8, 0.2, 0.025).toFixed(3));
`,

	solution: `function viaResistance(H_mm, D_mm, T_mm) {
  const rho = 1.72e-8;
  const H = H_mm / 1000;
  const D = D_mm / 1000;
  const T = T_mm / 1000;
  return (rho * H / (Math.PI * D * T)) * 1000;
}

console.log(viaResistance(1.6, 0.3, 0.025).toFixed(3));
console.log(viaResistance(0.8, 0.2, 0.025).toFixed(3));
`,

	tests: [
		{
			name: "standard and thin board vias",
			expected: "1.168\n0.876\n",
		},
		{
			name: "thicker plating reduces resistance",
			code: `{{FUNC}}
const r1 = viaResistance(1.6, 0.3, 0.025);
const r2 = viaResistance(1.6, 0.3, 0.050);
console.log((r1 / r2).toFixed(1));`,
			expected: "2.0\n",
		},
		{
			name: "via resistance is very low",
			code: `{{FUNC}}
const r = viaResistance(1.6, 0.3, 0.025);
console.log(r < 5 ? "yes" : "no");`,
			expected: "yes\n",
		},
	],
};
