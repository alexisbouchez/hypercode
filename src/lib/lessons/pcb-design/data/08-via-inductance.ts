import type { Lesson } from "../../types";

export const viaInductance: Lesson = {
	id: "via-inductance",
	title: "Via Inductance",
	chapterId: "via-design",
	content: `## Via Inductance

At high frequencies, via **inductance** matters more than resistance. A via's parasitic inductance can cause signal integrity problems for fast edge rates.

The via inductance formula (Howard Johnson approximation):
\`\`\`
L = 5.08 × H × (ln(4H/D) + 1) nH
\`\`\`

Where:
- **H** = via height (board thickness) in **inches**
- **D** = via drill diameter in **inches**
- **L** = inductance in **nanohenries (nH)**

Wait — old formulas often use inches. We'll accept mm inputs and convert:
\`\`\`
1 inch = 25.4 mm
H_in = H_mm / 25.4
D_in = D_mm / 25.4
\`\`\`

**Example:** Standard 1.6mm board, 0.3mm drill:
- H_in = 0.0630, D_in = 0.01181
- L = 5.08 × 0.0630 × (ln(4×0.0630/0.01181) + 1) ≈ 5.08 × 0.063 × (3.054 + 1) ≈ 1.30 nH

At 1 GHz, Z = 2π × 1e9 × 1.30e-9 ≈ 8.2 Ω — significant for a 50Ω system!

### Your Task

Implement \`viaInductance(H_mm, D_mm)\` returning inductance in **nH**.`,

	starterCode: `function viaInductance(H_mm, D_mm) {
  // Convert to inches, apply L = 5.08 * H * (ln(4H/D) + 1)
}

// 1.6mm board, 0.3mm drill
console.log(viaInductance(1.6, 0.3).toFixed(3));
// 0.8mm board, 0.2mm drill (blind via)
console.log(viaInductance(0.8, 0.2).toFixed(3));
`,

	solution: `function viaInductance(H_mm, D_mm) {
  const H = H_mm / 25.4;
  const D = D_mm / 25.4;
  return 5.08 * H * (Math.log(4 * H / D) + 1);
}

console.log(viaInductance(1.6, 0.3).toFixed(3));
console.log(viaInductance(0.8, 0.2).toFixed(3));
`,

	tests: [
		{
			name: "standard and blind via inductance",
			expected: "1.299\n0.604\n",
		},
		{
			name: "shorter via has less inductance",
			code: `{{FUNC}}
const l1 = viaInductance(1.6, 0.3);
const l2 = viaInductance(0.4, 0.3);
console.log(l1 > l2 ? "yes" : "no");`,
			expected: "yes\n",
		},
		{
			name: "via impedance at 1GHz exceeds 5 ohm",
			code: `{{FUNC}}
const L = viaInductance(1.6, 0.3) * 1e-9;
const Z = 2 * Math.PI * 1e9 * L;
console.log(Z > 5 ? "yes" : "no");`,
			expected: "yes\n",
		},
	],
};
