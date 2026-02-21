import type { Lesson } from "../../types";

export const skinEffect: Lesson = {
	id: "skin-effect",
	title: "Skin Effect",
	chapterId: "signal-integrity",
	content: `## Skin Effect

At high frequencies, current doesn't flow uniformly through a conductor — it concentrates near the surface. This is the **skin effect**, and it increases the effective resistance of traces at high frequencies.

The **skin depth** δ (how deep current penetrates) is:
\`\`\`
δ = √(ρ / (π × f × μ))
\`\`\`

Where:
- **ρ** = resistivity (copper: 1.72 × 10⁻⁸ Ω·m)
- **f** = frequency (Hz)
- **μ** = permeability = μ₀ × μᵣ = 4π × 10⁻⁷ × 1 for copper

At high frequency, the effective cross-section is only a thin shell of thickness δ, increasing resistance.

For a trace of width W and thickness T >> δ, the AC resistance per unit length is approximately:
\`\`\`
R_ac ≈ ρ / (2 × δ × W)   (two sides carrying current)
\`\`\`

### Your Task

Implement:
- \`skinDepth(f_GHz)\` returning skin depth in **micrometers (μm)**
- \`acResistance(f_GHz, widthMm)\` returning AC resistance per meter in **Ω/m** (1oz trace, T >> δ assumption)`,

	starterCode: `function skinDepth(f_GHz) {
  const rho = 1.72e-8;
  const mu = 4 * Math.PI * 1e-7;
  // delta = sqrt(rho / (pi * f * mu))
  // Return in micrometers
}

function acResistance(f_GHz, widthMm) {
  // R_ac = rho / (2 * delta * W) per meter
  // Return in Ohm/m
}

console.log(skinDepth(0.001).toFixed(1)); // 1 MHz
console.log(skinDepth(1).toFixed(2));     // 1 GHz
console.log(skinDepth(10).toFixed(2));    // 10 GHz
console.log(acResistance(1, 0.1).toFixed(1)); // 1GHz, 0.1mm trace
`,

	solution: `function skinDepth(f_GHz) {
  const rho = 1.72e-8;
  const mu = 4 * Math.PI * 1e-7;
  const f = f_GHz * 1e9;
  return Math.sqrt(rho / (Math.PI * f * mu)) * 1e6;
}

function acResistance(f_GHz, widthMm) {
  const rho = 1.72e-8;
  const delta = skinDepth(f_GHz) * 1e-6;
  const W = widthMm / 1000;
  return rho / (2 * delta * W);
}

console.log(skinDepth(0.001).toFixed(1));
console.log(skinDepth(1).toFixed(2));
console.log(skinDepth(10).toFixed(2));
console.log(acResistance(1, 0.1).toFixed(1));
`,

	tests: [
		{
			name: "skin depth at 1MHz, 1GHz, 10GHz and AC resistance",
			expected: "66.0\n2.09\n0.66\n41.2\n",
		},
		{
			name: "skin depth decreases with frequency",
			code: `{{FUNC}}
const d1 = skinDepth(0.1);
const d2 = skinDepth(1);
console.log(d1 > d2 ? "yes" : "no");`,
			expected: "yes\n",
		},
		{
			name: "skin depth scales as 1/sqrt(f)",
			code: `{{FUNC}}
const d1 = skinDepth(1);
const d4 = skinDepth(4);
console.log((d1 / d4).toFixed(1));`,
			expected: "2.0\n",
		},
	],
};
