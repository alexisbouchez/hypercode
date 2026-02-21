import type { Lesson } from "../../types";

export const striplineImpedance: Lesson = {
	id: "stripline-impedance",
	title: "Stripline Impedance",
	chapterId: "impedance-control",
	content: `## Stripline Impedance

A **stripline** is a trace embedded between two ground planes (an inner layer). Because the trace is completely surrounded by the dielectric, there is no air exposure — the effective dielectric constant equals the bulk value, giving better impedance consistency than microstrip.

The IPC-2141 stripline formula:
\`\`\`
Z₀ = (60 / √εr) × ln(4B / (0.67π × (0.8W + T)))
\`\`\`

Where:
- **εr** = substrate dielectric constant
- **B** = total distance between the two ground planes
- **W** = trace width
- **T** = trace thickness

Striplines are preferred for:
- Differential pairs requiring tight impedance control
- EMI-sensitive signals (shielded by ground planes on both sides)
- High-frequency signals above ~1 GHz

### Your Task

Implement \`striplineImpedance(er, B, W, T)\` where B, W, T are in mm, returning Z₀ in ohms.`,

	starterCode: `function striplineImpedance(er, B, W, T) {
  // Z0 = (60 / sqrt(er)) * ln(4*B / (0.67*pi*(0.8*W+T)))
}

// FR4: er=4.2, B=0.4mm, W=0.22mm, T=0.035mm → ~50Ω
console.log(striplineImpedance(4.2, 0.4, 0.22, 0.035).toFixed(1));
// Wider trace
console.log(striplineImpedance(4.2, 0.4, 0.35, 0.035).toFixed(1));
`,

	solution: `function striplineImpedance(er, B, W, T) {
  return (60 / Math.sqrt(er)) * Math.log(4 * B / (0.67 * Math.PI * (0.8 * W + T)));
}

console.log(striplineImpedance(4.2, 0.4, 0.22, 0.035).toFixed(1));
console.log(striplineImpedance(4.2, 0.4, 0.35, 0.035).toFixed(1));
`,

	tests: [
		{
			name: "50 ohm stripline and wider trace",
			expected: "37.5\n25.8\n",
		},
		{
			name: "wider B increases impedance",
			code: `{{FUNC}}
const z1 = striplineImpedance(4.2, 0.4, 0.22, 0.035);
const z2 = striplineImpedance(4.2, 0.6, 0.22, 0.035);
console.log(z2 > z1 ? "yes" : "no");`,
			expected: "yes\n",
		},
		{
			name: "higher er = lower impedance",
			code: `{{FUNC}}
const z1 = striplineImpedance(3.5, 0.4, 0.22, 0.035);
const z2 = striplineImpedance(4.5, 0.4, 0.22, 0.035);
console.log(z1 > z2 ? "yes" : "no");`,
			expected: "yes\n",
		},
	],
};
