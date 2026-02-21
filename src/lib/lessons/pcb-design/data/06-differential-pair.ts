import type { Lesson } from "../../types";

export const differentialPair: Lesson = {
	id: "differential-pair",
	title: "Differential Pair Impedance",
	chapterId: "impedance-control",
	content: `## Differential Pair Impedance

High-speed protocols like USB, Ethernet, PCIe, and HDMI use **differential signaling**: two traces carry the same signal with opposite polarity. This rejects common-mode noise and allows higher speeds.

For a **differential microstrip pair**, the differential impedance depends on how closely the two traces are spaced:

\`\`\`
Zdiff = 2 × Z₀ × (1 - 0.347 × e^(-2.9 × S/H))
\`\`\`

Where:
- **Z₀** = single-ended microstrip impedance of each trace
- **S** = spacing between traces (edge-to-edge), same units as H
- **H** = dielectric height to ground plane

When S is large compared to H, the traces don't interact and Zdiff ≈ 2×Z₀. When the traces are close, mutual coupling reduces Zdiff.

Common targets:
- **90 Ω** differential: USB 2.0, USB 3.0
- **100 Ω** differential: Ethernet, HDMI, PCIe

### Your Task

Implement \`diffPairImpedance(z0, S, H)\` returning differential impedance in ohms.

Then find the spacing S needed to achieve 90Ω differential for Z₀=45Ω, H=0.2mm.`,

	starterCode: `function diffPairImpedance(z0, S, H) {
  // Zdiff = 2 * z0 * (1 - 0.347 * exp(-2.9 * S/H))
}

// Z0=45Ω, H=0.2mm, various spacings
console.log(diffPairImpedance(45, 0.1, 0.2).toFixed(1));
console.log(diffPairImpedance(45, 0.2, 0.2).toFixed(1));
console.log(diffPairImpedance(45, 0.5, 0.2).toFixed(1));
`,

	solution: `function diffPairImpedance(z0, S, H) {
  return 2 * z0 * (1 - 0.347 * Math.exp(-2.9 * S / H));
}

console.log(diffPairImpedance(45, 0.1, 0.2).toFixed(1));
console.log(diffPairImpedance(45, 0.2, 0.2).toFixed(1));
console.log(diffPairImpedance(45, 0.5, 0.2).toFixed(1));
`,

	tests: [
		{
			name: "spacing 0.1, 0.2, 0.5 with Z0=45",
			expected: "82.7\n88.3\n90.0\n",
		},
		{
			name: "larger spacing approaches 2*Z0",
			code: `{{FUNC}}
const zdiff = diffPairImpedance(50, 10, 0.2);
console.log(zdiff.toFixed(0));`,
			expected: "100\n",
		},
		{
			name: "tighter spacing reduces impedance",
			code: `{{FUNC}}
const z1 = diffPairImpedance(50, 0.05, 0.2);
const z2 = diffPairImpedance(50, 0.5, 0.2);
console.log(z1 < z2 ? "yes" : "no");`,
			expected: "yes\n",
		},
	],
};
