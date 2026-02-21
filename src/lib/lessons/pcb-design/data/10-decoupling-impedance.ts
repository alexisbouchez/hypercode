import type { Lesson } from "../../types";

export const decouplingImpedance: Lesson = {
	id: "decoupling-impedance",
	title: "Decoupling Capacitor Impedance",
	chapterId: "power-integrity",
	content: `## Decoupling Capacitor Impedance

A real capacitor's impedance is not just \`1/(2πfC)\`. It includes:
- **ESR** (Equivalent Series Resistance): resistive losses
- **ESL** (Equivalent Series Inductance): inductive parasitics

Total impedance at frequency f:
\`\`\`
Z = √(ESR² + (2πf·ESL - 1/(2πf·C))²)
\`\`\`

At the **self-resonant frequency**, the capacitive and inductive reactances cancel, leaving only ESR — the minimum impedance point.

The capacitor is only effective as a decoupler **near its resonant frequency** where Z is minimized.

### Frequency Regions
- **f < SRF**: capacitive — Z decreases as f increases
- **f = SRF**: Z = ESR (minimum impedance)
- **f > SRF**: inductive — Z increases as f increases

### Your Task

Implement \`capImpedance(f_MHz, C_nF, ESR_ohm, ESL_nH)\` returning impedance in ohms.

Then find the frequency of minimum impedance for a 100nF, 50mΩ ESR, 1nH ESL capacitor by evaluating at 1, 10, and 16 MHz.`,

	starterCode: `function capImpedance(f_MHz, C_nF, ESR_ohm, ESL_nH) {
  const f = f_MHz * 1e6;
  const C = C_nF * 1e-9;
  const L = ESL_nH * 1e-9;
  // Z = sqrt(ESR^2 + (2*pi*f*L - 1/(2*pi*f*C))^2)
}

// 100nF, ESR=50mΩ, ESL=1nH
console.log(capImpedance(1, 100, 0.05, 1).toFixed(4));
console.log(capImpedance(10, 100, 0.05, 1).toFixed(4));
console.log(capImpedance(15.92, 100, 0.05, 1).toFixed(4));
`,

	solution: `function capImpedance(f_MHz, C_nF, ESR_ohm, ESL_nH) {
  const f = f_MHz * 1e6;
  const C = C_nF * 1e-9;
  const L = ESL_nH * 1e-9;
  const Xc = 1 / (2 * Math.PI * f * C);
  const Xl = 2 * Math.PI * f * L;
  return Math.sqrt(ESR_ohm * ESR_ohm + (Xl - Xc) * (Xl - Xc));
}

console.log(capImpedance(1, 100, 0.05, 1).toFixed(4));
console.log(capImpedance(10, 100, 0.05, 1).toFixed(4));
console.log(capImpedance(15.92, 100, 0.05, 1).toFixed(4));
`,

	tests: [
		{
			name: "impedance at 1MHz, 10MHz, 15.92MHz (SRF)",
			expected: "1.5861\n0.1085\n0.0500\n",
		},
		{
			name: "impedance at SRF equals ESR",
			code: `{{FUNC}}
const z = capImpedance(15.92, 100, 0.05, 1);
console.log(Math.abs(z - 0.05) < 0.001 ? "yes" : "no");`,
			expected: "yes\n",
		},
		{
			name: "impedance above SRF is higher than at SRF",
			code: `{{FUNC}}
const zAt = capImpedance(15.92, 100, 0.05, 1);
const zAbove = capImpedance(100, 100, 0.05, 1);
console.log(zAbove > zAt ? "yes" : "no");`,
			expected: "yes\n",
		},
	],
};
