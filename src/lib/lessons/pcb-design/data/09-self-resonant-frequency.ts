import type { Lesson } from "../../types";

export const selfResonantFrequency: Lesson = {
	id: "self-resonant-frequency",
	title: "Self-Resonant Frequency",
	chapterId: "power-integrity",
	content: `## Self-Resonant Frequency (SRF)

Every real capacitor has **parasitic inductance** (ESL — Equivalent Series Inductance) from its leads and internal structure. This creates an LC circuit that resonates at the **self-resonant frequency (SRF)**:

\`\`\`
f_SRF = 1 / (2π × √(L × C))
\`\`\`

Where:
- **L** = ESL in Henries
- **C** = capacitance in Farads

Below SRF: the component behaves as a capacitor (impedance decreases with frequency).
Above SRF: the component behaves as an inductor (impedance increases with frequency).

**This is critical for decoupling!** A 100nF capacitor with 1nH ESL resonates at:
\`f = 1 / (2π × √(1e-9 × 100e-9)) ≈ 15.9 MHz\`

Above 15.9 MHz, it stops decoupling! You need smaller capacitors (lower ESL) for higher frequencies.

Typical ESL values:
| Package | ESL |
|---------|-----|
| 0402 | ~0.4 nH |
| 0201 | ~0.2 nH |
| 0603 | ~0.8 nH |

### Your Task

Implement \`selfResonantFreq(C_nF, L_nH)\` returning SRF in **MHz**.`,

	starterCode: `function selfResonantFreq(C_nF, L_nH) {
  // f = 1 / (2 * pi * sqrt(L * C))
  // Return in MHz
}

// 100nF with 1nH ESL
console.log(selfResonantFreq(100, 1).toFixed(2));
// 10nF with 0.4nH ESL (0402 package)
console.log(selfResonantFreq(10, 0.4).toFixed(2));
// 1nF with 0.4nH ESL
console.log(selfResonantFreq(1, 0.4).toFixed(2));
`,

	solution: `function selfResonantFreq(C_nF, L_nH) {
  const C = C_nF * 1e-9;
  const L = L_nH * 1e-9;
  return (1 / (2 * Math.PI * Math.sqrt(L * C))) / 1e6;
}

console.log(selfResonantFreq(100, 1).toFixed(2));
console.log(selfResonantFreq(10, 0.4).toFixed(2));
console.log(selfResonantFreq(1, 0.4).toFixed(2));
`,

	tests: [
		{
			name: "100nF/1nH, 10nF/0.4nH, 1nF/0.4nH",
			expected: "15.92\n79.58\n251.65\n",
		},
		{
			name: "smaller capacitor has higher SRF",
			code: `{{FUNC}}
const f1 = selfResonantFreq(100, 1);
const f2 = selfResonantFreq(1, 1);
console.log(f2 > f1 ? "yes" : "no");`,
			expected: "yes\n",
		},
		{
			name: "lower ESL has higher SRF",
			code: `{{FUNC}}
const f1 = selfResonantFreq(10, 1.0);
const f2 = selfResonantFreq(10, 0.2);
console.log(f2 > f1 ? "yes" : "no");`,
			expected: "yes\n",
		},
	],
};
