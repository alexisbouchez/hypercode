import type { Lesson } from "../../types";

export const parallelCapacitors: Lesson = {
	id: "parallel-capacitors",
	title: "Parallel Decoupling Strategy",
	chapterId: "power-integrity",
	content: `## Parallel Decoupling Strategy

A single capacitor only decouples effectively near its resonant frequency. For broad-spectrum decoupling, you **parallelize capacitors of different values** — each one handles a different frequency range.

**Anti-resonance problem:** Two parallel capacitors with different SRFs create an anti-resonance (impedance peak) between their resonant frequencies. This can cause worse noise than a single capacitor!

The impedance of N parallel capacitors (assuming they don't interact):
\`\`\`
1/Z_total = 1/Z₁ + 1/Z₂ + ... + 1/Zₙ
\`\`\`

### The Strategy

A well-designed PDN uses a **capacitance hierarchy**:
- **Bulk caps** (10–470 μF): low-frequency (kHz range) bulk storage
- **Ceramic MLCC** (100 nF–10 μF): mid-frequency (1–100 MHz)  
- **Small ceramics** (1–100 nF): high-frequency (100 MHz+)

Each capacitor's effective frequency range: from ~SRF/10 to ~SRF×10.

### Your Task

Implement \`parallelImpedance(capacitors, f_MHz)\` where \`capacitors\` is an array of \`{C_nF, ESR_ohm, ESL_nH}\` objects.

Compute total impedance at frequency f_MHz using parallel combination.`,

	starterCode: `function capImpedance(f_MHz, C_nF, ESR_ohm, ESL_nH) {
  const f = f_MHz * 1e6;
  const C = C_nF * 1e-9;
  const L = ESL_nH * 1e-9;
  const Xc = 1 / (2 * Math.PI * f * C);
  const Xl = 2 * Math.PI * f * L;
  return Math.sqrt(ESR_ohm * ESR_ohm + (Xl - Xc) * (Xl - Xc));
}

function parallelImpedance(capacitors, f_MHz) {
  // Sum 1/Z for each cap, return 1/total
}

const caps = [
  { C_nF: 100, ESR_ohm: 0.05, ESL_nH: 1.0 },  // SRF ~16 MHz
  { C_nF: 10,  ESR_ohm: 0.05, ESL_nH: 0.4 },  // SRF ~80 MHz
];

console.log(parallelImpedance(caps, 16).toFixed(4));
console.log(parallelImpedance(caps, 80).toFixed(4));
`,

	solution: `function capImpedance(f_MHz, C_nF, ESR_ohm, ESL_nH) {
  const f = f_MHz * 1e6;
  const C = C_nF * 1e-9;
  const L = ESL_nH * 1e-9;
  const Xc = 1 / (2 * Math.PI * f * C);
  const Xl = 2 * Math.PI * f * L;
  return Math.sqrt(ESR_ohm * ESR_ohm + (Xl - Xc) * (Xl - Xc));
}

function parallelImpedance(capacitors, f_MHz) {
  let invSum = 0;
  for (const cap of capacitors) {
    invSum += 1 / capImpedance(f_MHz, cap.C_nF, cap.ESR_ohm, cap.ESL_nH);
  }
  return 1 / invSum;
}

const caps = [
  { C_nF: 100, ESR_ohm: 0.05, ESL_nH: 1.0 },
  { C_nF: 10,  ESR_ohm: 0.05, ESL_nH: 0.4 },
];

console.log(parallelImpedance(caps, 16).toFixed(4));
console.log(parallelImpedance(caps, 80).toFixed(4));
`,

	tests: [
		{
			name: "parallel impedance at 16MHz and 80MHz",
			expected: "0.0475\n0.0454\n",
		},
		{
			name: "parallel impedance lower than individual",
			code: `{{FUNC}}
const caps = [{ C_nF: 100, ESR_ohm: 0.05, ESL_nH: 1 }];
const zSingle = parallelImpedance(caps, 16);
const caps2 = [
  { C_nF: 100, ESR_ohm: 0.05, ESL_nH: 1 },
  { C_nF: 100, ESR_ohm: 0.05, ESL_nH: 1 },
];
const zDouble = parallelImpedance(caps2, 16);
console.log(zDouble < zSingle ? "yes" : "no");`,
			expected: "yes\n",
		},
		{
			name: "two identical caps halve impedance",
			code: `{{FUNC}}
const cap = { C_nF: 100, ESR_ohm: 0.05, ESL_nH: 1 };
const z1 = parallelImpedance([cap], 16);
const z2 = parallelImpedance([cap, cap], 16);
console.log((z1 / z2).toFixed(1));`,
			expected: "2.0\n",
		},
	],
};
