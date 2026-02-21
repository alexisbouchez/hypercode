import type { Lesson } from "../../types";

export const riseTimeBandwidth: Lesson = {
	id: "rise-time-bandwidth",
	title: "Rise Time and Bandwidth",
	chapterId: "signal-integrity",
	content: `## Rise Time and Bandwidth

Every digital signal has a **rise time** — the time to transition from 10% to 90% of the final voltage. Faster rise times mean higher signal bandwidth, which means more stringent PCB design requirements.

The relationship between rise time and bandwidth (using a Gaussian approximation):
\`\`\`
BW = 0.35 / t_rise
\`\`\`

Where:
- **BW** = bandwidth in Hz
- **t_rise** = 10-90% rise time in seconds

This bandwidth tells you the **highest frequency content** in the signal. Your PCB traces must support this bandwidth without excessive loss or reflection.

### Critical Length Rule

A trace is considered **electrically long** (transmission line behavior must be modeled) when:
\`\`\`
L_critical = t_rise × v / 6
\`\`\`

Where:
- **v** = signal velocity ≈ c / √εr_eff (propagation velocity)
- For FR4 microstrip: v ≈ c / √2.6 ≈ 1.86 × 10⁸ m/s

Any trace longer than L_critical needs controlled impedance.

### Your Task

Implement:
- \`signalBandwidth(t_rise_ps)\` returning bandwidth in **GHz**
- \`criticalLength(t_rise_ps, v_mmps)\` returning critical trace length in **mm**
  - \`v_mmps\` = signal velocity in mm/ps (default FR4: ~0.186 mm/ps)`,

	starterCode: `function signalBandwidth(t_rise_ps) {
  // BW = 0.35 / t_rise, return in GHz
}

function criticalLength(t_rise_ps, v_mmps) {
  // L = t_rise * v / 6, return in mm
}

// USB 2.0: rise time ~500ps
console.log(signalBandwidth(500).toFixed(3));
console.log(criticalLength(500, 0.186).toFixed(1));

// PCIe Gen3: rise time ~100ps
console.log(signalBandwidth(100).toFixed(3));
console.log(criticalLength(100, 0.186).toFixed(1));
`,

	solution: `function signalBandwidth(t_rise_ps) {
  const t = t_rise_ps * 1e-12;
  return 0.35 / t / 1e9;
}

function criticalLength(t_rise_ps, v_mmps) {
  return t_rise_ps * v_mmps / 6;
}

console.log(signalBandwidth(500).toFixed(3));
console.log(criticalLength(500, 0.186).toFixed(1));
console.log(signalBandwidth(100).toFixed(3));
console.log(criticalLength(100, 0.186).toFixed(1));
`,

	tests: [
		{
			name: "USB 2.0 and PCIe Gen3 bandwidth and critical length",
			expected: "0.700\n15.5\n3.500\n3.1\n",
		},
		{
			name: "faster rise time means more bandwidth",
			code: `{{FUNC}}
const bw1 = signalBandwidth(1000);
const bw2 = signalBandwidth(100);
console.log(bw2 > bw1 ? "yes" : "no");`,
			expected: "yes\n",
		},
		{
			name: "faster signals have shorter critical length",
			code: `{{FUNC}}
const l1 = criticalLength(1000, 0.186);
const l2 = criticalLength(100, 0.186);
console.log(l1 > l2 ? "yes" : "no");`,
			expected: "yes\n",
		},
	],
};
