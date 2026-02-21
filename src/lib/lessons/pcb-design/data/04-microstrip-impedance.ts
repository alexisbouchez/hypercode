import type { Lesson } from "../../types";

export const microstripImpedance: Lesson = {
	id: "microstrip-impedance",
	title: "Microstrip Impedance",
	chapterId: "impedance-control",
	content: `## Microstrip Impedance

**Impedance control** is critical for high-speed PCB design. When a signal's wavelength approaches the trace length, impedance mismatches cause reflections that corrupt data.

A **microstrip** is a trace on an outer layer with a ground plane below it. Its characteristic impedance Z₀ is determined by the trace geometry and dielectric constant.

The IPC-2141 formula:
\`\`\`
Z₀ = (87 / √(εr + 1.41)) × ln(5.98H / (0.8W + T))
\`\`\`

Where:
- **εr** = substrate dielectric constant
- **H** = height of dielectric (distance to ground plane), in the same units as W and T
- **W** = trace width
- **T** = trace thickness

Common target impedances:
- **50 Ω** — single-ended signals (RF, USB, Ethernet)
- **90 Ω** differential — USB, PCIe
- **100 Ω** differential — Ethernet, HDMI

### Your Task

Implement \`microstripImpedance(er, H, W, T)\` where H, W, T are in mm, returning Z₀ in ohms.`,

	starterCode: `function microstripImpedance(er, H, W, T) {
  // Z0 = (87 / sqrt(er + 1.41)) * ln(5.98*H / (0.8*W + T))
}

// FR4: er=4.2, H=0.2mm, W=0.36mm, T=0.035mm → ~50Ω
console.log(microstripImpedance(4.2, 0.2, 0.36, 0.035).toFixed(1));
// Wider trace → lower impedance
console.log(microstripImpedance(4.2, 0.2, 0.6, 0.035).toFixed(1));
`,

	solution: `function microstripImpedance(er, H, W, T) {
  return (87 / Math.sqrt(er + 1.41)) * Math.log(5.98 * H / (0.8 * W + T));
}

console.log(microstripImpedance(4.2, 0.2, 0.36, 0.035).toFixed(1));
console.log(microstripImpedance(4.2, 0.2, 0.6, 0.035).toFixed(1));
`,

	tests: [
		{
			name: "50 ohm microstrip and wider trace",
			expected: "48.1\n30.9\n",
		},
		{
			name: "wider trace has lower impedance",
			code: `{{FUNC}}
const z1 = microstripImpedance(4.2, 0.2, 0.3, 0.035);
const z2 = microstripImpedance(4.2, 0.2, 0.6, 0.035);
console.log(z1 > z2 ? "yes" : "no");`,
			expected: "yes\n",
		},
		{
			name: "higher dielectric = lower impedance",
			code: `{{FUNC}}
const z1 = microstripImpedance(3.0, 0.2, 0.36, 0.035);
const z2 = microstripImpedance(5.0, 0.2, 0.36, 0.035);
console.log(z1 > z2 ? "yes" : "no");`,
			expected: "yes\n",
		},
	],
};
