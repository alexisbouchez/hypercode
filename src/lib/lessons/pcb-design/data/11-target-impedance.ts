import type { Lesson } from "../../types";

export const targetImpedance: Lesson = {
	id: "target-impedance",
	title: "Target Impedance",
	chapterId: "power-integrity",
	content: `## Target Impedance

**Target impedance** is the maximum PDN (Power Delivery Network) impedance allowed to keep voltage ripple within spec.

\`\`\`
Z_target = VDD × (ΔV/VDD) / I_max
\`\`\`

Where:
- **VDD** = supply voltage (V)
- **ΔV/VDD** = allowed ripple as a fraction of VDD (e.g., 0.05 = 5%)
- **I_max** = maximum current transient (A)

**Example:** A 1.0V core, 5% ripple, 10A transient:
\`Z_target = 1.0 × 0.05 / 10 = 5 mΩ\`

You must keep the PDN impedance below Z_target across the entire frequency range of interest.

### Selecting Decoupling Capacitors

Once you know Z_target, you choose capacitors so that their impedance stays below Z_target near their resonant frequency. The required capacitance to achieve Z_target at frequency f is:

\`\`\`
C_min = 1 / (2π × f × Z_target)
\`\`\`

### Your Task

Implement:
- \`targetImpedance(VDD, rippleFraction, I_max)\` returning Z_target in **milliohms**
- \`minCapacitance(Z_target_mohm, f_MHz)\` returning required capacitance in **μF**`,

	starterCode: `function targetImpedance(VDD, rippleFraction, I_max) {
  // Return Z_target in milliohms
}

function minCapacitance(Z_target_mohm, f_MHz) {
  // C = 1 / (2 * pi * f * Z_target)
  // Return in microfarads
}

// 1.0V core, 5% ripple, 10A transient
const zt = targetImpedance(1.0, 0.05, 10);
console.log(zt.toFixed(1));
// Capacitance needed at 10 MHz
console.log(minCapacitance(zt, 10).toFixed(3));
`,

	solution: `function targetImpedance(VDD, rippleFraction, I_max) {
  return (VDD * rippleFraction / I_max) * 1000;
}

function minCapacitance(Z_target_mohm, f_MHz) {
  const Z = Z_target_mohm / 1000;
  const f = f_MHz * 1e6;
  return (1 / (2 * Math.PI * f * Z)) * 1e6;
}

const zt = targetImpedance(1.0, 0.05, 10);
console.log(zt.toFixed(1));
console.log(minCapacitance(zt, 10).toFixed(3));
`,

	tests: [
		{
			name: "Z_target and min capacitance at 10MHz",
			expected: "5.0\n3.183\n",
		},
		{
			name: "higher current needs lower target impedance",
			code: `{{FUNC}}
const z1 = targetImpedance(1.0, 0.05, 5);
const z2 = targetImpedance(1.0, 0.05, 20);
console.log(z1 > z2 ? "yes" : "no");`,
			expected: "yes\n",
		},
		{
			name: "3.3V 3% 5A target impedance",
			code: `{{FUNC}}
console.log(targetImpedance(3.3, 0.03, 5).toFixed(2));`,
			expected: "19.80\n",
		},
	],
};
