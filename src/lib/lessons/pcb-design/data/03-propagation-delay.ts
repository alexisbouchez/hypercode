import type { Lesson } from "../../types";

export const propagationDelay: Lesson = {
	id: "propagation-delay",
	title: "Propagation Delay",
	chapterId: "trace-design",
	content: `## Propagation Delay

Signals don't travel instantaneously through PCB traces. The **propagation delay** depends on the dielectric constant (Dk or εr) of the PCB material.

\`\`\`
t_pd = (√εr) / c
\`\`\`

Where:
- **t_pd** = delay per unit length (seconds/meter)
- **εr** = relative permittivity (dielectric constant)
- **c** = speed of light = 2.998 × 10⁸ m/s

Common PCB materials:
| Material | εr |
|----------|----|
| Air | 1.0 |
| FR4 (common PCB) | 4.2 |
| Rogers 4003C | 3.55 |
| PTFE (Teflon) | 2.1 |

For a **microstrip** (trace on outer layer), the effective εr is lower than the bulk value because part of the field travels through air. A common approximation:

\`\`\`
εr_eff ≈ (εr + 1) / 2
\`\`\`

Total delay for a trace of length L (meters):
\`\`\`
T = t_pd × L = L × √εr / c
\`\`\`

### Your Task

Implement \`propagationDelay(lengthMm, er)\` that returns total delay in **picoseconds** (ps) using the effective dielectric constant approximation for microstrip.`,

	starterCode: `function propagationDelay(lengthMm, er) {
  const c = 2.998e8; // m/s
  // Use effective er = (er + 1) / 2
  // Return delay in picoseconds
}

// 100mm trace on FR4 (er=4.2)
console.log(propagationDelay(100, 4.2).toFixed(2));
// 100mm trace on Rogers 4003C (er=3.55)
console.log(propagationDelay(100, 3.55).toFixed(2));
`,

	solution: `function propagationDelay(lengthMm, er) {
  const c = 2.998e8;
  const erEff = (er + 1) / 2;
  const L = lengthMm / 1000;
  const tpd = Math.sqrt(erEff) / c;
  return tpd * L * 1e12;
}

console.log(propagationDelay(100, 4.2).toFixed(2));
console.log(propagationDelay(100, 3.55).toFixed(2));
`,

	tests: [
		{
			name: "FR4 and Rogers 4003C 100mm",
			expected: "537.84\n503.11\n",
		},
		{
			name: "delay scales linearly with length",
			code: `{{FUNC}}
const d1 = propagationDelay(100, 4.2);
const d2 = propagationDelay(200, 4.2);
console.log((d2 / d1).toFixed(1));`,
			expected: "2.0\n",
		},
		{
			name: "air has minimum delay",
			code: `{{FUNC}}
const dAir = propagationDelay(100, 1.0);
const dFR4 = propagationDelay(100, 4.2);
console.log(dAir < dFR4 ? "yes" : "no");`,
			expected: "yes\n",
		},
	],
};
