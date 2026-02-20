import type { Lesson } from "../../types";

export const soundPowerLevelLesson: Lesson = {
	id: "sound-power-level",
	title: "Sound Power Level",
	chapterId: "room-acoustics",
	content: `## Sound Power Level

**Sound power** W (watts) measures total acoustic energy per second radiated by a source. Like sound pressure level, it is expressed on a logarithmic scale:

\`\`\`
L_W = 10 × log₁₀(W / W₀)
\`\`\`

- **W** — sound power (W)
- **W₀** = 10⁻¹² W — reference sound power
- **L_W** — sound power level (dB re 1 pW)

### Power Level vs Pressure Level

Sound power level is a **source property** — it doesn't depend on distance or room acoustics. Sound pressure level (SPL) depends on where you stand. A louder room or closer listener gives higher SPL from the same source.

| W (watts) | L_W (dB) | Example |
|-----------|---------|---------|
| 10⁻¹² | **0** | threshold of hearing |
| 10⁻⁶ | **60** | whisper |
| 1 | **120** | rock singer |
| 100 | **140** | jet engine |

### Your Task

Implement \`soundPowerLevel(W)\` returning L_W in dB.`,

	starterCode: `function soundPowerLevel(W) {
  // Lw = 10 * log10(W / 1e-12)
  return 0;
}

console.log(soundPowerLevel(1e-12).toFixed(2));   // 0.00
console.log(soundPowerLevel(1e-6).toFixed(2));    // 60.00
console.log(soundPowerLevel(1).toFixed(2));       // 120.00
console.log(soundPowerLevel(100).toFixed(2));     // 140.00
`,

	solution: `function soundPowerLevel(W) {
  return 10 * Math.log10(W / 1e-12);
}

console.log(soundPowerLevel(1e-12).toFixed(2));   // 0.00
console.log(soundPowerLevel(1e-6).toFixed(2));    // 60.00
console.log(soundPowerLevel(1).toFixed(2));       // 120.00
console.log(soundPowerLevel(100).toFixed(2));     // 140.00
`,

	tests: [
		{
			name: "W=10⁻¹² W → 0.00 dB",
			code: `{{FUNC}}
console.log(soundPowerLevel(1e-12).toFixed(2));`,
			expected: "0.00\n",
		},
		{
			name: "W=10⁻⁶ W → 60.00 dB",
			code: `{{FUNC}}
console.log(soundPowerLevel(1e-6).toFixed(2));`,
			expected: "60.00\n",
		},
		{
			name: "W=1 W → 120.00 dB",
			code: `{{FUNC}}
console.log(soundPowerLevel(1).toFixed(2));`,
			expected: "120.00\n",
		},
		{
			name: "W=100 W → 140.00 dB",
			code: `{{FUNC}}
console.log(soundPowerLevel(100).toFixed(2));`,
			expected: "140.00\n",
		},
	],
};
