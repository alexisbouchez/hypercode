import type { Lesson } from "../../types";

export const gainToDbLesson: Lesson = {
	id: "gain-to-db",
	title: "Gain to Decibels",
	chapterId: "effects-and-timbre",
	content: `## The Inverse: Gain → dB

In the previous lesson we converted dB → gain. The inverse converts a linear gain value back to decibels:

\`\`\`
dB = 20 × log₁₀(gain)
\`\`\`

### When Is This Useful?

- **VU meters**: show the current signal level in dB
- **Metering plugins**: display loudness in LUFS/dBFS
- **Normalisation**: "how many dB do I need to boost this to reach 0 dBFS?"

### Key Values

| Gain | dB |
|------|----|
| 1.0 | 0.00 |
| 0.5 | −6.02 |
| 0.1 | −20.00 |
| 2.0 | +6.02 |

Note: doubling the amplitude adds exactly 6.02 dB (20 × log₁₀(2)).

### Your Task

Implement \`gainToDb(gain)\` that converts a linear gain to decibels.

Run your code to see a rising volume displayed in dB.`,

	starterCode: `function gainToDb(gain) {
  // dB = 20 × log10(gain)
  return 0;
}

console.log(gainToDb(1).toFixed(2));   // 0.00
console.log(gainToDb(0.5).toFixed(2)); // -6.02
console.log(gainToDb(0.1).toFixed(2)); // -20.00
console.log(gainToDb(2).toFixed(2));   // 6.02

// Show gain ramping up, display in dB
const ac = new AudioContext();
const osc = ac.createOscillator();
const gainNode = ac.createGain();
osc.frequency.value = 440;
gainNode.gain.value = 0;
osc.connect(gainNode);
gainNode.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 2);

// Simulate reading gain at intervals and logging in dB
[0.1, 0.2, 0.4, 0.7, 1.0].forEach((g, i) => {
  setTimeout(() => {
    gainNode.gain.value = g;
    console.log(gainToDb(g).toFixed(1) + " dB");
  }, i * 300);
});
`,

	solution: `function gainToDb(gain) {
  return 20 * Math.log10(gain);
}

console.log(gainToDb(1).toFixed(2));   // 0.00
console.log(gainToDb(0.5).toFixed(2)); // -6.02
console.log(gainToDb(0.1).toFixed(2)); // -20.00
console.log(gainToDb(2).toFixed(2));   // 6.02

const ac = new AudioContext();
const osc = ac.createOscillator();
const gainNode = ac.createGain();
osc.frequency.value = 440;
gainNode.gain.value = 0;
osc.connect(gainNode);
gainNode.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 2);

[0.1, 0.2, 0.4, 0.7, 1.0].forEach((g, i) => {
  setTimeout(() => {
    gainNode.gain.value = g;
    console.log(gainToDb(g).toFixed(1) + " dB");
  }, i * 300);
});
`,

	tests: [
		{
			name: "Gain 1.0 = 0.00 dB",
			code: `{{FUNC}}
console.log(gainToDb(1).toFixed(2));`,
			expected: "0.00\n",
		},
		{
			name: "Gain 0.5 = -6.02 dB",
			code: `{{FUNC}}
console.log(gainToDb(0.5).toFixed(2));`,
			expected: "-6.02\n",
		},
		{
			name: "Gain 0.1 = -20.00 dB",
			code: `{{FUNC}}
console.log(gainToDb(0.1).toFixed(2));`,
			expected: "-20.00\n",
		},
		{
			name: "Gain 2.0 = 6.02 dB",
			code: `{{FUNC}}
console.log(gainToDb(2).toFixed(2));`,
			expected: "6.02\n",
		},
	],
};
