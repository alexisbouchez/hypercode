import type { Lesson } from "../../types";

export const decibelLevelLesson: Lesson = {
	id: "decibel-level",
	title: "Decibel Level",
	chapterId: "intensity-and-perception",
	content: `## The Decibel Scale

Human hearing spans an enormous range — from a pin drop to a jet engine. The decibel (dB) scale compresses this using a logarithm:

\`\`\`
L = 10 \u00d7 log\u2081\u2080(I / I\u2080)
\`\`\`

- **L** — sound pressure level (dB)
- **I** — intensity (W/m\u00b2)
- **I\u2080** = 10\u207b\u00b9\u00b2 W/m\u00b2 — threshold of hearing (reference level)

### Why Logarithm?

Each +10 dB represents a 10\u00d7 increase in intensity. A 60 dB conversation is 10\u2076 times louder than the quietest audible sound.

| I (W/m\u00b2) | L (dB) | Example |
|----------|--------|---------|
| 10\u207b\u00b9\u00b2 | **0** | threshold of hearing |
| 10\u207b\u2076 | **60** | normal conversation |
| 10\u207b\u00b2 | **100** | underground train |
| 1 | **120** | threshold of pain |

### Your Task

Implement \`soundLevel(I)\` returning the level in dB.

Run the code — you'll hear a tone at a level corresponding to 60 dB.`,

	starterCode: `function soundLevel(I) {
  // L = 10 * log10(I / 1e-12)
  return 0;
}

console.log(soundLevel(1e-12).toFixed(2));  // 0.00
console.log(soundLevel(1e-6).toFixed(2));   // 60.00
console.log(soundLevel(1e-2).toFixed(2));   // 100.00
console.log(soundLevel(1).toFixed(2));      // 120.00

// Hear a tone — gain set for moderate loudness
const ac = new AudioContext();
const osc = ac.createOscillator();
const gain = ac.createGain();
osc.frequency.value = 440;
gain.gain.value = 0.15;
osc.connect(gain);
gain.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 1);
`,

	solution: `function soundLevel(I) {
  return 10 * Math.log10(I / 1e-12);
}

console.log(soundLevel(1e-12).toFixed(2));  // 0.00
console.log(soundLevel(1e-6).toFixed(2));   // 60.00
console.log(soundLevel(1e-2).toFixed(2));   // 100.00
console.log(soundLevel(1).toFixed(2));      // 120.00

const ac = new AudioContext();
const osc = ac.createOscillator();
const gain = ac.createGain();
osc.frequency.value = 440;
gain.gain.value = 0.15;
osc.connect(gain);
gain.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 1);
`,

	tests: [
		{
			name: "I=10\u207b\u00b9\u00b2 W/m\u00b2 \u2192 0.00 dB (threshold of hearing)",
			code: `{{FUNC}}
console.log(soundLevel(1e-12).toFixed(2));`,
			expected: "0.00\n",
		},
		{
			name: "I=10\u207b\u2076 W/m\u00b2 \u2192 60.00 dB (conversation)",
			code: `{{FUNC}}
console.log(soundLevel(1e-6).toFixed(2));`,
			expected: "60.00\n",
		},
		{
			name: "I=10\u207b\u00b2 W/m\u00b2 \u2192 100.00 dB (loud)",
			code: `{{FUNC}}
console.log(soundLevel(1e-2).toFixed(2));`,
			expected: "100.00\n",
		},
		{
			name: "I=1 W/m\u00b2 \u2192 120.00 dB (threshold of pain)",
			code: `{{FUNC}}
console.log(soundLevel(1).toFixed(2));`,
			expected: "120.00\n",
		},
	],
};
