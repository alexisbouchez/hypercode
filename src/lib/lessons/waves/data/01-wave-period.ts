import type { Lesson } from "../../types";

export const wavePeriodLesson: Lesson = {
	id: "wave-period",
	title: "Wave Period",
	chapterId: "wave-fundamentals",
	content: `## Wave Period & Frequency

A wave oscillates in time. Two quantities describe this:

$$T = \frac{1}{f} \qquad f = \frac{1}{T}$$

- **T** — period (s): time for one complete cycle
- **f** — frequency (Hz): cycles per second

### The Audible Spectrum

| Frequency | Period | Sound |
|-----------|--------|-------|
| 20 Hz | 0.050000 s | lowest audible bass |
| 440 Hz | 0.002273 s | concert A4 |
| 1000 Hz | 0.001000 s | reference tone |
| 20 000 Hz | 0.000050 s | upper hearing limit |

### Your Task

Implement \`wavePeriod(f)\` returning the period in seconds.

Run the code to hear a 440 Hz tone — one cycle every 0.002273 seconds.`,

	starterCode: `function wavePeriod(f) {
  // T = 1 / f
  return 0;
}

console.log(wavePeriod(440).toFixed(6));   // 0.002273
console.log(wavePeriod(1000).toFixed(6));  // 0.001000

// Hear a 440 Hz tone
const ac = new AudioContext();
const osc = ac.createOscillator();
const gain = ac.createGain();
osc.frequency.value = 440;
gain.gain.value = 0.3;
osc.connect(gain);
gain.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 1);
`,

	solution: `function wavePeriod(f) {
  return 1 / f;
}

console.log(wavePeriod(440).toFixed(6));   // 0.002273
console.log(wavePeriod(1000).toFixed(6));  // 0.001000

const ac = new AudioContext();
const osc = ac.createOscillator();
const gain = ac.createGain();
osc.frequency.value = 440;
gain.gain.value = 0.3;
osc.connect(gain);
gain.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 1);
`,

	tests: [
		{
			name: "f=440 Hz \u2192 T=0.002273 s",
			code: `{{FUNC}}
console.log(wavePeriod(440).toFixed(6));`,
			expected: "0.002273\n",
		},
		{
			name: "f=1000 Hz \u2192 T=0.001000 s",
			code: `{{FUNC}}
console.log(wavePeriod(1000).toFixed(6));`,
			expected: "0.001000\n",
		},
		{
			name: "f=20 Hz \u2192 T=0.050000 s",
			code: `{{FUNC}}
console.log(wavePeriod(20).toFixed(6));`,
			expected: "0.050000\n",
		},
		{
			name: "f=20000 Hz \u2192 T=0.000050 s",
			code: `{{FUNC}}
console.log(wavePeriod(20000).toFixed(6));`,
			expected: "0.000050\n",
		},
	],
};
