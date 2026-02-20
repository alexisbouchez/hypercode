import type { Lesson } from "../../types";

export const dBToGainLesson: Lesson = {
	id: "db-to-gain",
	title: "Decibels to Gain",
	chapterId: "effects-and-timbre",
	content: `## Decibels

**Decibels (dB)** measure sound level on a logarithmic scale, matching how human hearing perceives loudness.

### dB to Linear Gain

The Web Audio API's \`GainNode\` expects a linear gain value (0 to 1+). Convert from dB:

\`\`\`
gain = 10^(dB / 20)
\`\`\`

### Key Reference Points

| dB | Gain | Meaning |
|----|------|---------|
| 0 | 1.0000 | Unity — no change |
| −6 | 0.5012 | Approximately half loudness |
| −20 | 0.1000 | One-tenth amplitude |
| +6 | 1.9953 | Approximately double loudness |
| −∞ | 0 | Silence |

### Why Logarithmic?

A 6 dB increase always doubles the amplitude, regardless of the starting level. This matches how we perceive sound — each doubling of loudness feels like a similar step up.

### Your Task

Implement \`dBToGain(db)\` that converts decibels to linear gain.

Run your code to hear the same note played at 0 dB and −12 dB.`,

	starterCode: `function dBToGain(db) {
  // gain = 10^(db / 20)
  return 0;
}

console.log(dBToGain(0).toFixed(4));   // 1.0000
console.log(dBToGain(-6).toFixed(4));  // 0.5012
console.log(dBToGain(-20).toFixed(4)); // 0.1000
console.log(dBToGain(6).toFixed(4));   // 1.9953

// Hear 0 dB then -12 dB
const ac = new AudioContext();
function playNote(freq, gainDb, startTime) {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = freq;
  gain.gain.value = dBToGain(gainDb);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(startTime);
  osc.stop(startTime + 0.8);
}
playNote(440, 0, ac.currentTime);        // full volume
playNote(440, -12, ac.currentTime + 1);  // quieter
`,

	solution: `function dBToGain(db) {
  return Math.pow(10, db / 20);
}

console.log(dBToGain(0).toFixed(4));   // 1.0000
console.log(dBToGain(-6).toFixed(4));  // 0.5012
console.log(dBToGain(-20).toFixed(4)); // 0.1000
console.log(dBToGain(6).toFixed(4));   // 1.9953

const ac = new AudioContext();
function playNote(freq, gainDb, startTime) {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = freq;
  gain.gain.value = dBToGain(gainDb);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(startTime);
  osc.stop(startTime + 0.8);
}
playNote(440, 0, ac.currentTime);
playNote(440, -12, ac.currentTime + 1);
`,

	tests: [
		{
			name: "0 dB = 1.0000 gain",
			code: `{{FUNC}}
console.log(dBToGain(0).toFixed(4));`,
			expected: "1.0000\n",
		},
		{
			name: "-6 dB ≈ 0.5012 gain",
			code: `{{FUNC}}
console.log(dBToGain(-6).toFixed(4));`,
			expected: "0.5012\n",
		},
		{
			name: "-20 dB = 0.1000 gain",
			code: `{{FUNC}}
console.log(dBToGain(-20).toFixed(4));`,
			expected: "0.1000\n",
		},
		{
			name: "+6 dB ≈ 1.9953 gain",
			code: `{{FUNC}}
console.log(dBToGain(6).toFixed(4));`,
			expected: "1.9953\n",
		},
	],
};
