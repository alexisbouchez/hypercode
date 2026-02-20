import type { Lesson } from "../../types";

export const intervalRatioLesson: Lesson = {
	id: "interval-ratio",
	title: "Interval Ratios",
	chapterId: "chords-and-harmony",
	content: `## Intervals and Frequency Ratios

An **interval** is the distance between two notes measured in semitones.

In 12-TET, every interval has a precise frequency ratio:

\`\`\`
ratio = 2^(semitones / 12)
\`\`\`

### Common Intervals

| Semitones | Name | Ratio |
|-----------|------|-------|
| 0 | Unison | 1.0000 |
| 3 | Minor 3rd | 1.1892 |
| 4 | Major 3rd | 1.2599 |
| 7 | Perfect 5th | 1.4983 |
| 12 | Octave | 2.0000 |

The **perfect fifth** (7 semitones) has ratio ≈ 3/2, making it one of the most consonant intervals. Two notes a perfect fifth apart sound naturally "in tune" together.

### From Ratio to Frequency

If the root is at frequency \`f\`, the note \`n\` semitones above is at:

\`\`\`
f × intervalRatio(n)
\`\`\`

### Your Task

Implement \`intervalRatio(semitones)\` that returns the frequency ratio.

Run your code to hear A4 and its perfect fifth (E5) played together.`,

	starterCode: `function intervalRatio(semitones) {
  // ratio = 2^(semitones / 12)
  return 0;
}

console.log(intervalRatio(12).toFixed(4)); // 2.0000 (octave)
console.log(intervalRatio(7).toFixed(4));  // 1.4983 (perfect 5th)
console.log(intervalRatio(4).toFixed(4));  // 1.2599 (major 3rd)
console.log(intervalRatio(0).toFixed(4));  // 1.0000 (unison)

// Hear A4 + E5 (perfect fifth above A4)
const baseFreq = 440;
const ratio = intervalRatio(7);
const ac = new AudioContext();
[baseFreq, baseFreq * ratio].forEach((freq, i) => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = freq;
  gain.gain.setValueAtTime(0.25, ac.currentTime);
  gain.gain.linearRampToValueAtTime(0, ac.currentTime + 1.5);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * 0.05);
  osc.stop(ac.currentTime + 1.5);
});
`,

	solution: `function intervalRatio(semitones) {
  return Math.pow(2, semitones / 12);
}

console.log(intervalRatio(12).toFixed(4)); // 2.0000
console.log(intervalRatio(7).toFixed(4));  // 1.4983
console.log(intervalRatio(4).toFixed(4));  // 1.2599
console.log(intervalRatio(0).toFixed(4));  // 1.0000

const baseFreq = 440;
const ratio = intervalRatio(7);
const ac = new AudioContext();
[baseFreq, baseFreq * ratio].forEach((freq, i) => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = freq;
  gain.gain.setValueAtTime(0.25, ac.currentTime);
  gain.gain.linearRampToValueAtTime(0, ac.currentTime + 1.5);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * 0.05);
  osc.stop(ac.currentTime + 1.5);
});
`,

	tests: [
		{
			name: "Octave (12 semitones) = 2.0000",
			code: `{{FUNC}}
console.log(intervalRatio(12).toFixed(4));`,
			expected: "2.0000\n",
		},
		{
			name: "Perfect 5th (7 semitones) = 1.4983",
			code: `{{FUNC}}
console.log(intervalRatio(7).toFixed(4));`,
			expected: "1.4983\n",
		},
		{
			name: "Unison (0 semitones) = 1.0000",
			code: `{{FUNC}}
console.log(intervalRatio(0).toFixed(4));`,
			expected: "1.0000\n",
		},
		{
			name: "Major 3rd (4 semitones) = 1.2599",
			code: `{{FUNC}}
console.log(intervalRatio(4).toFixed(4));`,
			expected: "1.2599\n",
		},
	],
};
