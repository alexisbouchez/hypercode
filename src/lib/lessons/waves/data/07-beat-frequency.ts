import type { Lesson } from "../../types";

export const beatFrequencyLesson: Lesson = {
	id: "beat-frequency",
	title: "Beat Frequency",
	chapterId: "intensity-and-perception",
	content: `## Acoustic Beats

When two tones of slightly different frequencies f\u2081 and f\u2082 are played together, they alternate between constructive and destructive interference. The result is a pulsing "wah-wah" — **acoustic beats**.

\`\`\`
f_beat = |f\u2081 \u2212 f\u2082|
\`\`\`

The beat frequency equals the absolute difference of the two source frequencies.

### Why It Happens

The two waves periodically come in and out of phase. When aligned (phase difference = 0) their amplitudes add; half a beat period later (phase = \u03c0) they cancel.

### Used for Tuning

Musicians tune by beating: play two strings and listen for the pulsing. Slow the beating until it disappears — the strings are in unison.

| f\u2081 (Hz) | f\u2082 (Hz) | f_beat (Hz) |
|---------|---------|-------------|
| 440 | 441 | **1.0000** |
| 440 | 443 | **3.0000** |
| 880 | 882 | **2.0000** |
| 100 | 104 | **4.0000** |

### Your Task

Implement \`beatFrequency(f1, f2)\` returning the beat frequency.

Run the code and **hear the beats** — two nearly-identical tones pulsing together.`,

	starterCode: `function beatFrequency(f1, f2) {
  // f_beat = |f1 - f2|
  return 0;
}

console.log(beatFrequency(440, 441).toFixed(4));   // 1.0000
console.log(beatFrequency(440, 443).toFixed(4));   // 3.0000
console.log(beatFrequency(880, 882).toFixed(4));   // 2.0000
console.log(beatFrequency(100, 104).toFixed(4));   // 4.0000

// Hear 3 beats per second: 440 Hz + 443 Hz
const ac = new AudioContext();
const g = ac.createGain();
g.gain.value = 0.2;
g.connect(ac.destination);
[440, 443].forEach(freq => {
  const osc = ac.createOscillator();
  osc.frequency.value = freq;
  osc.connect(g);
  osc.start();
  osc.stop(ac.currentTime + 3);
});
`,

	solution: `function beatFrequency(f1, f2) {
  return Math.abs(f1 - f2);
}

console.log(beatFrequency(440, 441).toFixed(4));   // 1.0000
console.log(beatFrequency(440, 443).toFixed(4));   // 3.0000
console.log(beatFrequency(880, 882).toFixed(4));   // 2.0000
console.log(beatFrequency(100, 104).toFixed(4));   // 4.0000

const ac = new AudioContext();
const g = ac.createGain();
g.gain.value = 0.2;
g.connect(ac.destination);
[440, 443].forEach(freq => {
  const osc = ac.createOscillator();
  osc.frequency.value = freq;
  osc.connect(g);
  osc.start();
  osc.stop(ac.currentTime + 3);
});
`,

	tests: [
		{
			name: "440 Hz & 441 Hz \u2192 1.0000 beat/s",
			code: `{{FUNC}}
console.log(beatFrequency(440, 441).toFixed(4));`,
			expected: "1.0000\n",
		},
		{
			name: "440 Hz & 443 Hz \u2192 3.0000 beats/s",
			code: `{{FUNC}}
console.log(beatFrequency(440, 443).toFixed(4));`,
			expected: "3.0000\n",
		},
		{
			name: "880 Hz & 882 Hz \u2192 2.0000 beats/s",
			code: `{{FUNC}}
console.log(beatFrequency(880, 882).toFixed(4));`,
			expected: "2.0000\n",
		},
		{
			name: "100 Hz & 104 Hz \u2192 4.0000 beats/s",
			code: `{{FUNC}}
console.log(beatFrequency(100, 104).toFixed(4));`,
			expected: "4.0000\n",
		},
	],
};
