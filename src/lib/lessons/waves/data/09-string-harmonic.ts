import type { Lesson } from "../../types";

export const stringHarmonicLesson: Lesson = {
	id: "string-harmonic",
	title: "String Harmonics",
	chapterId: "standing-waves",
	content: `## Standing Waves on a String

A stretched string fixed at both ends supports **standing waves**. Only wavelengths that fit a whole number of half-wavelengths between the endpoints are allowed:

\`\`\`
fₙ = n × v / (2L)
\`\`\`

- **n** — harmonic number (1, 2, 3 …)
- **v** — wave speed along the string (m/s)
- **L** — string length (m)
- **fₙ** — frequency of the nth harmonic (Hz)

### Harmonic Series

n = 1 is the **fundamental** (lowest pitch). Higher harmonics are integer multiples — the tone colour (timbre) of an instrument depends on how much energy each harmonic carries.

| n | fₙ (343 m/s, L=1 m) |
|---|---------------------|
| 1 | **171.5000** Hz |
| 2 | **343.0000** Hz |
| 3 | **514.5000** Hz |

### Your Task

Implement \`stringHarmonic(n, v, L)\` returning the frequency of the nth harmonic.

Run the code to hear the fundamental and the second harmonic of a 1-metre string.`,

	starterCode: `function stringHarmonic(n, v, L) {
  // fn = n * v / (2 * L)
  return 0;
}

console.log(stringHarmonic(1, 343, 1).toFixed(4));    // 171.5000
console.log(stringHarmonic(2, 343, 1).toFixed(4));    // 343.0000
console.log(stringHarmonic(3, 343, 1).toFixed(4));    // 514.5000

// Hear fundamental then octave (2nd harmonic)
const ac = new AudioContext();
[1, 2].forEach((n, i) => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = stringHarmonic(n, 343, 1);
  gain.gain.value = 0.3;
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * 1.2);
  osc.stop(ac.currentTime + i * 1.2 + 1);
});
`,

	solution: `function stringHarmonic(n, v, L) {
  return n * v / (2 * L);
}

console.log(stringHarmonic(1, 343, 1).toFixed(4));    // 171.5000
console.log(stringHarmonic(2, 343, 1).toFixed(4));    // 343.0000
console.log(stringHarmonic(3, 343, 1).toFixed(4));    // 514.5000

const ac = new AudioContext();
[1, 2].forEach((n, i) => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = stringHarmonic(n, 343, 1);
  gain.gain.value = 0.3;
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * 1.2);
  osc.stop(ac.currentTime + i * 1.2 + 1);
});
`,

	tests: [
		{
			name: "n=1, v=343, L=1 → 171.5000 Hz (fundamental)",
			code: `{{FUNC}}
console.log(stringHarmonic(1, 343, 1).toFixed(4));`,
			expected: "171.5000\n",
		},
		{
			name: "n=2, v=343, L=1 → 343.0000 Hz (2nd harmonic)",
			code: `{{FUNC}}
console.log(stringHarmonic(2, 343, 1).toFixed(4));`,
			expected: "343.0000\n",
		},
		{
			name: "n=1, v=343, L=0.5 → 343.0000 Hz (shorter string)",
			code: `{{FUNC}}
console.log(stringHarmonic(1, 343, 0.5).toFixed(4));`,
			expected: "343.0000\n",
		},
		{
			name: "n=3, v=343, L=1 → 514.5000 Hz (3rd harmonic)",
			code: `{{FUNC}}
console.log(stringHarmonic(3, 343, 1).toFixed(4));`,
			expected: "514.5000\n",
		},
	],
};
