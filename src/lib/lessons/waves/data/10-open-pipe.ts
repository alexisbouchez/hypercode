import type { Lesson } from "../../types";

export const openPipeLesson: Lesson = {
	id: "open-pipe",
	title: "Open Pipe Harmonics",
	chapterId: "standing-waves",
	content: `## Open Cylindrical Pipe

A pipe open at both ends (flute, organ open pipe) has **pressure antinodes** at each end. The resonant frequencies follow the same formula as a string:

\`\`\`
fₙ = n × v / (2L)        n = 1, 2, 3, …
\`\`\`

- **n** — harmonic number
- **v** = 343 m/s (speed of sound in air)
- **L** — pipe length (m)

### All Harmonics Present

An open pipe supports all integer harmonics — the same as a string. This gives it a bright, rich timbre (flute, recorder, human vowels).

### Changing Pitch

Halving the pipe length doubles the frequency — one octave up. This is why flutes are shorter than tubas.

| n | L=1 m | L=0.5 m |
|---|-------|---------|
| 1 | **171.5000** | **343.0000** |
| 2 | **343.0000** | **686.0000** |
| 3 | **514.5000** | **1029.0000** |

### Your Task

Implement \`openPipeMode(n, L)\` returning the nth resonant frequency (v = 343 m/s).

Run the code to hear the fundamental and second harmonic of a 1-metre open pipe.`,

	starterCode: `function openPipeMode(n, L) {
  // fn = n * 343 / (2 * L)
  return 0;
}

console.log(openPipeMode(1, 1).toFixed(4));    // 171.5000
console.log(openPipeMode(2, 1).toFixed(4));    // 343.0000
console.log(openPipeMode(1, 0.5).toFixed(4)); // 343.0000
console.log(openPipeMode(3, 2).toFixed(4));   // 257.2500

// Hear first three harmonics of a 1 m open pipe
const ac = new AudioContext();
[1, 2, 3].forEach((n, i) => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = openPipeMode(n, 1);
  gain.gain.value = 0.25 / n;
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * 1.1);
  osc.stop(ac.currentTime + i * 1.1 + 1);
});
`,

	solution: `function openPipeMode(n, L) {
  return n * 343 / (2 * L);
}

console.log(openPipeMode(1, 1).toFixed(4));    // 171.5000
console.log(openPipeMode(2, 1).toFixed(4));    // 343.0000
console.log(openPipeMode(1, 0.5).toFixed(4)); // 343.0000
console.log(openPipeMode(3, 2).toFixed(4));   // 257.2500

const ac = new AudioContext();
[1, 2, 3].forEach((n, i) => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = openPipeMode(n, 1);
  gain.gain.value = 0.25 / n;
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * 1.1);
  osc.stop(ac.currentTime + i * 1.1 + 1);
});
`,

	tests: [
		{
			name: "n=1, L=1 m → 171.5000 Hz",
			code: `{{FUNC}}
console.log(openPipeMode(1, 1).toFixed(4));`,
			expected: "171.5000\n",
		},
		{
			name: "n=2, L=1 m → 343.0000 Hz",
			code: `{{FUNC}}
console.log(openPipeMode(2, 1).toFixed(4));`,
			expected: "343.0000\n",
		},
		{
			name: "n=1, L=0.5 m → 343.0000 Hz (half length = double freq)",
			code: `{{FUNC}}
console.log(openPipeMode(1, 0.5).toFixed(4));`,
			expected: "343.0000\n",
		},
		{
			name: "n=3, L=2 m → 257.2500 Hz",
			code: `{{FUNC}}
console.log(openPipeMode(3, 2).toFixed(4));`,
			expected: "257.2500\n",
		},
	],
};
