import type { Lesson } from "../../types";

export const closedPipeLesson: Lesson = {
	id: "closed-pipe",
	title: "Closed Pipe Harmonics",
	chapterId: "standing-waves",
	content: `## Closed Cylindrical Pipe

A pipe closed at one end (clarinet, stopped organ pipe) has a **pressure node** at the closed end and a **pressure antinode** at the open end. This boundary condition permits only **odd harmonics**:

\`\`\`
fₙ = (2n − 1) × v / (4L)        n = 1, 2, 3, …
\`\`\`

- n=1 gives the fundamental: f₁ = v / 4L
- n=2 gives the 3rd harmonic: f₂ = 3v / 4L
- n=3 gives the 5th harmonic: f₃ = 5v / 4L

### Why Only Odd Harmonics?

The closed end forces a node; the open end forces an antinode. Only standing waves with an odd number of quarter-wavelengths between the ends satisfy both conditions.

### Closed vs Open (same length)

A closed pipe's fundamental is one octave **lower** than an open pipe of the same length — the same reason a stopped organ pipe sounds an octave below its open counterpart.

| n | fₙ (L=1 m) | Harmonic |
|---|-----------|----------|
| 1 | **85.7500** | 1st |
| 2 | **257.2500** | 3rd |
| 3 | **428.7500** | 5th |

### Your Task

Implement \`closedPipeMode(n, L)\` returning the nth resonant frequency (v = 343 m/s).

Run the code to hear the hollow, woody tone of odd harmonics only.`,

	starterCode: `function closedPipeMode(n, L) {
  // fn = (2n - 1) * 343 / (4 * L)
  return 0;
}

console.log(closedPipeMode(1, 1).toFixed(4));    // 85.7500
console.log(closedPipeMode(2, 1).toFixed(4));    // 257.2500
console.log(closedPipeMode(3, 1).toFixed(4));    // 428.7500
console.log(closedPipeMode(1, 0.5).toFixed(4)); // 171.5000

// Hear a chord of odd harmonics (closed pipe timbre)
const ac = new AudioContext();
[1, 2, 3, 4].forEach(n => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = closedPipeMode(n, 1);
  gain.gain.value = 0.15 / n;
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start();
  osc.stop(ac.currentTime + 2);
});
`,

	solution: `function closedPipeMode(n, L) {
  return (2 * n - 1) * 343 / (4 * L);
}

console.log(closedPipeMode(1, 1).toFixed(4));    // 85.7500
console.log(closedPipeMode(2, 1).toFixed(4));    // 257.2500
console.log(closedPipeMode(3, 1).toFixed(4));    // 428.7500
console.log(closedPipeMode(1, 0.5).toFixed(4)); // 171.5000

const ac = new AudioContext();
[1, 2, 3, 4].forEach(n => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = closedPipeMode(n, 1);
  gain.gain.value = 0.15 / n;
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start();
  osc.stop(ac.currentTime + 2);
});
`,

	tests: [
		{
			name: "n=1, L=1 m → 85.7500 Hz (fundamental)",
			code: `{{FUNC}}
console.log(closedPipeMode(1, 1).toFixed(4));`,
			expected: "85.7500\n",
		},
		{
			name: "n=2, L=1 m → 257.2500 Hz (3rd harmonic)",
			code: `{{FUNC}}
console.log(closedPipeMode(2, 1).toFixed(4));`,
			expected: "257.2500\n",
		},
		{
			name: "n=3, L=1 m → 428.7500 Hz (5th harmonic)",
			code: `{{FUNC}}
console.log(closedPipeMode(3, 1).toFixed(4));`,
			expected: "428.7500\n",
		},
		{
			name: "n=1, L=0.5 m → 171.5000 Hz",
			code: `{{FUNC}}
console.log(closedPipeMode(1, 0.5).toFixed(4));`,
			expected: "171.5000\n",
		},
	],
};
