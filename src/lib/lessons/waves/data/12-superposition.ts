import type { Lesson } from "../../types";

export const superpositionLesson: Lesson = {
	id: "superposition",
	title: "Wave Superposition",
	chapterId: "standing-waves",
	content: `## Superposition of Two Waves

When two waves of equal frequency but different phases and amplitudes meet, their amplitudes add vectorially. The **resultant amplitude** is:

\`\`\`
A = √(A₁² + A₂² + 2·A₁·A₂·cos φ)
\`\`\`

- **A₁, A₂** — individual amplitudes
- **φ** — phase difference (radians)
- **A** — resultant amplitude

### Two Extremes

| φ | Result | Name |
|---|--------|------|
| 0 | A₁ + A₂ | **Constructive interference** |
| π | \|A₁ − A₂\| | **Destructive interference** |
| π/2 | √(A₁² + A₂²) | Quadrature |

### Examples

| A₁ | A₂ | φ | A |
|----|----|----|---|
| 1 | 1 | 0 | **2.0000** (double) |
| 1 | 1 | π | **0.0000** (cancel) |
| 1 | 1 | π/2 | **1.4142** (√2) |
| 3 | 4 | 0 | **7.0000** |

### Your Task

Implement \`superpose(A1, A2, phi)\` returning the resultant amplitude.

Run the code to hear constructive then destructive interference.`,

	starterCode: `function superpose(A1, A2, phi) {
  // A = sqrt(A1^2 + A2^2 + 2*A1*A2*cos(phi))
  return 0;
}

console.log(superpose(1, 1, 0).toFixed(4));              // 2.0000
console.log(superpose(1, 1, Math.PI).toFixed(4));        // 0.0000
console.log(superpose(1, 1, Math.PI / 2).toFixed(4));    // 1.4142
console.log(superpose(3, 4, 0).toFixed(4));              // 7.0000

// Hear constructive (loud) then destructive (quiet) — same two freqs
const ac = new AudioContext();
const makeOscs = (gainVal, startTime) => {
  const g = ac.createGain();
  g.gain.value = gainVal * 0.2;
  g.connect(ac.destination);
  [440, 440].forEach((f, i) => {
    const osc = ac.createOscillator();
    osc.frequency.value = f;
    osc.connect(g);
    osc.start(startTime);
    osc.stop(startTime + 1);
  });
};
makeOscs(superpose(1, 1, 0), ac.currentTime);
makeOscs(superpose(1, 1, Math.PI), ac.currentTime + 1.3);
`,

	solution: `function superpose(A1, A2, phi) {
  return Math.sqrt(A1 * A1 + A2 * A2 + 2 * A1 * A2 * Math.cos(phi));
}

console.log(superpose(1, 1, 0).toFixed(4));              // 2.0000
console.log(superpose(1, 1, Math.PI).toFixed(4));        // 0.0000
console.log(superpose(1, 1, Math.PI / 2).toFixed(4));    // 1.4142
console.log(superpose(3, 4, 0).toFixed(4));              // 7.0000

const ac = new AudioContext();
const makeOscs = (gainVal, startTime) => {
  const g = ac.createGain();
  g.gain.value = gainVal * 0.2;
  g.connect(ac.destination);
  [440, 440].forEach((f, i) => {
    const osc = ac.createOscillator();
    osc.frequency.value = f;
    osc.connect(g);
    osc.start(startTime);
    osc.stop(startTime + 1);
  });
};
makeOscs(superpose(1, 1, 0), ac.currentTime);
makeOscs(superpose(1, 1, Math.PI), ac.currentTime + 1.3);
`,

	tests: [
		{
			name: "φ=0 (constructive): A=2.0000",
			code: `{{FUNC}}
console.log(superpose(1, 1, 0).toFixed(4));`,
			expected: "2.0000\n",
		},
		{
			name: "φ=π (destructive): A=0.0000",
			code: `{{FUNC}}
console.log(superpose(1, 1, Math.PI).toFixed(4));`,
			expected: "0.0000\n",
		},
		{
			name: "φ=π/2 (quadrature): A=1.4142",
			code: `{{FUNC}}
console.log(superpose(1, 1, Math.PI / 2).toFixed(4));`,
			expected: "1.4142\n",
		},
		{
			name: "A₁=3, A₂=4, φ=0 → 7.0000",
			code: `{{FUNC}}
console.log(superpose(3, 4, 0).toFixed(4));`,
			expected: "7.0000\n",
		},
	],
};
