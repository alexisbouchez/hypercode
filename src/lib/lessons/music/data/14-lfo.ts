import type { Lesson } from "../../types";

export const lfoLesson: Lesson = {
	id: "lfo",
	title: "Low-Frequency Oscillator",
	chapterId: "effects-and-timbre",
	content: `## LFO — Low-Frequency Oscillator

A **Low-Frequency Oscillator (LFO)** is a sine wave oscillating too slowly to hear as pitch (typically 0.1–20 Hz). Instead, it **modulates** other parameters over time to create effects.

### LFO Formula

\`\`\`
lfoValue(t, rate, depth) = depth × sin(2π × rate × t)
\`\`\`

- \`t\` — time in seconds
- \`rate\` — oscillation frequency in Hz
- \`depth\` — amplitude (range: −depth to +depth)

### Common LFO Uses

| Modulation target | Effect |
|-------------------|--------|
| Gain | Tremolo (volume wobble) |
| Pitch | Vibrato (pitch wobble) |
| Filter cutoff | Wah / auto-wah |
| Pan | Auto-pan |

### Example: Tremolo at 4 Hz, depth 0.4

The gain oscillates between 0.1 and 0.9 four times per second.

### Your Task

Implement \`lfoValue(t, rate, depth)\` returning the oscillator value.

Run your code to hear tremolo (LFO applied to gain).`,

	starterCode: `function lfoValue(t, rate, depth) {
  // depth × sin(2π × rate × t)
  return 0;
}

console.log(lfoValue(0, 4, 1).toFixed(4));       // 0.0000
console.log(lfoValue(0.25, 1, 1).toFixed(4));    // 1.0000 (quarter period of 1Hz)
console.log(lfoValue(0.0625, 4, 0.5).toFixed(4)); // 0.5000 (quarter period of 4Hz)

// Tremolo: LFO at 4 Hz modulating gain
const ac = new AudioContext();
const osc = ac.createOscillator();
const gainNode = ac.createGain();
osc.frequency.value = 440;
osc.connect(gainNode);
gainNode.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 3);

// Manually schedule gain automation using lfoValue
const steps = 300;
const dur = 3;
for (let i = 0; i <= steps; i++) {
  const t = (i / steps) * dur;
  const g = 0.5 + lfoValue(t, 4, 0.4); // center 0.5, wobble ±0.4
  gainNode.gain.setValueAtTime(Math.max(0, Math.min(1, g)), ac.currentTime + t);
}
`,

	solution: `function lfoValue(t, rate, depth) {
  return depth * Math.sin(2 * Math.PI * rate * t);
}

console.log(lfoValue(0, 4, 1).toFixed(4));        // 0.0000
console.log(lfoValue(0.25, 1, 1).toFixed(4));     // 1.0000
console.log(lfoValue(0.0625, 4, 0.5).toFixed(4)); // 0.5000

const ac = new AudioContext();
const osc = ac.createOscillator();
const gainNode = ac.createGain();
osc.frequency.value = 440;
osc.connect(gainNode);
gainNode.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 3);

const steps = 300;
const dur = 3;
for (let i = 0; i <= steps; i++) {
  const t = (i / steps) * dur;
  const g = 0.5 + lfoValue(t, 4, 0.4);
  gainNode.gain.setValueAtTime(Math.max(0, Math.min(1, g)), ac.currentTime + t);
}
`,

	tests: [
		{
			name: "lfoValue at t=0 = 0.0000",
			code: `{{FUNC}}
console.log(lfoValue(0, 4, 1).toFixed(4));`,
			expected: "0.0000\n",
		},
		{
			name: "Quarter period of 1Hz LFO = 1.0000",
			code: `{{FUNC}}
console.log(lfoValue(0.25, 1, 1).toFixed(4));`,
			expected: "1.0000\n",
		},
		{
			name: "Quarter period of 4Hz LFO, depth 0.5 = 0.5000",
			code: `{{FUNC}}
console.log(lfoValue(0.0625, 4, 0.5).toFixed(4));`,
			expected: "0.5000\n",
		},
		{
			name: "Half period of 1Hz LFO ≈ 0.0000",
			code: `{{FUNC}}
console.log(lfoValue(0.5, 1, 1).toFixed(4));`,
			expected: "0.0000\n",
		},
	],
};
