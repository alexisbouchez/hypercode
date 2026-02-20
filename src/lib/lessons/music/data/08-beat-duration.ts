import type { Lesson } from "../../types";

export const beatDurationLesson: Lesson = {
	id: "beat-duration",
	title: "BPM and Beat Duration",
	chapterId: "synthesis-and-rhythm",
	content: `## Tempo and Timing

**BPM** (Beats Per Minute) describes the tempo of music. The beat is the rhythmic pulse you tap your foot to.

### Beat Duration

The duration of one beat in seconds:

\`\`\`
beatDuration(bpm) = 60 / bpm
\`\`\`

### Common Tempos

| BPM | Genre | Beat (seconds) |
|-----|-------|----------------|
| 60 | Slow ballad | 1.0000 |
| 90 | Moderate | 0.6667 |
| 120 | Dance / Pop | 0.5000 |
| 140 | Fast dance | 0.4286 |
| 180 | Very fast | 0.3333 |

### Why This Matters

In the Web Audio API, you schedule sounds with \`AudioContext.currentTime\` (in seconds). Knowing beat duration lets you place notes on the beat grid:

\`\`\`js
// Note at beat 3 (0-indexed):
const onset = ac.currentTime + 3 * beatDuration(120);
\`\`\`

### Your Task

Implement \`beatDuration(bpm)\` returning seconds per beat.

Run your code to hear four quarter notes at 120 BPM.`,

	starterCode: `function beatDuration(bpm) {
  // seconds per beat = 60 / bpm
  return 0;
}

console.log(beatDuration(120).toFixed(4)); // 0.5000
console.log(beatDuration(60).toFixed(4));  // 1.0000
console.log(beatDuration(90).toFixed(4));  // 0.6667

// Play 4 quarter notes at 120 BPM
function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
const bpm = 120;
const bd = beatDuration(bpm);
[60, 64, 67, 72].forEach((note, i) => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, ac.currentTime + i * bd);
  gain.gain.linearRampToValueAtTime(0, ac.currentTime + i * bd + bd * 0.8);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * bd);
  osc.stop(ac.currentTime + i * bd + bd);
});
`,

	solution: `function beatDuration(bpm) {
  return 60 / bpm;
}

console.log(beatDuration(120).toFixed(4)); // 0.5000
console.log(beatDuration(60).toFixed(4));  // 1.0000
console.log(beatDuration(90).toFixed(4));  // 0.6667

function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
const bpm = 120;
const bd = beatDuration(bpm);
[60, 64, 67, 72].forEach((note, i) => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, ac.currentTime + i * bd);
  gain.gain.linearRampToValueAtTime(0, ac.currentTime + i * bd + bd * 0.8);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * bd);
  osc.stop(ac.currentTime + i * bd + bd);
});
`,

	tests: [
		{
			name: "120 BPM = 0.5000 s/beat",
			code: `{{FUNC}}
console.log(beatDuration(120).toFixed(4));`,
			expected: "0.5000\n",
		},
		{
			name: "60 BPM = 1.0000 s/beat",
			code: `{{FUNC}}
console.log(beatDuration(60).toFixed(4));`,
			expected: "1.0000\n",
		},
		{
			name: "90 BPM = 0.6667 s/beat",
			code: `{{FUNC}}
console.log(beatDuration(90).toFixed(4));`,
			expected: "0.6667\n",
		},
		{
			name: "180 BPM = 0.3333 s/beat",
			code: `{{FUNC}}
console.log(beatDuration(180).toFixed(4));`,
			expected: "0.3333\n",
		},
	],
};
