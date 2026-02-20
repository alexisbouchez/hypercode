import type { Lesson } from "../../types";

export const onsetTimeLesson: Lesson = {
	id: "onset-time",
	title: "Scheduling Notes",
	chapterId: "synthesis-and-rhythm",
	content: `## Note Onset Times

A **sequencer** schedules notes at precise times. Each note's start time (onset) is calculated from its position in the sequence.

### The Formula

\`\`\`
onsetTime(step, bpm, subdivision) = step × (60 / bpm / subdivision)
\`\`\`

- \`step\` — position in the sequence (0-indexed)
- \`bpm\` — tempo
- \`subdivision\` — how many notes per beat (1=quarter, 2=eighth, 4=sixteenth)

### Example at 120 BPM, Eighth Notes (subdivision=2)

\`\`\`
step 0 → 0 × 0.25 = 0.000 s
step 1 → 1 × 0.25 = 0.250 s
step 2 → 2 × 0.25 = 0.500 s
step 4 → 4 × 0.25 = 1.000 s
\`\`\`

### Using Onset Times

\`\`\`js
const melody = [60, 62, 64, 65, 67];
melody.forEach((note, step) => {
  const t = ac.currentTime + onsetTime(step, 120, 2);
  scheduleNote(note, t);
});
\`\`\`

### Your Task

Implement \`onsetTime(step, bpm, subdivision)\` returning seconds.

Run your code to hear a melody with precisely timed notes.`,

	starterCode: `function onsetTime(step, bpm, subdivision) {
  // onset = step × (60 / bpm / subdivision)
  return 0;
}

console.log(onsetTime(0, 120, 2).toFixed(3)); // 0.000
console.log(onsetTime(1, 120, 2).toFixed(3)); // 0.250
console.log(onsetTime(4, 120, 2).toFixed(3)); // 1.000
console.log(onsetTime(1, 60, 1).toFixed(3));  // 1.000

// Play a melody using onsetTime
function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
const melody = [60, 62, 64, 65, 67, 69, 71, 72];
melody.forEach((note, step) => {
  const t = ac.currentTime + onsetTime(step, 120, 2);
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, t);
  gain.gain.linearRampToValueAtTime(0, t + 0.22);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(t);
  osc.stop(t + 0.25);
});
`,

	solution: `function onsetTime(step, bpm, subdivision) {
  return step * (60 / bpm / subdivision);
}

console.log(onsetTime(0, 120, 2).toFixed(3)); // 0.000
console.log(onsetTime(1, 120, 2).toFixed(3)); // 0.250
console.log(onsetTime(4, 120, 2).toFixed(3)); // 1.000
console.log(onsetTime(1, 60, 1).toFixed(3));  // 1.000

function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
const melody = [60, 62, 64, 65, 67, 69, 71, 72];
melody.forEach((note, step) => {
  const t = ac.currentTime + onsetTime(step, 120, 2);
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, t);
  gain.gain.linearRampToValueAtTime(0, t + 0.22);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(t);
  osc.stop(t + 0.25);
});
`,

	tests: [
		{
			name: "Step 0 onset = 0.000",
			code: `{{FUNC}}
console.log(onsetTime(0, 120, 2).toFixed(3));`,
			expected: "0.000\n",
		},
		{
			name: "Step 1, 120BPM, eighth = 0.250",
			code: `{{FUNC}}
console.log(onsetTime(1, 120, 2).toFixed(3));`,
			expected: "0.250\n",
		},
		{
			name: "Step 4, 120BPM, eighth = 1.000",
			code: `{{FUNC}}
console.log(onsetTime(4, 120, 2).toFixed(3));`,
			expected: "1.000\n",
		},
		{
			name: "Step 1, 60BPM, quarter = 1.000",
			code: `{{FUNC}}
console.log(onsetTime(1, 60, 1).toFixed(3));`,
			expected: "1.000\n",
		},
	],
};
