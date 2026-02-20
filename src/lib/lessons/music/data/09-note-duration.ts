import type { Lesson } from "../../types";

export const noteDurationLesson: Lesson = {
	id: "note-duration",
	title: "Note Durations",
	chapterId: "synthesis-and-rhythm",
	content: `## Note Values

Western music notation divides beats into standard note values. At a given BPM, each note value has a precise duration in seconds.

### Note Values

| Note | Type | Beats | Formula |
|------|------|-------|---------|
| 𝅝 | Whole | 4 | 4 × beat |
| 𝅗𝅥 | Half | 2 | 2 × beat |
| ♩ | Quarter | 1 | 1 × beat |
| ♪ | Eighth | ½ | ½ × beat |
| ♬ | Sixteenth | ¼ | ¼ × beat |

### Formula

\`\`\`
noteDuration(bpm, noteType) = 60 / bpm × (4 / noteType)
\`\`\`

Where \`noteType\` is 1 (whole), 2 (half), 4 (quarter), 8 (eighth), or 16 (sixteenth).

### Example at 120 BPM

\`\`\`
Quarter (4):   60/120 × (4/4)  = 0.5000 s
Eighth  (8):   60/120 × (4/8)  = 0.2500 s
Sixteenth (16): 60/120 × (4/16) = 0.1250 s
\`\`\`

### Your Task

Implement \`noteDuration(bpm, noteType)\` returning the duration in seconds.

Run your code to hear quarter notes followed by eighth notes.`,

	starterCode: `function noteDuration(bpm, noteType) {
  // duration = 60 / bpm × (4 / noteType)
  return 0;
}

console.log(noteDuration(120, 4).toFixed(4));  // 0.5000 (quarter)
console.log(noteDuration(120, 8).toFixed(4));  // 0.2500 (eighth)
console.log(noteDuration(60, 1).toFixed(4));   // 4.0000 (whole)
console.log(noteDuration(120, 16).toFixed(4)); // 0.1250 (sixteenth)

// Play: 4 quarter notes then 8 eighth notes
function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
const bpm = 120;
const q = noteDuration(bpm, 4);
const e = noteDuration(bpm, 8);
const melody = [60, 62, 64, 65];
let t = ac.currentTime;
// Quarter notes
melody.forEach(note => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, t);
  gain.gain.linearRampToValueAtTime(0, t + q * 0.9);
  osc.connect(gain); gain.connect(ac.destination);
  osc.start(t); osc.stop(t + q);
  t += q;
});
// Eighth notes (same melody, twice as fast)
melody.concat(melody).forEach(note => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, t);
  gain.gain.linearRampToValueAtTime(0, t + e * 0.9);
  osc.connect(gain); gain.connect(ac.destination);
  osc.start(t); osc.stop(t + e);
  t += e;
});
`,

	solution: `function noteDuration(bpm, noteType) {
  return 60 / bpm * (4 / noteType);
}

console.log(noteDuration(120, 4).toFixed(4));  // 0.5000
console.log(noteDuration(120, 8).toFixed(4));  // 0.2500
console.log(noteDuration(60, 1).toFixed(4));   // 4.0000
console.log(noteDuration(120, 16).toFixed(4)); // 0.1250

function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
const bpm = 120;
const q = noteDuration(bpm, 4);
const e = noteDuration(bpm, 8);
const melody = [60, 62, 64, 65];
let t = ac.currentTime;
melody.forEach(note => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, t);
  gain.gain.linearRampToValueAtTime(0, t + q * 0.9);
  osc.connect(gain); gain.connect(ac.destination);
  osc.start(t); osc.stop(t + q);
  t += q;
});
melody.concat(melody).forEach(note => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, t);
  gain.gain.linearRampToValueAtTime(0, t + e * 0.9);
  osc.connect(gain); gain.connect(ac.destination);
  osc.start(t); osc.stop(t + e);
  t += e;
});
`,

	tests: [
		{
			name: "Quarter note at 120 BPM = 0.5000 s",
			code: `{{FUNC}}
console.log(noteDuration(120, 4).toFixed(4));`,
			expected: "0.5000\n",
		},
		{
			name: "Eighth note at 120 BPM = 0.2500 s",
			code: `{{FUNC}}
console.log(noteDuration(120, 8).toFixed(4));`,
			expected: "0.2500\n",
		},
		{
			name: "Whole note at 60 BPM = 4.0000 s",
			code: `{{FUNC}}
console.log(noteDuration(60, 1).toFixed(4));`,
			expected: "4.0000\n",
		},
		{
			name: "Sixteenth note at 120 BPM = 0.1250 s",
			code: `{{FUNC}}
console.log(noteDuration(120, 16).toFixed(4));`,
			expected: "0.1250\n",
		},
	],
};
