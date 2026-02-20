import type { Lesson } from "../../types";

export const scaleLesson: Lesson = {
	id: "scale",
	title: "Major & Minor Scales",
	chapterId: "notes-and-frequencies",
	content: `## Scales

A **scale** is an ordered set of notes within an octave, defined by intervals between consecutive notes.

### Major Scale (Ionian Mode)

The major scale has a bright, happy character. Intervals in semitones:

\`\`\`
W W H W W W H
2 2 1 2 2 2 1
\`\`\`

(W = whole step = 2 semitones, H = half step = 1 semitone)

Starting from MIDI root: add **[0, 2, 4, 5, 7, 9, 11, 12]** semitones.

### Natural Minor Scale (Aeolian Mode)

The natural minor scale sounds darker and more melancholic:

\`\`\`
W H W W H W W
2 1 2 2 1 2 2
\`\`\`

Intervals from root: **[0, 2, 3, 5, 7, 8, 10, 12]**

### Example: C Major

\`\`\`
MIDI: 60, 62, 64, 65, 67, 69, 71, 72
Note:  C4  D4  E4  F4  G4  A4  B4  C5
\`\`\`

### Example: A Minor

\`\`\`
MIDI: 69, 71, 72, 74, 76, 77, 79, 81
Note:  A4  B4  C5  D5  E5  F5  G5  A5
\`\`\`

### Your Task

Implement \`scaleNotes(root, isMinor)\` that returns an array of 8 MIDI note numbers.

Run your code to **hear a major scale** ascend.`,

	starterCode: `function scaleNotes(root, isMinor) {
  const MAJOR = [0, 2, 4, 5, 7, 9, 11, 12];
  const MINOR = [0, 2, 3, 5, 7, 8, 10, 12];
  const intervals = isMinor ? MINOR : MAJOR;
  return intervals.map(i => root + i);
}

// C major scale starting from C4 (MIDI 60)
const notes = scaleNotes(60, false);
console.log(notes.join(" ")); // 60 62 64 65 67 69 71 72

// Play it
function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
notes.forEach((note, i) => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, ac.currentTime + i * 0.25);
  gain.gain.linearRampToValueAtTime(0, ac.currentTime + i * 0.25 + 0.22);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * 0.25);
  osc.stop(ac.currentTime + i * 0.25 + 0.25);
});
`,

	solution: `function scaleNotes(root, isMinor) {
  const MAJOR = [0, 2, 4, 5, 7, 9, 11, 12];
  const MINOR = [0, 2, 3, 5, 7, 8, 10, 12];
  const intervals = isMinor ? MINOR : MAJOR;
  return intervals.map(i => root + i);
}

const notes = scaleNotes(60, false);
console.log(notes.join(" "));

function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
notes.forEach((note, i) => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, ac.currentTime + i * 0.25);
  gain.gain.linearRampToValueAtTime(0, ac.currentTime + i * 0.25 + 0.22);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * 0.25);
  osc.stop(ac.currentTime + i * 0.25 + 0.25);
});
`,

	tests: [
		{
			name: "C major scale: 60 62 64 65 67 69 71 72",
			code: `{{FUNC}}
console.log(scaleNotes(60, false).join(" "));`,
			expected: "60 62 64 65 67 69 71 72\n",
		},
		{
			name: "A minor scale: 69 71 72 74 76 77 79 81",
			code: `{{FUNC}}
console.log(scaleNotes(69, true).join(" "));`,
			expected: "69 71 72 74 76 77 79 81\n",
		},
		{
			name: "G major scale: 67 69 71 72 74 76 78 79",
			code: `{{FUNC}}
console.log(scaleNotes(67, false).join(" "));`,
			expected: "67 69 71 72 74 76 78 79\n",
		},
		{
			name: "D minor scale: 62 64 65 67 69 70 72 74",
			code: `{{FUNC}}
console.log(scaleNotes(62, true).join(" "));`,
			expected: "62 64 65 67 69 70 72 74\n",
		},
	],
};
