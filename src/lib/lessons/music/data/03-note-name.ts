import type { Lesson } from "../../types";

export const noteName: Lesson = {
	id: "note-name",
	title: "Note Names",
	chapterId: "notes-and-frequencies",
	content: `## MIDI to Note Name

MIDI note 60 is "C4". But how do we compute that from the number?

### The 12 Pitch Classes

Within every octave, there are 12 pitch classes:

\`\`\`
Index: 0  1   2  3   4  5  6   7  8   9  10  11
Note:  C  C#  D  D#  E  F  F#  G  G#  A  A#  B
\`\`\`

### Computing Name and Octave

\`\`\`
pitchClass = n % 12         → index into the names array
octave     = Math.floor(n / 12) − 1   → C4 is MIDI 60
\`\`\`

Wait — why subtract 1? Because MIDI octave numbering starts at C−1 (MIDI 0), so C4 is in the 5th octave from zero (index 4+1 = 5, then 60/12 = 5 − 1 = 4).

### Examples

| MIDI | Pitch class | Octave | Name |
|------|-------------|--------|------|
| 60 | 0 (C) | 4 | **C4** |
| 69 | 9 (A) | 4 | **A4** |
| 72 | 0 (C) | 5 | **C5** |
| 71 | 11 (B) | 4 | **B4** |

### Your Task

Implement \`noteName(n)\` that returns the note name as a string like \`"C4"\`, \`"A#3"\`, etc.

Run your code to see note names printed as the scale plays.`,

	starterCode: `function noteName(n) {
  const NOTE_NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
  const pitch = n % 12;
  const octave = Math.floor(n / 12) - 1;
  return NOTE_NAMES[pitch] + octave;
}

console.log(noteName(60)); // C4
console.log(noteName(69)); // A4
console.log(noteName(72)); // C5

// Play C major scale with note names
function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const scale = [60, 62, 64, 65, 67, 69, 71, 72];
const ac = new AudioContext();
scale.forEach((note, i) => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, ac.currentTime + i * 0.25);
  gain.gain.setValueAtTime(0, ac.currentTime + i * 0.25 + 0.2);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * 0.25);
  osc.stop(ac.currentTime + i * 0.25 + 0.25);
  console.log(noteName(note));
});
`,

	solution: `function noteName(n) {
  const NOTE_NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
  const pitch = n % 12;
  const octave = Math.floor(n / 12) - 1;
  return NOTE_NAMES[pitch] + octave;
}

console.log(noteName(60)); // C4
console.log(noteName(69)); // A4
console.log(noteName(72)); // C5

function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const scale = [60, 62, 64, 65, 67, 69, 71, 72];
const ac = new AudioContext();
scale.forEach((note, i) => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, ac.currentTime + i * 0.25);
  gain.gain.setValueAtTime(0, ac.currentTime + i * 0.25 + 0.2);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * 0.25);
  osc.stop(ac.currentTime + i * 0.25 + 0.25);
  console.log(noteName(note));
});
`,

	tests: [
		{
			name: "MIDI 60 → C4",
			code: `{{FUNC}}
console.log(noteName(60));`,
			expected: "C4\n",
		},
		{
			name: "MIDI 69 → A4",
			code: `{{FUNC}}
console.log(noteName(69));`,
			expected: "A4\n",
		},
		{
			name: "MIDI 72 → C5",
			code: `{{FUNC}}
console.log(noteName(72));`,
			expected: "C5\n",
		},
		{
			name: "MIDI 70 → A#4",
			code: `{{FUNC}}
console.log(noteName(70));`,
			expected: "A#4\n",
		},
	],
};
