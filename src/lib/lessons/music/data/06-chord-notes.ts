import type { Lesson } from "../../types";

export const chordNotesLesson: Lesson = {
	id: "chord-notes",
	title: "Chord Notes",
	chapterId: "chords-and-harmony",
	content: `## Chords

A **chord** is multiple notes played simultaneously. Chords are defined by semitone intervals from a root note.

### Common Triads

| Type | Intervals | Character |
|------|-----------|-----------|
| Major | [0, 4, 7] | bright, happy |
| Minor | [0, 3, 7] | dark, melancholic |

### Seventh Chords

| Type | Intervals | Character |
|------|-----------|-----------|
| Dominant 7th | [0, 4, 7, 10] | tense, jazzy |
| Major 7th | [0, 4, 7, 11] | smooth, dreamy |

### Example: C Major (root = MIDI 60)

\`\`\`
root + 0 = 60 → C4
root + 4 = 64 → E4
root + 7 = 67 → G4
\`\`\`

### Your Task

Implement \`chordNotes(root, type)\` that returns an array of MIDI notes.

Run your code to hear a C major chord ring out.`,

	starterCode: `function chordNotes(root, type) {
  const INTERVALS = {
    major: [0, 4, 7],
    minor: [0, 3, 7],
    dom7:  [0, 4, 7, 10],
    maj7:  [0, 4, 7, 11],
  };
  // Return the intervals shifted by root
  return [];
}

console.log(chordNotes(60, "major").join(" ")); // 60 64 67
console.log(chordNotes(60, "minor").join(" ")); // 60 63 67

// Play C major chord
function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
chordNotes(60, "major").forEach(note => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.2, ac.currentTime);
  gain.gain.linearRampToValueAtTime(0, ac.currentTime + 1.5);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start();
  osc.stop(ac.currentTime + 1.5);
});
`,

	solution: `function chordNotes(root, type) {
  const INTERVALS = {
    major: [0, 4, 7],
    minor: [0, 3, 7],
    dom7:  [0, 4, 7, 10],
    maj7:  [0, 4, 7, 11],
  };
  return INTERVALS[type].map(i => root + i);
}

console.log(chordNotes(60, "major").join(" ")); // 60 64 67
console.log(chordNotes(60, "minor").join(" ")); // 60 63 67

function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
chordNotes(60, "major").forEach(note => {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.2, ac.currentTime);
  gain.gain.linearRampToValueAtTime(0, ac.currentTime + 1.5);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start();
  osc.stop(ac.currentTime + 1.5);
});
`,

	tests: [
		{
			name: "C major: 60 64 67",
			code: `{{FUNC}}
console.log(chordNotes(60, "major").join(" "));`,
			expected: "60 64 67\n",
		},
		{
			name: "C minor: 60 63 67",
			code: `{{FUNC}}
console.log(chordNotes(60, "minor").join(" "));`,
			expected: "60 63 67\n",
		},
		{
			name: "G dominant 7th: 67 71 74 77",
			code: `{{FUNC}}
console.log(chordNotes(67, "dom7").join(" "));`,
			expected: "67 71 74 77\n",
		},
		{
			name: "C major 7th: 60 64 67 71",
			code: `{{FUNC}}
console.log(chordNotes(60, "maj7").join(" "));`,
			expected: "60 64 67 71\n",
		},
	],
};
