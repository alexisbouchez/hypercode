import type { Lesson } from "../../types";

export const transposeLesson: Lesson = {
	id: "transpose",
	title: "Transposition",
	chapterId: "chords-and-harmony",
	content: `## Transposition

**Transposing** means shifting all notes up or down by the same number of semitones, preserving the shape of a melody or chord.

### Why Transpose?

- Change the **key** of a song to suit a singer's range
- Move a chord up or down the neck of a guitar
- Shift a melody to fit a different instrument

### The Formula

For each note in an array:

\`\`\`
transposed[i] = notes[i] + steps
\`\`\`

### Example: C Major → D Major (+2 semitones)

\`\`\`
C major:  [60, 62, 64, 65, 67, 69, 71, 72]
D major:  [62, 64, 66, 67, 69, 71, 73, 74]
\`\`\`

Negative steps transpose **down**.

### Your Task

Implement \`transpose(notes, steps)\` that shifts all notes by \`steps\` semitones.

Run your code to hear C major and F major (transposed up 5) back to back.`,

	starterCode: `function transpose(notes, steps) {
  // Add steps to every note
  return [];
}

const cMajor = [60, 64, 67];
console.log(transpose(cMajor, 2).join(" "));  // 62 66 69 (D major)
console.log(transpose(cMajor, -12).join(" ")); // 48 52 55 (C major, octave down)

// Play C major then F major (5 semitones up)
function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
function playChord(notes, startTime) {
  notes.forEach(note => {
    const osc = ac.createOscillator();
    const gain = ac.createGain();
    osc.frequency.value = noteToFreq(note);
    gain.gain.setValueAtTime(0.2, startTime);
    gain.gain.linearRampToValueAtTime(0, startTime + 1.2);
    osc.connect(gain);
    gain.connect(ac.destination);
    osc.start(startTime);
    osc.stop(startTime + 1.2);
  });
}
playChord(cMajor, ac.currentTime);
playChord(transpose(cMajor, 5), ac.currentTime + 1.4);
`,

	solution: `function transpose(notes, steps) {
  return notes.map(n => n + steps);
}

const cMajor = [60, 64, 67];
console.log(transpose(cMajor, 2).join(" "));  // 62 66 69
console.log(transpose(cMajor, -12).join(" ")); // 48 52 55

function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
function playChord(notes, startTime) {
  notes.forEach(note => {
    const osc = ac.createOscillator();
    const gain = ac.createGain();
    osc.frequency.value = noteToFreq(note);
    gain.gain.setValueAtTime(0.2, startTime);
    gain.gain.linearRampToValueAtTime(0, startTime + 1.2);
    osc.connect(gain);
    gain.connect(ac.destination);
    osc.start(startTime);
    osc.stop(startTime + 1.2);
  });
}
playChord(cMajor, ac.currentTime);
playChord(transpose(cMajor, 5), ac.currentTime + 1.4);
`,

	tests: [
		{
			name: "C major up 2 semitones: 62 66 69",
			code: `{{FUNC}}
console.log(transpose([60, 64, 67], 2).join(" "));`,
			expected: "62 66 69\n",
		},
		{
			name: "C major down one octave: 48 52 55",
			code: `{{FUNC}}
console.log(transpose([60, 64, 67], -12).join(" "));`,
			expected: "48 52 55\n",
		},
		{
			name: "Single note up octave: 81",
			code: `{{FUNC}}
console.log(transpose([69], 12).join(" "));`,
			expected: "81\n",
		},
		{
			name: "C D E up 5 semitones: 65 67 69",
			code: `{{FUNC}}
console.log(transpose([60, 62, 64], 5).join(" "));`,
			expected: "65 67 69\n",
		},
	],
};
