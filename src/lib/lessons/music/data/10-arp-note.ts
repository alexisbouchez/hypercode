import type { Lesson } from "../../types";

export const arpNoteLesson: Lesson = {
	id: "arp-note",
	title: "Arpeggiation",
	chapterId: "synthesis-and-rhythm",
	content: `## Arpeggios

An **arpeggio** plays the notes of a chord one at a time rather than all together. This creates a flowing, harp-like texture.

### Cycling Through Chord Notes

Given a chord as a MIDI array, an arpeggiator cycles through the notes:

\`\`\`
arpNote(chord, step) = chord[step % chord.length]
\`\`\`

The **modulo** operator (\`%\`) wraps the step index back to the start when it reaches the end of the chord.

### Example: C Major Arpeggio

\`\`\`
chord = [60, 64, 67]   // C4, E4, G4

step 0 → chord[0 % 3] = chord[0] = 60  (C4)
step 1 → chord[1 % 3] = chord[1] = 64  (E4)
step 2 → chord[2 % 3] = chord[2] = 67  (G4)
step 3 → chord[3 % 3] = chord[0] = 60  (C4, wraps!)
step 4 → chord[4 % 3] = chord[1] = 64  (E4)
\`\`\`

### Your Task

Implement \`arpNote(chord, step)\` returning the MIDI note at the given step.

Run your code to hear a C major arpeggio ascend and descend.`,

	starterCode: `function arpNote(chord, step) {
  // Return chord[step % chord.length]
  return 0;
}

const cMajor = [60, 64, 67];
console.log(arpNote(cMajor, 0)); // 60
console.log(arpNote(cMajor, 1)); // 64
console.log(arpNote(cMajor, 3)); // 60 (wraps)

// Play an arpeggio: up and down C major
function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
const chord = [60, 64, 67, 72]; // C4 E4 G4 C5
const pattern = [0,1,2,3, 3,2,1,0, 0,1,2,3]; // up-down-up
pattern.forEach((step, i) => {
  const note = arpNote(chord, step);
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, ac.currentTime + i * 0.15);
  gain.gain.linearRampToValueAtTime(0, ac.currentTime + i * 0.15 + 0.13);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * 0.15);
  osc.stop(ac.currentTime + i * 0.15 + 0.15);
});
`,

	solution: `function arpNote(chord, step) {
  return chord[step % chord.length];
}

const cMajor = [60, 64, 67];
console.log(arpNote(cMajor, 0)); // 60
console.log(arpNote(cMajor, 1)); // 64
console.log(arpNote(cMajor, 3)); // 60

function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const ac = new AudioContext();
const chord = [60, 64, 67, 72];
const pattern = [0,1,2,3, 3,2,1,0, 0,1,2,3];
pattern.forEach((step, i) => {
  const note = arpNote(chord, step);
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = noteToFreq(note);
  gain.gain.setValueAtTime(0.3, ac.currentTime + i * 0.15);
  gain.gain.linearRampToValueAtTime(0, ac.currentTime + i * 0.15 + 0.13);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + i * 0.15);
  osc.stop(ac.currentTime + i * 0.15 + 0.15);
});
`,

	tests: [
		{
			name: "Step 0 of [60,64,67] = 60",
			code: `{{FUNC}}
console.log(arpNote([60, 64, 67], 0));`,
			expected: "60\n",
		},
		{
			name: "Step 1 of [60,64,67] = 64",
			code: `{{FUNC}}
console.log(arpNote([60, 64, 67], 1));`,
			expected: "64\n",
		},
		{
			name: "Step 3 wraps to index 0: 60",
			code: `{{FUNC}}
console.log(arpNote([60, 64, 67], 3));`,
			expected: "60\n",
		},
		{
			name: "Step 5 of [60,64,67,72]: index 5%4=1 → 64",
			code: `{{FUNC}}
console.log(arpNote([60, 64, 67, 72], 5));`,
			expected: "64\n",
		},
	],
};
