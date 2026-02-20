import type { Lesson } from "../../types";

export const noteToFreq: Lesson = {
	id: "note-to-freq",
	title: "Note to Frequency",
	chapterId: "notes-and-frequencies",
	content: `## Equal Temperament

Modern Western music divides the octave into **12 equal semitones**. Each semitone is a frequency ratio of ²¹²√2 ≈ 1.0595.

This system is called **twelve-tone equal temperament** (12-TET). It lets instruments in different keys play together in tune.

### MIDI Note Numbers

Every note has a **MIDI number** — an integer from 0 to 127:

| Note | MIDI | Freq |
|------|------|------|
| A4 (concert A) | 69 | 440 Hz |
| Middle C (C4) | 60 | 261.63 Hz |
| C5 | 72 | 523.25 Hz |
| A5 | 81 | 880 Hz |

### The Formula

\`\`\`
f(n) = 440 × 2^((n − 69) / 12)
\`\`\`

The anchor is A4 = 440 Hz (MIDI 69). Every 12 semitones is one octave (frequency doubles). Every semitone multiplies by 2^(1/12).

### Your Task

Implement \`noteToFreq(n)\` that converts a MIDI note number to its frequency in Hz.

Run your code to **hear A4 (440 Hz)** through your speakers.`,

	starterCode: `function noteToFreq(n) {
  // f = 440 * 2^((n - 69) / 12)
  return 0;
}

console.log(noteToFreq(69).toFixed(4)); // 440.0000
console.log(noteToFreq(60).toFixed(2)); // 261.63

// Hear it — play A4 for 1 second
const ac = new AudioContext();
const osc = ac.createOscillator();
const gain = ac.createGain();
osc.frequency.value = noteToFreq(69);
gain.gain.value = 0.3;
osc.connect(gain);
gain.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 1);
`,

	solution: `function noteToFreq(n) {
  return 440 * Math.pow(2, (n - 69) / 12);
}

console.log(noteToFreq(69).toFixed(4)); // 440.0000
console.log(noteToFreq(60).toFixed(2)); // 261.63

const ac = new AudioContext();
const osc = ac.createOscillator();
const gain = ac.createGain();
osc.frequency.value = noteToFreq(69);
gain.gain.value = 0.3;
osc.connect(gain);
gain.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 1);
`,

	tests: [
		{
			name: "A4 (MIDI 69) = 440.0000 Hz",
			code: `{{FUNC}}
console.log(noteToFreq(69).toFixed(4));`,
			expected: "440.0000\n",
		},
		{
			name: "Middle C (MIDI 60) ≈ 261.63 Hz",
			code: `{{FUNC}}
console.log(noteToFreq(60).toFixed(2));`,
			expected: "261.63\n",
		},
		{
			name: "One octave up (MIDI 81) = 880.0000 Hz",
			code: `{{FUNC}}
console.log(noteToFreq(81).toFixed(4));`,
			expected: "880.0000\n",
		},
		{
			name: "One octave down (MIDI 57) = 220.0000 Hz",
			code: `{{FUNC}}
console.log(noteToFreq(57).toFixed(4));`,
			expected: "220.0000\n",
		},
	],
};
