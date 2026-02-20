import type { Lesson } from "../../types";

export const freqToNote: Lesson = {
	id: "freq-to-note",
	title: "Frequency to Note",
	chapterId: "notes-and-frequencies",
	content: `## The Inverse Formula

Given a frequency f, which MIDI note is it closest to?

Inverting the formula f = 440 × 2^((n−69)/12):

\`\`\`
n = 69 + 12 × log₂(f / 440)
\`\`\`

Round to the nearest integer to get the closest MIDI note.

### Logarithm Review

log₂(x) = Math.log2(x) in JavaScript.

### Cents

The fractional part tells you **how far off** the frequency is from the nearest note, measured in **cents** (1/100th of a semitone):

\`\`\`
cents = (exact_n − nearest_n) × 100
\`\`\`

- ±50 cents = halfway between notes (maximally out of tune)
- 0 cents = perfectly in tune

### Examples

| Frequency | Exact n | Nearest MIDI | Note |
|-----------|---------|--------------|------|
| 440 Hz | 69.0 | **69** | A4 |
| 432 Hz | 68.68 | **69** | A4 (−32 cents flat) |
| 450 Hz | 69.39 | **69** | A4 (+39 cents sharp) |
| 261.63 Hz | 60.0 | **60** | C4 |

### Your Task

Implement \`freqToNote(f)\` that returns the nearest MIDI note number for frequency \`f\`.

Run your code to hear the note detected from 440 Hz.`,

	starterCode: `function freqToNote(f) {
  // n = 69 + 12 * log2(f / 440), then round
  return 0;
}

console.log(freqToNote(440));  // 69
console.log(freqToNote(880));  // 81
console.log(freqToNote(261.63)); // 60

// Hear the detected note
function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const detected = freqToNote(440);
const ac = new AudioContext();
const osc = ac.createOscillator();
const gain = ac.createGain();
osc.frequency.value = noteToFreq(detected);
gain.gain.value = 0.3;
osc.connect(gain);
gain.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 1);
`,

	solution: `function freqToNote(f) {
  return Math.round(69 + 12 * Math.log2(f / 440));
}

console.log(freqToNote(440));  // 69
console.log(freqToNote(880));  // 81
console.log(freqToNote(261.63)); // 60

function noteToFreq(n) { return 440 * Math.pow(2, (n - 69) / 12); }
const detected = freqToNote(440);
const ac = new AudioContext();
const osc = ac.createOscillator();
const gain = ac.createGain();
osc.frequency.value = noteToFreq(detected);
gain.gain.value = 0.3;
osc.connect(gain);
gain.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 1);
`,

	tests: [
		{
			name: "440 Hz → MIDI 69 (A4)",
			code: `{{FUNC}}
console.log(freqToNote(440));`,
			expected: "69\n",
		},
		{
			name: "880 Hz → MIDI 81 (A5)",
			code: `{{FUNC}}
console.log(freqToNote(880));`,
			expected: "81\n",
		},
		{
			name: "261.63 Hz → MIDI 60 (C4)",
			code: `{{FUNC}}
console.log(freqToNote(261.63));`,
			expected: "60\n",
		},
		{
			name: "220 Hz → MIDI 57 (A3)",
			code: `{{FUNC}}
console.log(freqToNote(220));`,
			expected: "57\n",
		},
	],
};
