import type { Lesson } from "../../types";

export const patternOnsetsLesson: Lesson = {
	id: "pattern-onsets",
	title: "Drum Patterns",
	chapterId: "effects-and-timbre",
	content: `## Step Sequencer

A **step sequencer** triggers sounds on a rhythmic grid. Each step is either ON (1) or OFF (0). At a given BPM, you convert the step grid into onset times.

### Formula

Each step is a sixteenth note (1/4 of a beat):

\`\`\`
stepDuration = 60 / (bpm × 4)
onsets = steps where pattern[i] === 1, at time i × stepDuration
\`\`\`

### Classic Drum Patterns

\`\`\`
// 4-on-the-floor kick (16 steps):
kick:  [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0]

// Snare on beats 2 and 4:
snare: [0,0,0,0, 1,0,0,0, 0,0,0,0, 1,0,0,0]

// Closed hi-hat every eighth note:
hihat: [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0]
\`\`\`

### Your Task

Implement \`patternOnsets(pattern, bpm)\` that returns an array of onset times (in seconds) for each active step.

Run your code to hear a basic beat: kick on 1 and 3, hi-hat on every eighth.`,

	starterCode: `function patternOnsets(pattern, bpm) {
  const stepDur = 60 / (bpm * 4);
  // Return an array of times where pattern[i] === 1
  return [];
}

const kick  = [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0];
const hihat = [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0];
const bpm = 120;

console.log(patternOnsets(kick, bpm).map(t => t.toFixed(2)).join(" "));
// 0.00 0.50 1.00 1.50

// Play the beat using sine waves (kick=low, hihat=high)
const ac = new AudioContext();
function playHit(freq, onset, dur) {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = freq;
  gain.gain.setValueAtTime(0.4, ac.currentTime + onset);
  gain.gain.linearRampToValueAtTime(0, ac.currentTime + onset + dur);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + onset);
  osc.stop(ac.currentTime + onset + dur + 0.01);
}
patternOnsets(kick, bpm).forEach(t => playHit(80, t, 0.12));
patternOnsets(hihat, bpm).forEach(t => playHit(1200, t, 0.05));
`,

	solution: `function patternOnsets(pattern, bpm) {
  const stepDur = 60 / (bpm * 4);
  return pattern
    .map((hit, i) => ({ hit, time: i * stepDur }))
    .filter(({ hit }) => hit)
    .map(({ time }) => time);
}

const kick  = [1,0,0,0, 1,0,0,0, 1,0,0,0, 1,0,0,0];
const hihat = [1,0,1,0, 1,0,1,0, 1,0,1,0, 1,0,1,0];
const bpm = 120;

console.log(patternOnsets(kick, bpm).map(t => t.toFixed(2)).join(" "));

const ac = new AudioContext();
function playHit(freq, onset, dur) {
  const osc = ac.createOscillator();
  const gain = ac.createGain();
  osc.frequency.value = freq;
  gain.gain.setValueAtTime(0.4, ac.currentTime + onset);
  gain.gain.linearRampToValueAtTime(0, ac.currentTime + onset + dur);
  osc.connect(gain);
  gain.connect(ac.destination);
  osc.start(ac.currentTime + onset);
  osc.stop(ac.currentTime + onset + dur + 0.01);
}
patternOnsets(kick, bpm).forEach(t => playHit(80, t, 0.12));
patternOnsets(hihat, bpm).forEach(t => playHit(1200, t, 0.05));
`,

	tests: [
		{
			name: "4-on-floor kick at 120 BPM: 0.00 0.50 1.00 1.50",
			code: `{{FUNC}}
console.log(patternOnsets([1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0], 120).map(t => t.toFixed(2)).join(" "));`,
			expected: "0.00 0.50 1.00 1.50\n",
		},
		{
			name: "Two hits at 60 BPM: 0.00 0.50",
			code: `{{FUNC}}
console.log(patternOnsets([1,0,1,0], 60).map(t => t.toFixed(2)).join(" "));`,
			expected: "0.00 0.50\n",
		},
		{
			name: "Off-beats at 60 BPM: 0.25 0.75",
			code: `{{FUNC}}
console.log(patternOnsets([0,1,0,1], 60).map(t => t.toFixed(2)).join(" "));`,
			expected: "0.25 0.75\n",
		},
		{
			name: "Last step only at 120 BPM: 0.38",
			code: `{{FUNC}}
console.log(patternOnsets([0,0,0,1], 120).map(t => t.toFixed(2)).join(" "));`,
			expected: "0.38\n",
		},
	],
};
