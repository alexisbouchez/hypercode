import type { Lesson } from "../../types";

export const dopplerEffectLesson: Lesson = {
	id: "doppler-effect",
	title: "Doppler Effect",
	chapterId: "intensity-and-perception",
	content: `## The Doppler Effect

When a sound source moves toward you, successive wavefronts are compressed — the frequency you hear is higher. Moving away stretches them — the frequency is lower.

\`\`\`
f_observed = f\u2080 \u00d7 v / (v \u2212 v_s)
\`\`\`

- **f\u2080** — emitted frequency (Hz)
- **v** = 343 m/s — speed of sound
- **v_s** — source velocity (m/s): positive = moving toward observer

### Sign Convention

| v_s | Effect |
|-----|--------|
| > 0 | source approaching — higher pitch |
| = 0 | source stationary — unchanged |
| < 0 | source receding — lower pitch |

### Examples (f\u2080 = 440 Hz)

| v_s (m/s) | f_observed (Hz) |
|-----------|----------------|
| 0 | **440.0000** |
| 34.3 (\u2248 Mach 0.1) | **488.8889** |
| \u221234.3 | **400.0000** |

### Your Task

Implement \`dopplerShift(f0, vSource)\` returning the observed frequency (v = 343 m/s).

Run the code to **hear the Doppler shift** — ascending then descending pitch.`,

	starterCode: `function dopplerShift(f0, vSource) {
  // f = f0 * 343 / (343 - vSource)
  return 0;
}

console.log(dopplerShift(440, 0).toFixed(4));      // 440.0000
console.log(dopplerShift(440, 34.3).toFixed(4));   // 488.8889
console.log(dopplerShift(440, -34.3).toFixed(4));  // 400.0000
console.log(dopplerShift(880, 34.3).toFixed(4));   // 977.7778

// Hear the effect: approaching (high) then receding (low)
const ac = new AudioContext();
const osc = ac.createOscillator();
const gain = ac.createGain();
osc.frequency.value = 488;
osc.frequency.setValueAtTime(488, ac.currentTime);
osc.frequency.linearRampToValueAtTime(400, ac.currentTime + 1.5);
gain.gain.value = 0.3;
osc.connect(gain);
gain.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 1.5);
`,

	solution: `function dopplerShift(f0, vSource) {
  return f0 * 343 / (343 - vSource);
}

console.log(dopplerShift(440, 0).toFixed(4));      // 440.0000
console.log(dopplerShift(440, 34.3).toFixed(4));   // 488.8889
console.log(dopplerShift(440, -34.3).toFixed(4));  // 400.0000
console.log(dopplerShift(880, 34.3).toFixed(4));   // 977.7778

const ac = new AudioContext();
const osc = ac.createOscillator();
const gain = ac.createGain();
osc.frequency.value = 488;
osc.frequency.setValueAtTime(488, ac.currentTime);
osc.frequency.linearRampToValueAtTime(400, ac.currentTime + 1.5);
gain.gain.value = 0.3;
osc.connect(gain);
gain.connect(ac.destination);
osc.start();
osc.stop(ac.currentTime + 1.5);
`,

	tests: [
		{
			name: "v_s=0 \u2192 no shift (440.0000 Hz)",
			code: `{{FUNC}}
console.log(dopplerShift(440, 0).toFixed(4));`,
			expected: "440.0000\n",
		},
		{
			name: "v_s=34.3 m/s (approaching) \u2192 488.8889 Hz",
			code: `{{FUNC}}
console.log(dopplerShift(440, 34.3).toFixed(4));`,
			expected: "488.8889\n",
		},
		{
			name: "v_s=-34.3 m/s (receding) \u2192 400.0000 Hz",
			code: `{{FUNC}}
console.log(dopplerShift(440, -34.3).toFixed(4));`,
			expected: "400.0000\n",
		},
		{
			name: "f\u2080=880 Hz, v_s=34.3 m/s \u2192 977.7778 Hz",
			code: `{{FUNC}}
console.log(dopplerShift(880, 34.3).toFixed(4));`,
			expected: "977.7778\n",
		},
	],
};
