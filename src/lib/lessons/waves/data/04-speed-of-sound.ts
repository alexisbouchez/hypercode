import type { Lesson } from "../../types";

export const speedOfSoundLesson: Lesson = {
	id: "speed-of-sound",
	title: "Speed of Sound in Air",
	chapterId: "wave-fundamentals",
	content: `## Temperature Dependence

Sound speed in air depends on temperature. The linear approximation:

\`\`\`
v \u2248 331 + 0.6 \u00d7 T_C
\`\`\`

where **T_C** is the temperature in Celsius.

### Physics Behind It

Sound travels by compressing adjacent air molecules. Warmer air has faster-moving molecules, so disturbances propagate more quickly. The exact formula uses the adiabatic index and molar mass, but the linear form is accurate to within 0.5% for typical temperatures.

### Effect on Tuning

An orchestra tunes at 20 \u00b0C (343 m/s). If the stage warms to 30 \u00b0C (349 m/s), instruments that haven't warmed up play sharp — wavelengths shorten at higher speeds.

| T (\u00b0C) | v (m/s) |
|--------|---------|
| -20 | **319.0** |
| 0 | **331.0** |
| 20 | **343.0** |
| 100 | **391.0** |

### Your Task

Implement \`speedOfSound(celsius)\` using the linear approximation.`,

	starterCode: `function speedOfSound(celsius) {
  // v = 331 + 0.6 * celsius
  return 0;
}

console.log(speedOfSound(0).toFixed(1));    // 331.0
console.log(speedOfSound(20).toFixed(1));   // 343.0
console.log(speedOfSound(100).toFixed(1));  // 391.0
console.log(speedOfSound(-20).toFixed(1));  // 319.0
`,

	solution: `function speedOfSound(celsius) {
  return 331 + 0.6 * celsius;
}

console.log(speedOfSound(0).toFixed(1));    // 331.0
console.log(speedOfSound(20).toFixed(1));   // 343.0
console.log(speedOfSound(100).toFixed(1));  // 391.0
console.log(speedOfSound(-20).toFixed(1));  // 319.0
`,

	tests: [
		{
			name: "T=0 \u00b0C \u2192 331.0 m/s",
			code: `{{FUNC}}
console.log(speedOfSound(0).toFixed(1));`,
			expected: "331.0\n",
		},
		{
			name: "T=20 \u00b0C \u2192 343.0 m/s",
			code: `{{FUNC}}
console.log(speedOfSound(20).toFixed(1));`,
			expected: "343.0\n",
		},
		{
			name: "T=100 \u00b0C \u2192 391.0 m/s",
			code: `{{FUNC}}
console.log(speedOfSound(100).toFixed(1));`,
			expected: "391.0\n",
		},
		{
			name: "T=-20 \u00b0C \u2192 319.0 m/s",
			code: `{{FUNC}}
console.log(speedOfSound(-20).toFixed(1));`,
			expected: "319.0\n",
		},
	],
};
