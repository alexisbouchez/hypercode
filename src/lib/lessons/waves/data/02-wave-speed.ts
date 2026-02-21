import type { Lesson } from "../../types";

export const waveSpeedLesson: Lesson = {
	id: "wave-speed",
	title: "Wave Speed",
	chapterId: "wave-fundamentals",
	content: `## Wave Speed

A wave has both frequency and spatial extent. The **wavelength** $\lambda$ is the distance between successive crests. The wave equation connects all three quantities:

$$v = f\lambda$$

- **v** — wave speed (m/s)
- **f** — frequency (Hz)
- $\lambda$ — wavelength (m)

### Sound in Air

Sound travels at $\approx 343$ m/s at 20 °C. This links frequency and wavelength for every audible tone:

| f (Hz) | $\lambda$ (m) | v (m/s) |
|--------|-------|---------|
| 440 | 0.780 | **343.20** |
| 1000 | 0.343 | **343.00** |
| 200 | 1.715 | **343.00** |
| 100 | 3.430 | **343.00** |

### Your Task

Implement \`waveSpeed(f, lambda)\` returning the wave speed in m/s.`,

	starterCode: `function waveSpeed(f, lambda) {
  // v = f * lambda
  return 0;
}

console.log(waveSpeed(440, 0.780).toFixed(2));    // 343.20
console.log(waveSpeed(1000, 0.343).toFixed(2));   // 343.00
console.log(waveSpeed(200, 1.715).toFixed(2));    // 343.00
console.log(waveSpeed(100, 3.43).toFixed(2));     // 343.00
`,

	solution: `function waveSpeed(f, lambda) {
  return f * lambda;
}

console.log(waveSpeed(440, 0.780).toFixed(2));    // 343.20
console.log(waveSpeed(1000, 0.343).toFixed(2));   // 343.00
console.log(waveSpeed(200, 1.715).toFixed(2));    // 343.00
console.log(waveSpeed(100, 3.43).toFixed(2));     // 343.00
`,

	tests: [
		{
			name: "f=440 Hz, \u03bb=0.780 m \u2192 343.20 m/s",
			code: `{{FUNC}}
console.log(waveSpeed(440, 0.780).toFixed(2));`,
			expected: "343.20\n",
		},
		{
			name: "f=1000 Hz, \u03bb=0.343 m \u2192 343.00 m/s",
			code: `{{FUNC}}
console.log(waveSpeed(1000, 0.343).toFixed(2));`,
			expected: "343.00\n",
		},
		{
			name: "f=200 Hz, \u03bb=1.715 m \u2192 343.00 m/s",
			code: `{{FUNC}}
console.log(waveSpeed(200, 1.715).toFixed(2));`,
			expected: "343.00\n",
		},
		{
			name: "f=100 Hz, \u03bb=3.43 m \u2192 343.00 m/s",
			code: `{{FUNC}}
console.log(waveSpeed(100, 3.43).toFixed(2));`,
			expected: "343.00\n",
		},
	],
};
