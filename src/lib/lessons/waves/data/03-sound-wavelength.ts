import type { Lesson } from "../../types";

export const soundWavelengthLesson: Lesson = {
	id: "sound-wavelength",
	title: "Sound Wavelength",
	chapterId: "wave-fundamentals",
	content: `## Wavelength of Sound

Rearranging the wave equation ($v = f\lambda$) for wavelength:

$$\lambda = \frac{v}{f}$$

In air at 20 °C, $v = 343$ m/s. So for any audible frequency:

$$\lambda = \frac{343}{f}$$

### Why It Matters

Wavelength governs diffraction: sound bends around obstacles whose size is comparable to $\lambda$. Bass frequencies (long $\lambda$) bend around walls; treble frequencies (short $\lambda$) are more directional.

| f (Hz) | $\lambda$ (m) | Example |
|--------|-------|---------|
| 100 | **3.4300** | ~door width |
| 343 | **1.0000** | ~1 m |
| 440 | **0.7795** | concert A |
| 1000 | **0.3430** | ~smartphone |

### Your Task

Implement \`soundWavelength(f)\` returning the wavelength in metres ($v = 343$ m/s).`,

	starterCode: `function soundWavelength(f) {
  // lambda = 343 / f
  return 0;
}

console.log(soundWavelength(343).toFixed(4));    // 1.0000
console.log(soundWavelength(1000).toFixed(4));   // 0.3430
console.log(soundWavelength(100).toFixed(4));    // 3.4300
console.log(soundWavelength(440).toFixed(4));    // 0.7795
`,

	solution: `function soundWavelength(f) {
  return 343 / f;
}

console.log(soundWavelength(343).toFixed(4));    // 1.0000
console.log(soundWavelength(1000).toFixed(4));   // 0.3430
console.log(soundWavelength(100).toFixed(4));    // 3.4300
console.log(soundWavelength(440).toFixed(4));    // 0.7795
`,

	tests: [
		{
			name: "f=343 Hz \u2192 \u03bb=1.0000 m",
			code: `{{FUNC}}
console.log(soundWavelength(343).toFixed(4));`,
			expected: "1.0000\n",
		},
		{
			name: "f=1000 Hz \u2192 \u03bb=0.3430 m",
			code: `{{FUNC}}
console.log(soundWavelength(1000).toFixed(4));`,
			expected: "0.3430\n",
		},
		{
			name: "f=100 Hz \u2192 \u03bb=3.4300 m",
			code: `{{FUNC}}
console.log(soundWavelength(100).toFixed(4));`,
			expected: "3.4300\n",
		},
		{
			name: "f=440 Hz \u2192 \u03bb=0.7795 m",
			code: `{{FUNC}}
console.log(soundWavelength(440).toFixed(4));`,
			expected: "0.7795\n",
		},
	],
};
