import type { Lesson } from "../../types";

export const reverberationLesson: Lesson = {
	id: "reverberation",
	title: "Reverberation Time",
	chapterId: "room-acoustics",
	content: `## Sabine's Formula

**Reverberation time** ($T_{60}$) is how long it takes a sound to decay by 60 dB after the source stops. Wallace Sabine derived in 1900:

$$T_{60} = \frac{0.161 \cdot V}{A}$$

- **V** — room volume (m³)
- **A** — total acoustic absorption (m² sabins) = $\sum(\text{surface area} \times \text{absorption coefficient})$
- $T_{60}$ — reverberation time (s)

### What It Means

| $T_{60}$ (s) | Acoustic character |
|---------|-------------------|
| < 0.5 | dead / anechoic |
| 0.5–1.0 | speech intelligibility (lecture hall) |
| 1.5–2.5 | music (concert hall) |
| > 3.0 | cathedral / very reverberant |

### Examples

| V (m³) | A (m²) | $T_{60}$ (s) |
|--------|--------|---------|
| 100 | 10 | **1.6100** |
| 200 | 20 | **1.6100** |
| 500 | 100 | **0.8050** |
| 1000 | 50 | **3.2200** |

### Your Task

Implement \`reverbTime(V, A)\` returning $T_{60}$ in seconds.`,

	starterCode: `function reverbTime(V, A) {
  // T60 = 0.161 * V / A
  return 0;
}

console.log(reverbTime(100, 10).toFixed(4));    // 1.6100
console.log(reverbTime(200, 20).toFixed(4));    // 1.6100
console.log(reverbTime(500, 100).toFixed(4));   // 0.8050
console.log(reverbTime(1000, 50).toFixed(4));   // 3.2200
`,

	solution: `function reverbTime(V, A) {
  return 0.161 * V / A;
}

console.log(reverbTime(100, 10).toFixed(4));    // 1.6100
console.log(reverbTime(200, 20).toFixed(4));    // 1.6100
console.log(reverbTime(500, 100).toFixed(4));   // 0.8050
console.log(reverbTime(1000, 50).toFixed(4));   // 3.2200
`,

	tests: [
		{
			name: "V=100 m³, A=10 m² → T₆₀=1.6100 s",
			code: `{{FUNC}}
console.log(reverbTime(100, 10).toFixed(4));`,
			expected: "1.6100\n",
		},
		{
			name: "V=200 m³, A=20 m² → T₆₀=1.6100 s (same ratio)",
			code: `{{FUNC}}
console.log(reverbTime(200, 20).toFixed(4));`,
			expected: "1.6100\n",
		},
		{
			name: "V=500 m³, A=100 m² → T₆₀=0.8050 s",
			code: `{{FUNC}}
console.log(reverbTime(500, 100).toFixed(4));`,
			expected: "0.8050\n",
		},
		{
			name: "V=1000 m³, A=50 m² → T₆₀=3.2200 s (cathedral-like)",
			code: `{{FUNC}}
console.log(reverbTime(1000, 50).toFixed(4));`,
			expected: "3.2200\n",
		},
	],
};
