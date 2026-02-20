import type { Lesson } from "../../types";

export const soundIntensityLesson: Lesson = {
	id: "sound-intensity",
	title: "Sound Intensity",
	chapterId: "intensity-and-perception",
	content: `## Intensity of a Point Source

A speaker radiates power P (watts) equally in all directions. At distance r, that power is spread over a sphere of surface area 4\u03c0r\u00b2:

\`\`\`
I = P / (4\u03c0 r\u00b2)
\`\`\`

- **I** — intensity (W/m\u00b2)
- **P** — acoustic power (W)
- **r** — distance from source (m)

### Inverse-Square Law

Doubling the distance quarters the intensity — the same r\u00b2 dependence as gravity and light. This is why outdoor concerts sound quiet from far away.

| P (W) | r (m) | I (W/m\u00b2) |
|-------|-------|-----------|
| 1 | 1 | **0.0796** |
| 100 | 10 | **0.0796** |
| 1 | 2 | **0.0199** |
| 4 | 1 | **0.3183** |

### Your Task

Implement \`soundIntensity(P, r)\` returning intensity in W/m\u00b2.`,

	starterCode: `function soundIntensity(P, r) {
  // I = P / (4 * PI * r^2)
  return 0;
}

console.log(soundIntensity(1, 1).toFixed(4));     // 0.0796
console.log(soundIntensity(100, 10).toFixed(4));  // 0.0796
console.log(soundIntensity(1, 2).toFixed(4));     // 0.0199
console.log(soundIntensity(4, 1).toFixed(4));     // 0.3183
`,

	solution: `function soundIntensity(P, r) {
  return P / (4 * Math.PI * r * r);
}

console.log(soundIntensity(1, 1).toFixed(4));     // 0.0796
console.log(soundIntensity(100, 10).toFixed(4));  // 0.0796
console.log(soundIntensity(1, 2).toFixed(4));     // 0.0199
console.log(soundIntensity(4, 1).toFixed(4));     // 0.3183
`,

	tests: [
		{
			name: "P=1 W, r=1 m \u2192 0.0796 W/m\u00b2",
			code: `{{FUNC}}
console.log(soundIntensity(1, 1).toFixed(4));`,
			expected: "0.0796\n",
		},
		{
			name: "P=100 W, r=10 m \u2192 0.0796 W/m\u00b2 (same intensity)",
			code: `{{FUNC}}
console.log(soundIntensity(100, 10).toFixed(4));`,
			expected: "0.0796\n",
		},
		{
			name: "P=1 W, r=2 m \u2192 0.0199 W/m\u00b2 (double distance, quarter intensity)",
			code: `{{FUNC}}
console.log(soundIntensity(1, 2).toFixed(4));`,
			expected: "0.0199\n",
		},
		{
			name: "P=4 W, r=1 m \u2192 0.3183 W/m\u00b2",
			code: `{{FUNC}}
console.log(soundIntensity(4, 1).toFixed(4));`,
			expected: "0.3183\n",
		},
	],
};
