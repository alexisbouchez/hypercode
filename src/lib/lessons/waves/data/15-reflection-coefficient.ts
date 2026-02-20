import type { Lesson } from "../../types";

export const reflectionCoefficientLesson: Lesson = {
	id: "reflection-coefficient",
	title: "Acoustic Reflection",
	chapterId: "room-acoustics",
	content: `## Acoustic Impedance & Reflection

When a sound wave hits a boundary between two media, part is reflected and part is transmitted. The fraction of intensity reflected depends on the **acoustic impedances** Z₁ and Z₂:

\`\`\`
R = ((Z₂ − Z₁) / (Z₂ + Z₁))²
\`\`\`

- **Z = ρv** — acoustic impedance (Pa·s/m), where ρ is density and v is sound speed
- **R** — reflection coefficient (0 to 1)

### Matched Impedance

When Z₁ = Z₂ (same medium on both sides), R = 0 — no reflection, perfect transmission. This is why ultrasound gel is used: it matches skin and water impedances to minimise reflection.

### Large Mismatch

Air (Z ≈ 413 Pa·s/m) vs water (Z ≈ 1.48 × 10⁶ Pa·s/m) have such different impedances that nearly all sound reflects — explaining why you can't hear underwater from above.

| Z₁ | Z₂ | R |
|----|----|----|
| 1 | 1 | **0.0000** (no reflection) |
| 1 | 3 | **0.2500** |
| 1 | 9 | **0.6400** |
| 1 | 100 | **0.9608** (strong reflection) |

### Your Task

Implement \`reflectionCoeff(Z1, Z2)\` returning the reflection coefficient.`,

	starterCode: `function reflectionCoeff(Z1, Z2) {
  // R = ((Z2 - Z1) / (Z2 + Z1))^2
  return 0;
}

console.log(reflectionCoeff(1, 1).toFixed(4));     // 0.0000
console.log(reflectionCoeff(1, 3).toFixed(4));     // 0.2500
console.log(reflectionCoeff(1, 9).toFixed(4));     // 0.6400
console.log(reflectionCoeff(1, 100).toFixed(4));   // 0.9608
`,

	solution: `function reflectionCoeff(Z1, Z2) {
  const ratio = (Z2 - Z1) / (Z2 + Z1);
  return ratio * ratio;
}

console.log(reflectionCoeff(1, 1).toFixed(4));     // 0.0000
console.log(reflectionCoeff(1, 3).toFixed(4));     // 0.2500
console.log(reflectionCoeff(1, 9).toFixed(4));     // 0.6400
console.log(reflectionCoeff(1, 100).toFixed(4));   // 0.9608
`,

	tests: [
		{
			name: "Z₁=Z₂ → R=0.0000 (no reflection)",
			code: `{{FUNC}}
console.log(reflectionCoeff(1, 1).toFixed(4));`,
			expected: "0.0000\n",
		},
		{
			name: "Z₁=1, Z₂=3 → R=0.2500",
			code: `{{FUNC}}
console.log(reflectionCoeff(1, 3).toFixed(4));`,
			expected: "0.2500\n",
		},
		{
			name: "Z₁=1, Z₂=9 → R=0.6400",
			code: `{{FUNC}}
console.log(reflectionCoeff(1, 9).toFixed(4));`,
			expected: "0.6400\n",
		},
		{
			name: "Z₁=1, Z₂=100 → R=0.9608 (large mismatch)",
			code: `{{FUNC}}
console.log(reflectionCoeff(1, 100).toFixed(4));`,
			expected: "0.9608\n",
		},
	],
};
