import type { Lesson } from "../../types";

export const vectorNorm: Lesson = {
	id: "vector-norm",
	title: "Vector Norm",
	chapterId: "vectors",
	content: `## Vector Norm (Magnitude)

The **norm** (or magnitude) of a vector is its length:

\`\`\`
‖v‖ = √(v₀² + v₁² + ... + vₙ²)
\`\`\`

\`\`\`python
import math

v = [3, 4]
norm = math.sqrt(sum(x**2 for x in v))
print(norm)  # √(9 + 16) = √25 = 5.0
\`\`\`

### Unit Vectors

A **unit vector** has norm 1. To **normalize** a vector — convert it to a unit vector — divide by its norm:

\`\`\`python
import math

v = [3, 4]
norm = math.sqrt(sum(x**2 for x in v))   # 5.0
unit = [round(x / norm, 2) for x in v]   # [0.6, 0.8]

print(math.sqrt(sum(x**2 for x in unit)))  # ≈ 1.0
\`\`\`

Unit vectors preserve direction but discard magnitude. They answer: *which way?* rather than *how far?*

### Why It Matters

- **Normalization** is used everywhere in ML: embeddings, attention weights, cosine similarity
- The dot product formula \`a · b = ‖a‖‖b‖cos(θ)\` uses norms to extract the angle
- **L2 regularization** in neural networks penalizes the norm of the weight vector

### Your Task

Implement \`normalize(v)\` that returns the unit vector in the direction of \`v\`, with each component rounded to 2 decimal places.`,

	starterCode: `import math

def normalize(v):
    # Return unit vector as a list, each component rounded to 2 decimal places
    pass

v = [3, 4]
u = normalize(v)
print(u)
print(round(math.sqrt(sum(x**2 for x in u)), 1))
`,

	solution: `import math

def normalize(v):
    norm = math.sqrt(sum(x**2 for x in v))
    return [round(x / norm, 2) for x in v]

v = [3, 4]
u = normalize(v)
print(u)
print(round(math.sqrt(sum(x**2 for x in u)), 1))
`,

	tests: [
		{
			name: "normalize [3,4] → [0.6, 0.8], norm ≈ 1.0",
			expected: "[0.6, 0.8]\n1.0\n",
		},
		{
			name: "normalize [1,0] → [1.0, 0.0]",
			code: `{{FUNC}}
print(normalize([1, 0]))`,
			expected: "[1.0, 0.0]\n",
		},
		{
			name: "normalize [5,0,0] → [1.0, 0.0, 0.0]",
			code: `{{FUNC}}
print(normalize([5, 0, 0]))`,
			expected: "[1.0, 0.0, 0.0]\n",
		},
		{
			name: "normalize [0,0,4] → [0.0, 0.0, 1.0]",
			code: `{{FUNC}}
print(normalize([0, 0, 4]))`,
			expected: "[0.0, 0.0, 1.0]\n",
		},
	],
};
