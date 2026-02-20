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
import numpy as np

v = np.array([3, 4])
print(np.linalg.norm(v))  # √(9 + 16) = √25 = 5.0
\`\`\`

### Unit Vectors

A **unit vector** has norm 1. To **normalize** a vector — convert it to a unit vector — divide by its norm:

\`\`\`python
v = np.array([3, 4])
norm = np.linalg.norm(v)   # 5.0
unit = v / norm            # [0.6, 0.8]

print(np.linalg.norm(unit))  # 1.0
\`\`\`

Unit vectors preserve direction but discard magnitude. They answer: *which way?* rather than *how far?*

### Why It Matters

- **Normalization** is used everywhere in ML: embeddings, attention weights, cosine similarity
- The dot product formula \`a · b = ‖a‖‖b‖cos(θ)\` uses norms to extract the angle
- **L2 regularization** in neural networks penalizes the norm of the weight vector

### Higher-Order Norms

\`np.linalg.norm(v, ord=1)\` gives the **L1 norm** (Manhattan distance): sum of absolute values.
\`np.linalg.norm(v, ord=np.inf)\` gives the **L∞ norm**: the maximum absolute value.

### Your Task

Implement \`normalize(v)\` that returns the unit vector in the direction of \`v\`, rounded to 2 decimal places.`,

	starterCode: `import numpy as np

def normalize(v):
    # Return the unit vector in the direction of v, rounded to 2 decimal places
    pass

v = np.array([3, 4])
print(normalize(v))
print(np.linalg.norm(normalize(v)))
`,

	solution: `import numpy as np

def normalize(v):
    norm = np.linalg.norm(v)
    return np.round(v / norm, 2)

v = np.array([3, 4])
print(normalize(v))
print(np.linalg.norm(normalize(v)))
`,

	tests: [
		{
			name: "normalize [3,4] → [0.6, 0.8]",
			expected: "[0.6 0.8]\n1.0\n",
		},
		{
			name: "normalize [1,0] → [1.0, 0.0]",
			code: `{{FUNC}}
print(normalize(np.array([1, 0])))`,
			expected: "[1. 0.]\n",
		},
		{
			name: "normalize [5,0,0] → [1.0, 0.0, 0.0]",
			code: `{{FUNC}}
print(normalize(np.array([5, 0, 0])))`,
			expected: "[1. 0. 0.]\n",
		},
		{
			name: "normalize [0,0,4] → [0.0, 0.0, 1.0]",
			code: `{{FUNC}}
print(normalize(np.array([0, 0, 4])))`,
			expected: "[0. 0. 1.]\n",
		},
	],
};
