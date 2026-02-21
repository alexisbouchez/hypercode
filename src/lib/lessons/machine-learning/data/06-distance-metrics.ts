import type { Lesson } from "../../types";

export const distanceMetrics: Lesson = {
	id: "distance-metrics",
	title: "Distance Metrics",
	chapterId: "unsupervised",
	content: `## Distance Metrics

Unsupervised learning algorithms like k-NN and k-means rely on measuring **distance** between data points. Different metrics capture different notions of similarity.

### Euclidean Distance

The straight-line distance between two points $\\mathbf{a}$ and $\\mathbf{b}$:

$$d_{\\text{Euclid}}(\\mathbf{a}, \\mathbf{b}) = \\sqrt{\\sum_{i=1}^{d} (a_i - b_i)^2}$$

### Manhattan Distance

The sum of absolute differences (also called $L_1$ distance or "city block" distance):

$$d_{\\text{Manhattan}}(\\mathbf{a}, \\mathbf{b}) = \\sum_{i=1}^{d} |a_i - b_i|$$

### Cosine Similarity

Measures the **angle** between two vectors, ignoring magnitude:

$$\\text{cos\\_sim}(\\mathbf{a}, \\mathbf{b}) = \\frac{\\mathbf{a} \\cdot \\mathbf{b}}{\\|\\mathbf{a}\\| \\|\\mathbf{b}\\|}$$

- $1$ means identical direction (angle = 0°)
- $0$ means perpendicular (angle = 90°)
- $-1$ means opposite direction (angle = 180°)

Cosine similarity is widely used in text and document similarity because it is scale-invariant.

### Your Task

Implement:
- \`euclidean(a, b)\` — Euclidean ($L_2$) distance
- \`manhattan(a, b)\` — Manhattan ($L_1$) distance
- \`cosine_similarity(a, b)\` — cosine similarity`,

	starterCode: `import math

def euclidean(a, b):
    # sqrt(sum of squared differences)
    return 0.0

def manhattan(a, b):
    # sum of absolute differences
    return 0.0

def cosine_similarity(a, b):
    # dot(a,b) / (|a| * |b|)
    return 0.0

print(euclidean([0, 0], [3, 4]))                           # 5.0
print(manhattan([1, 2, 3], [4, 6, 3]))                     # 7
print(round(cosine_similarity([1, 0], [0, 1]), 4))         # 0.0
print(round(cosine_similarity([1, 1], [1, 1]), 4))         # 1.0
`,

	solution: `import math

def euclidean(a, b):
    return math.sqrt(sum((ai - bi) ** 2 for ai, bi in zip(a, b)))

def manhattan(a, b):
    return sum(abs(ai - bi) for ai, bi in zip(a, b))

def cosine_similarity(a, b):
    dot = sum(ai * bi for ai, bi in zip(a, b))
    norm_a = math.sqrt(sum(ai ** 2 for ai in a))
    norm_b = math.sqrt(sum(bi ** 2 for bi in b))
    return dot / (norm_a * norm_b)

print(euclidean([0, 0], [3, 4]))
print(manhattan([1, 2, 3], [4, 6, 3]))
print(round(cosine_similarity([1, 0], [0, 1]), 4))
print(round(cosine_similarity([1, 1], [1, 1]), 4))
`,

	tests: [
		{
			name: "euclidean([0,0],[3,4])=5.0, manhattan=7, cosine perp=0.0, cosine same=1.0",
			expected: "5.0\n7\n0.0\n1.0\n",
		},
		{
			name: "euclidean of identical points = 0.0",
			code: `{{FUNC}}
print(round(euclidean([1, 2, 3], [1, 2, 3]), 4))`,
			expected: "0.0\n",
		},
		{
			name: "manhattan([0,0],[3,4]) = 7",
			code: `{{FUNC}}
print(manhattan([0, 0], [3, 4]))`,
			expected: "7\n",
		},
		{
			name: "euclidean 1D distance = 5.0",
			code: `{{FUNC}}
print(euclidean([0], [5]))`,
			expected: "5.0\n",
		},
		{
			name: "manhattan single dimension = 3",
			code: `{{FUNC}}
print(manhattan([1], [4]))`,
			expected: "3\n",
		},
	],
};
