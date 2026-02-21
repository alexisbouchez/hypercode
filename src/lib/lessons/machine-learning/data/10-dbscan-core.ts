import type { Lesson } from "../../types";

export const dbscanCore: Lesson = {
	id: "dbscan-core",
	title: "DBSCAN Core Operations",
	chapterId: "unsupervised",
	content: `## DBSCAN: Density-Based Clustering

**DBSCAN** (Density-Based Spatial Application of Noise) clusters points that are densely packed together while labelling sparse outliers as noise. Unlike k-means, it does not require choosing $k$ in advance.

### Key Concepts

**Epsilon neighbourhood** ($\\varepsilon$-neighbourhood): the set of points within distance $\\varepsilon$ of a point $p$:

$$N_\\varepsilon(p) = \\{q \\in X : d(p, q) \\leq \\varepsilon\\}$$

**Core point**: a point with at least \`min_samples\` neighbours within $\\varepsilon$ (excluding itself):

$$|N_\\varepsilon(p)| \\geq \\text{min\\_samples}$$

**Border point**: within $\\varepsilon$ of a core point but not itself a core point.

**Noise point**: neither core nor border.

### Algorithm Sketch

\`\`\`
for each unvisited point p:
    if p is a core point:
        expand cluster from p (BFS/DFS through neighbours)
    else:
        mark p as noise (may later become a border point)
\`\`\`

### Your Task

Implement the building blocks:
- \`neighbors(X, idx, eps)\` → indices of points within $\\varepsilon$ (excluding \`idx\` itself)
- \`is_core_point(X, idx, eps, min_samples)\` → True if $|N_\\varepsilon| \\geq$ min_samples
- \`range_query(X, idx, eps)\` → same as \`neighbors\` (alias used during cluster expansion)`,

	starterCode: `import math

def euclidean(a, b):
    return math.sqrt(sum((ai - bi) ** 2 for ai, bi in zip(a, b)))

def neighbors(X, idx, eps):
    # Return indices i != idx where euclidean(X[i], X[idx]) <= eps
    return []

def is_core_point(X, idx, eps, min_samples):
    # True if len(neighbors) >= min_samples
    return False

def range_query(X, idx, eps):
    # Same as neighbors
    return []

X = [[0,0], [0.5,0], [1,0], [5,5], [5.5,5], [10,10]]
print(neighbors(X, 0, 1.0))              # [1, 2]
print(is_core_point(X, 0, 1.0, 2))       # True
print(is_core_point(X, 5, 1.0, 2))       # False
print(range_query(X, 3, 1.0))            # [4]
`,

	solution: `import math

def euclidean(a, b):
    return math.sqrt(sum((ai - bi) ** 2 for ai, bi in zip(a, b)))

def neighbors(X, idx, eps):
    return [i for i in range(len(X)) if i != idx and euclidean(X[i], X[idx]) <= eps]

def is_core_point(X, idx, eps, min_samples):
    return len(neighbors(X, idx, eps)) >= min_samples

def range_query(X, idx, eps):
    return neighbors(X, idx, eps)

X = [[0,0], [0.5,0], [1,0], [5,5], [5.5,5], [10,10]]
print(neighbors(X, 0, 1.0))
print(is_core_point(X, 0, 1.0, 2))
print(is_core_point(X, 5, 1.0, 2))
print(range_query(X, 3, 1.0))
`,

	tests: [
		{
			name: "neighbors=[1,2], core_point(0)=True, core_point(5)=False, range_query=[4]",
			expected: "[1, 2]\nTrue\nFalse\n[4]\n",
		},
		{
			name: "isolated point has no neighbors",
			code: `{{FUNC}}
X = [[0,0], [10,10]]
print(neighbors(X, 0, 1.0))`,
			expected: "[]\n",
		},
		{
			name: "is_core_point with min_samples=1",
			code: `{{FUNC}}
X = [[0,0], [0.5,0]]
print(is_core_point(X, 0, 1.0, 1))`,
			expected: "True\n",
		},
		{
			name: "range_query returns same as neighbors",
			code: `{{FUNC}}
X = [[0,0], [0.5,0], [1,0], [5,5]]
print(range_query(X, 1, 0.6))`,
			expected: "[0, 2]\n",
		},
	],
};
