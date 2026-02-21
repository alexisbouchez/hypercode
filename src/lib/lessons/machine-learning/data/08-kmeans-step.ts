import type { Lesson } from "../../types";

export const kmeansStep: Lesson = {
	id: "kmeans-step",
	title: "K-Means Clustering",
	chapterId: "unsupervised",
	content: `## K-Means Clustering

**K-means** is the most widely used clustering algorithm. It partitions $n$ data points into $k$ clusters by iterating two steps:

### Step 1 — Assignment

Assign each point to the nearest centroid:

$$c^{(i)} = \\arg\\min_j \\| \\mathbf{x}^{(i)} - \\boldsymbol{\\mu}_j \\|_2$$

### Step 2 — Update

Move each centroid to the mean of its assigned points:

$$\\boldsymbol{\\mu}_j = \\frac{1}{|C_j|} \\sum_{i \\in C_j} \\mathbf{x}^{(i)}$$

### Inertia (Within-Cluster Sum of Squares)

A common quality metric is **inertia** — the sum of squared distances from each point to its assigned centroid:

$$\\text{Inertia} = \\sum_{i=1}^{n} \\| \\mathbf{x}^{(i)} - \\boldsymbol{\\mu}_{c^{(i)}} \\|_2^2$$

Lower inertia means tighter, more compact clusters.

### Your Task

Implement:
- \`assign_clusters(X, centroids)\` → list of cluster indices (one per point)
- \`update_centroids(X, assignments, k)\` → new centroids as mean of assigned points
- \`kmeans_inertia(X, assignments, centroids)\` → total within-cluster sum of squares`,

	starterCode: `import math

def euclidean(a, b):
    return math.sqrt(sum((ai - bi) ** 2 for ai, bi in zip(a, b)))

def assign_clusters(X, centroids):
    # Return list of cluster indices (nearest centroid for each point)
    return []

def update_centroids(X, assignments, k):
    # Return new centroids as mean of each cluster's points
    return []

def kmeans_inertia(X, assignments, centroids):
    # Sum of squared distances to assigned centroid
    return 0.0

X = [[0,0], [1,0], [0,1], [5,5], [6,5], [5,6]]
centroids = [[0,0], [5,5]]
a = assign_clusters(X, centroids)
print(a)                                        # [0, 0, 0, 1, 1, 1]
nc = update_centroids(X, a, 2)
print([round(v, 4) for v in nc[0]])             # [0.3333, 0.3333]
print(round(kmeans_inertia(X, a, centroids), 4)) # 4.0
`,

	solution: `import math

def euclidean(a, b):
    return math.sqrt(sum((ai - bi) ** 2 for ai, bi in zip(a, b)))

def assign_clusters(X, centroids):
    assignments = []
    for x in X:
        dists = [euclidean(x, c) for c in centroids]
        assignments.append(dists.index(min(dists)))
    return assignments

def update_centroids(X, assignments, k):
    dim = len(X[0])
    centroids = []
    for ci in range(k):
        points = [X[i] for i in range(len(X)) if assignments[i] == ci]
        if points:
            centroid = [sum(p[d] for p in points) / len(points) for d in range(dim)]
        else:
            centroid = [0.0] * dim
        centroids.append(centroid)
    return centroids

def kmeans_inertia(X, assignments, centroids):
    return sum(euclidean(X[i], centroids[assignments[i]]) ** 2 for i in range(len(X)))

X = [[0,0], [1,0], [0,1], [5,5], [6,5], [5,6]]
centroids = [[0,0], [5,5]]
a = assign_clusters(X, centroids)
print(a)
nc = update_centroids(X, a, 2)
print([round(v, 4) for v in nc[0]])
print(round(kmeans_inertia(X, a, centroids), 4))
`,

	tests: [
		{
			name: "assign=[0,0,0,1,1,1], centroid0=[0.3333,0.3333], inertia=4.0",
			expected: "[0, 0, 0, 1, 1, 1]\n[0.3333, 0.3333]\n4.0\n",
		},
		{
			name: "assign_clusters with single centroid assigns everything to cluster 0",
			code: `{{FUNC}}
X = [[1,2], [3,4], [5,6]]
print(assign_clusters(X, [[0,0]]))`,
			expected: "[0, 0, 0]\n",
		},
		{
			name: "update_centroids produces correct mean",
			code: `{{FUNC}}
X = [[0,0], [2,0], [1,0]]
assignments = [0, 0, 0]
nc = update_centroids(X, assignments, 1)
print([round(v, 4) for v in nc[0]])`,
			expected: "[1.0, 0.0]\n",
		},
		{
			name: "kmeans_inertia with points on centroids = 0.0",
			code: `{{FUNC}}
X = [[1,1], [3,3]]
centroids = [[1,1], [3,3]]
a = [0, 1]
print(kmeans_inertia(X, a, centroids))`,
			expected: "0.0\n",
		},
	],
};
