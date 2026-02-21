import type { Lesson } from "../../types";

export const knn: Lesson = {
	id: "knn",
	title: "K-Nearest Neighbours",
	chapterId: "unsupervised",
	content: `## K-Nearest Neighbours (k-NN)

**K-Nearest Neighbours** is one of the simplest machine learning algorithms. To classify a new point, it:

1. Computes the distance from the query point to every training point
2. Selects the $k$ closest training points
3. Returns the most common label among those $k$ neighbours

$$\\hat{y} = \\text{mode}\\left(\\{y^{(i)} : i \\in \\text{k-nearest}(\\mathbf{x})\\}\\right)$$

### Algorithm

\`\`\`
for each training point:
    compute euclidean distance to query
sort by distance
take k smallest
return the majority label
\`\`\`

### Tie-breaking

When two labels are equally common among the $k$ neighbours, return the **smaller** label value.

### Properties

- **Non-parametric**: no training phase, all computation at prediction time
- **Lazy learner**: stores the entire training set
- **$k$ is a hyperparameter**: small $k$ = low bias, high variance; large $k$ = high bias, low variance

### Your Task

Implement \`knn_classify(X_train, y_train, x_query, k)\` that returns the most common label among the $k$ nearest neighbours. Use Euclidean distance. Break ties by returning the lowest label value.`,

	starterCode: `import math

def euclidean(a, b):
    return math.sqrt(sum((ai - bi) ** 2 for ai, bi in zip(a, b)))

def knn_classify(X_train, y_train, x_query, k):
    # Find k nearest neighbours and return majority label
    return 0

X_train = [[0,0], [1,0], [0,1], [3,3], [4,3], [3,4]]
y_train = [0, 0, 0, 1, 1, 1]
print(knn_classify(X_train, y_train, [0.5, 0.5], 3))  # 0
print(knn_classify(X_train, y_train, [3.5, 3.5], 3))  # 1
print(knn_classify(X_train, y_train, [0, 0], 1))       # 0
`,

	solution: `import math

def euclidean(a, b):
    return math.sqrt(sum((ai - bi) ** 2 for ai, bi in zip(a, b)))

def knn_classify(X_train, y_train, x_query, k):
    distances = [(euclidean(x, x_query), y) for x, y in zip(X_train, y_train)]
    distances.sort(key=lambda d: (d[0], d[1]))
    k_nearest = [d[1] for d in distances[:k]]
    counts = {}
    for label in k_nearest:
        counts[label] = counts.get(label, 0) + 1
    return min(counts, key=lambda l: (-counts[l], l))

X_train = [[0,0], [1,0], [0,1], [3,3], [4,3], [3,4]]
y_train = [0, 0, 0, 1, 1, 1]
print(knn_classify(X_train, y_train, [0.5, 0.5], 3))
print(knn_classify(X_train, y_train, [3.5, 3.5], 3))
print(knn_classify(X_train, y_train, [0, 0], 1))
`,

	tests: [
		{
			name: "knn near origin=0, near cluster 2=1, exact match k=1",
			expected: "0\n1\n0\n",
		},
		{
			name: "knn k=1 nearest to [4,3] returns 1",
			code: `{{FUNC}}
X_train = [[0,0], [1,0], [0,1], [3,3], [4,3], [3,4]]
y_train = [0, 0, 0, 1, 1, 1]
print(knn_classify(X_train, y_train, [4, 3], 1))`,
			expected: "1\n",
		},
		{
			name: "knn k=3 midpoint query",
			code: `{{FUNC}}
X_train = [[0,0], [1,0], [0,1], [3,3], [4,3], [3,4]]
y_train = [0, 0, 0, 1, 1, 1]
print(knn_classify(X_train, y_train, [3, 3], 1))`,
			expected: "1\n",
		},
		{
			name: "knn k=5 majority vote",
			code: `{{FUNC}}
X_train = [[0,0], [1,0], [0,1], [1,1], [5,5]]
y_train = [0, 0, 0, 0, 1]
print(knn_classify(X_train, y_train, [1, 1], 5))`,
			expected: "0\n",
		},
	],
};
