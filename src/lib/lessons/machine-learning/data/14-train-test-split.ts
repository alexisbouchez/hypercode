import type { Lesson } from "../../types";

export const trainTestSplit: Lesson = {
	id: "train-test-split",
	title: "Train / Test Split",
	chapterId: "evaluation",
	content: `## Train / Test Split

To evaluate a model honestly, we must test it on data it has **never seen during training**. We split the dataset into a **training set** (used to fit the model) and a **test set** (used only for evaluation).

### Splitting Strategy

1. Create an index array $[0, 1, \\ldots, n-1]$
2. **Shuffle** the indices randomly (deterministic with a seed)
3. Take the first $\\lfloor n \\cdot \\text{test\\_ratio} \\rfloor$ as test indices; the rest as train indices

### Deterministic Shuffling with LCG

We use a **Linear Congruential Generator** (LCG) for reproducible shuffles:

$$s_{i+1} = (a \\cdot s_i + c) \\bmod m$$

with $a = 1664525$, $c = 1013904223$, $m = 2^{32}$.

**Fisher-Yates shuffle**: iterate $i$ from $n-1$ down to $1$; generate $j = s \\bmod (i+1)$; swap indices $i$ and $j$.

### Stratified Ratio

For imbalanced datasets it is useful to check whether the positive class proportion is preserved:

$$\\text{stratified\\_ratio} = \\frac{\\sum_{i \\in \\text{test}} y_i}{|\\text{test}|}$$

### Your Task

Implement:
- \`train_test_split(X, y, test_ratio, seed=42)\` → (X_train, X_test, y_train, y_test)
- \`stratified_ratio(y, test_ratio)\` → fraction of positives in the test set`,

	starterCode: `def train_test_split(X, y, test_ratio, seed=42):
    # Fisher-Yates shuffle with LCG(a=1664525, c=1013904223, m=2^32)
    # Split first int(n*test_ratio) as test
    n = len(X)
    return (X, [], y, [])

def stratified_ratio(y, test_ratio):
    # Fraction of positives in the test split (seed=42)
    return 0.0

X = [[i] for i in range(10)]
y = [0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
X_train, X_test, y_train, y_test = train_test_split(X, y, 0.2)
print(len(X_train), len(X_test))     # 8 2
print(len(y_train), len(y_test))     # 8 2
print(y_test)                         # [0, 0]
print(stratified_ratio(y, 0.2))       # 0.0
`,

	solution: `def train_test_split(X, y, test_ratio, seed=42):
    n = len(X)
    indices = list(range(n))
    a, c, m = 1664525, 1013904223, 2 ** 32
    state = seed
    for i in range(n - 1, 0, -1):
        state = (a * state + c) % m
        j = state % (i + 1)
        indices[i], indices[j] = indices[j], indices[i]
    test_size = int(n * test_ratio)
    test_idx = indices[:test_size]
    train_idx = indices[test_size:]
    X_train = [X[i] for i in train_idx]
    X_test = [X[i] for i in test_idx]
    y_train = [y[i] for i in train_idx]
    y_test = [y[i] for i in test_idx]
    return (X_train, X_test, y_train, y_test)

def stratified_ratio(y, test_ratio):
    n = len(y)
    indices = list(range(n))
    a, c, m = 1664525, 1013904223, 2 ** 32
    state = 42
    for i in range(n - 1, 0, -1):
        state = (a * state + c) % m
        j = state % (i + 1)
        indices[i], indices[j] = indices[j], indices[i]
    test_size = int(n * test_ratio)
    test_idx = indices[:test_size]
    y_test = [y[i] for i in test_idx]
    return round(sum(y_test) / len(y_test), 4) if y_test else 0.0

X = [[i] for i in range(10)]
y = [0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
X_train, X_test, y_train, y_test = train_test_split(X, y, 0.2)
print(len(X_train), len(X_test))
print(len(y_train), len(y_test))
print(y_test)
print(stratified_ratio(y, 0.2))
`,

	tests: [
		{
			name: "split sizes (8,2), y_test=[0,0], stratified_ratio=0.0",
			expected: "8 2\n8 2\n[0, 0]\n0.0\n",
		},
		{
			name: "split sizes for 20% of 5 samples",
			code: `{{FUNC}}
X = [[i] for i in range(5)]
y = [0, 0, 0, 0, 0]
Xtr, Xte, ytr, yte = train_test_split(X, y, 0.2)
print(len(Xtr), len(Xte))`,
			expected: "4 1\n",
		},
		{
			name: "train_test_split returns correct total sizes",
			code: `{{FUNC}}
X = [[i] for i in range(100)]
y = list(range(100))
Xtr, Xte, ytr, yte = train_test_split(X, y, 0.3)
print(len(Xtr) + len(Xte))`,
			expected: "100\n",
		},
		{
			name: "stratified_ratio all-ones data with 50% split",
			code: `{{FUNC}}
y = [1, 1, 1, 1]
print(stratified_ratio(y, 0.5))`,
			expected: "1.0\n",
		},
	],
};
