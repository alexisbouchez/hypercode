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

### k-Fold Cross-Validation

A single train/test split can be noisy — the model's score depends heavily on which examples land in the test set. **k-fold cross-validation** gives a more robust estimate by repeating the evaluation $k$ times:

1. Partition the $n$ indices into $k$ roughly equal **folds** (no shuffling needed — the split itself is deterministic).
2. For each fold $i \\in [0, k)$, treat fold $i$ as the **validation set** and the remaining $k-1$ folds as the **training set**.
3. Average the $k$ scores to get the final estimate.

Each fold has size $\\lfloor n / k \\rfloor$, except the last fold which absorbs any remainder. Fold $i$ covers indices $[i \\cdot f,\\; (i+1) \\cdot f)$ for $i < k-1$, and $[(k-1) \\cdot f,\\; n)$ for the last fold, where $f = \\lfloor n / k \\rfloor$.

Cross-validation is especially important when data is limited — it ensures every example is used for both training and validation exactly once.

### Your Task

Implement:
- \`train_test_split(X, y, test_ratio, seed=42)\` → (X_train, X_test, y_train, y_test)
- \`stratified_ratio(y, test_ratio)\` → fraction of positives in the test set
- \`k_fold_indices(n, k)\` → list of $k$ tuples, each containing (train_indices, val_indices)`,

	starterCode: `def train_test_split(X, y, test_ratio, seed=42):
    # Fisher-Yates shuffle with LCG(a=1664525, c=1013904223, m=2^32)
    # Split first int(n*test_ratio) as test
    n = len(X)
    return (X, [], y, [])

def stratified_ratio(y, test_ratio):
    # Fraction of positives in the test split (seed=42)
    return 0.0

def k_fold_indices(n, k):
    # Return list of k (train_indices, val_indices) tuples
    return []

X = [[i] for i in range(10)]
y = [0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
X_train, X_test, y_train, y_test = train_test_split(X, y, 0.2)
print(len(X_train), len(X_test))     # 8 2
print(len(y_train), len(y_test))     # 8 2
print(y_test)                         # [0, 0]
print(stratified_ratio(y, 0.2))       # 0.0
folds = k_fold_indices(10, 3)
print(len(folds))                     # 3
print(len(folds[0][0]), len(folds[0][1]))  # 7 3
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

def k_fold_indices(n, k):
    fold_size = n // k
    folds = []
    for i in range(k):
        if i < k - 1:
            val_idx = list(range(i * fold_size, (i + 1) * fold_size))
        else:
            val_idx = list(range(i * fold_size, n))
        train_idx = [j for j in range(n) if j not in val_idx]
        folds.append((train_idx, val_idx))
    return folds

X = [[i] for i in range(10)]
y = [0, 1, 0, 1, 0, 1, 0, 1, 0, 1]
X_train, X_test, y_train, y_test = train_test_split(X, y, 0.2)
print(len(X_train), len(X_test))
print(len(y_train), len(y_test))
print(y_test)
print(stratified_ratio(y, 0.2))
folds = k_fold_indices(10, 3)
print(len(folds))
print(len(folds[0][0]), len(folds[0][1]))
`,

	tests: [
		{
			name: "split sizes (8,2), y_test=[0,0], stratified_ratio=0.0, 3 folds, fold0=(7,3)",
			expected: "8 2\n8 2\n[0, 0]\n0.0\n3\n7 3\n",
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
		{
			name: "k_fold_indices 5-fold on 10 samples: each val has 2 items",
			code: `{{FUNC}}
folds = k_fold_indices(10, 5)
print(len(folds))
print([len(f[1]) for f in folds])`,
			expected: "5\n[2, 2, 2, 2, 2]\n",
		},
		{
			name: "k_fold_indices covers all indices exactly once as val",
			code: `{{FUNC}}
folds = k_fold_indices(7, 3)
all_val = []
for train, val in folds:
    all_val.extend(val)
print(sorted(all_val))`,
			expected: "[0, 1, 2, 3, 4, 5, 6]\n",
		},
	],
};
