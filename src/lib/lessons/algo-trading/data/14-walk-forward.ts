import type { Lesson } from "../../types";

export const walkForward: Lesson = {
  id: "walk-forward",
  title: "Walk-Forward Validation",
  chapterId: "backtesting",
  content: `## Walk-Forward Validation

Walk-forward validation is a more rigorous backtesting technique that prevents overfitting. Instead of optimizing and testing on the same data, you repeatedly train on a historical window and test on the immediately following period.

### Algorithm

Given \`n\` total data points, a training window of size \`n_train\`, and a test window of size \`n_test\`:

\`\`\`
start = 0
while start + n_train + n_test <= n:
    train: [start, start + n_train)
    test:  [start + n_train, start + n_train + n_test)
    start += n_test   # walk forward by one test period
\`\`\`

Each split is represented as \`(train_start, train_end, test_start, test_end)\`.

### Example

\`walk_forward_splits(20, 10, 5)\` produces:
- \`(0, 10, 10, 15)\`  — train on 0–9, test on 10–14
- \`(5, 15, 15, 20)\`  — train on 5–14, test on 15–19

### Task

Implement \`walk_forward_splits(n, n_train, n_test)\` that returns a list of tuples.`,
  starterCode: `def walk_forward_splits(n, n_train, n_test):
    splits = []
    start = 0
    # TODO: generate (train_start, train_end, test_start, test_end) tuples
    return splits`,
  solution: `def walk_forward_splits(n, n_train, n_test):
    splits = []
    start = 0
    while start + n_train + n_test <= n:
        train_start = start
        train_end = start + n_train
        test_start = train_end
        test_end = test_start + n_test
        splits.append((train_start, train_end, test_start, test_end))
        start += n_test
    return splits`,
  tests: [
    {
      name: "walk_forward_splits(20, 10, 5) — produces 2 splits",
      code: `{{FUNC}}
result = walk_forward_splits(20, 10, 5)
print(len(result))`,
      expected: "2\n",
    },
    {
      name: "walk_forward_splits(20, 10, 5) — first split",
      code: `{{FUNC}}
result = walk_forward_splits(20, 10, 5)
print(result[0])`,
      expected: "(0, 10, 10, 15)\n",
    },
    {
      name: "walk_forward_splits(20, 10, 5) — second split",
      code: `{{FUNC}}
result = walk_forward_splits(20, 10, 5)
print(result[1])`,
      expected: "(5, 15, 15, 20)\n",
    },
    {
      name: "walk_forward_splits(30, 10, 5) — produces 4 splits",
      code: `{{FUNC}}
result = walk_forward_splits(30, 10, 5)
print(len(result))`,
      expected: "4\n",
    },
  ],
};
