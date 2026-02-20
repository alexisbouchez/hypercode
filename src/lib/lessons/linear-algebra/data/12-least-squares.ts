import type { Lesson } from "../../types";

export const leastSquares: Lesson = {
	id: "least-squares",
	title: "Least Squares",
	chapterId: "systems",
	content: `## Least Squares: Fitting an Overdetermined System

When you have **more equations than unknowns** (overdetermined system), there is usually no exact solution. Instead, we find the \`x\` that **minimizes the residual** \`‖Ax - b‖²\`.

### Fitting a Line

Suppose we have data points and want to fit \`y = mx + c\`:

\`\`\`python
import numpy as np

x_vals = np.array([0, 1, 2, 3, 4])
y_vals = np.array([1, 3, 5, 7, 9])  # exactly y = 2x + 1

# Build A matrix: each row is [x_i, 1]
A = np.column_stack([x_vals, np.ones(len(x_vals))])
# [[0, 1],
#  [1, 1],
#  [2, 1],
#  [3, 1],
#  [4, 1]]

coeffs, _, _, _ = np.linalg.lstsq(A, y_vals, rcond=None)
print(np.round(coeffs, 1))  # [2. 1.]  →  slope=2, intercept=1
\`\`\`

### When Data is Noisy

Even with noisy data, least squares finds the best-fit line:

\`\`\`python
y_noisy = np.array([1.1, 2.9, 5.2, 6.8, 9.1])
coeffs, _, _, _ = np.linalg.lstsq(A, y_noisy, rcond=None)
# slope ≈ 2.0, intercept ≈ 1.04 — very close!
\`\`\`

### The Normal Equations

Least squares minimizes \`‖Ax - b‖²\`. The solution satisfies the **normal equations**: \`AᵀAx = Aᵀb\`.

\`np.linalg.lstsq\` solves this via SVD — more stable than the normal equations directly.

### Your Task

Implement \`fit_line(x_vals, y_vals)\` that returns \`[slope, intercept]\` rounded to 1 decimal place.`,

	starterCode: `import numpy as np

def fit_line(x_vals, y_vals):
    # Return [slope, intercept] rounded to 1 decimal place
    pass

x = np.array([0.0, 1.0, 2.0, 3.0, 4.0])
y = np.array([1.0, 3.0, 5.0, 7.0, 9.0])
print(fit_line(x, y))
`,

	solution: `import numpy as np

def fit_line(x_vals, y_vals):
    A = np.column_stack([x_vals, np.ones(len(x_vals))])
    coeffs, _, _, _ = np.linalg.lstsq(A, y_vals, rcond=None)
    return np.round(coeffs, 1)

x = np.array([0.0, 1.0, 2.0, 3.0, 4.0])
y = np.array([1.0, 3.0, 5.0, 7.0, 9.0])
print(fit_line(x, y))
`,

	tests: [
		{
			name: "fit y = 2x + 1 exactly",
			expected: "[2. 1.]\n",
		},
		{
			name: "fit y = 3x + 0",
			code: `{{FUNC}}
x = np.array([0.0, 1.0, 2.0])
y = np.array([0.0, 3.0, 6.0])
print(fit_line(x, y))`,
			expected: "[3. 0.]\n",
		},
		{
			name: "fit y = 0x + 5 (horizontal line)",
			code: `{{FUNC}}
x = np.array([0.0, 1.0, 2.0, 3.0])
y = np.array([5.0, 5.0, 5.0, 5.0])
print(fit_line(x, y))`,
			expected: "[0. 5.]\n",
		},
		{
			name: "fit y = x + 2",
			code: `{{FUNC}}
x = np.array([1.0, 2.0, 3.0, 4.0])
y = np.array([3.0, 4.0, 5.0, 6.0])
print(fit_line(x, y))`,
			expected: "[1. 2.]\n",
		},
	],
};
