import type { Lesson } from "../../types";

export const leastSquares: Lesson = {
	id: "least-squares",
	title: "Least Squares",
	chapterId: "systems",
	content: `## Least Squares: Fitting a Line

When you have data points and want to fit a line \`y = mx + c\`, the **least squares** method finds the slope and intercept that minimize the sum of squared residuals.

### Normal Equations

The slope and intercept can be computed directly using the **normal equations**:

\`\`\`python
def fit_line(x_vals, y_vals):
    n = len(x_vals)
    sx = sum(x_vals)
    sy = sum(y_vals)
    sxy = sum(x*y for x, y in zip(x_vals, y_vals))
    sxx = sum(x**2 for x in x_vals)

    slope = (n*sxy - sx*sy) / (n*sxx - sx**2)
    intercept = (sy - slope*sx) / n
    return [round(slope, 1), round(intercept, 1)]

x = [0, 1, 2, 3, 4]
y = [1, 3, 5, 7, 9]  # y = 2x + 1

print(fit_line(x, y))  # [2.0, 1.0]
\`\`\`

### When Data is Noisy

Even with noisy data, least squares finds the best-fit line — the one that minimizes the total vertical distance (squared) from the points to the line.

### The Normal Equations (Matrix Form)

For overdetermined systems with matrix \`A\` and vector \`b\`, least squares minimizes \`‖Ax - b‖²\`. The solution satisfies: \`AᵀAx = Aᵀb\`.

### Your Task

Implement \`fit_line(x_vals, y_vals)\` that returns \`[slope, intercept]\` rounded to 1 decimal place.`,

	starterCode: `def fit_line(x_vals, y_vals):
    # Return [slope, intercept] rounded to 1 decimal place
    pass

x = [0, 1, 2, 3, 4]
y = [1, 3, 5, 7, 9]
print(fit_line(x, y))
`,

	solution: `def fit_line(x_vals, y_vals):
    n = len(x_vals)
    sx = sum(x_vals)
    sy = sum(y_vals)
    sxy = sum(x*y for x, y in zip(x_vals, y_vals))
    sxx = sum(x**2 for x in x_vals)
    slope = (n*sxy - sx*sy) / (n*sxx - sx**2)
    intercept = (sy - slope*sx) / n
    return [round(slope, 1), round(intercept, 1)]

x = [0, 1, 2, 3, 4]
y = [1, 3, 5, 7, 9]
print(fit_line(x, y))
`,

	tests: [
		{
			name: "fit y = 2x + 1 exactly",
			expected: "[2.0, 1.0]\n",
		},
		{
			name: "fit y = 3x + 0",
			code: `{{FUNC}}
x = [0, 1, 2]
y = [0, 3, 6]
print(fit_line(x, y))`,
			expected: "[3.0, 0.0]\n",
		},
		{
			name: "fit y = 0x + 5 (horizontal line)",
			code: `{{FUNC}}
x = [0, 1, 2, 3]
y = [5, 5, 5, 5]
print(fit_line(x, y))`,
			expected: "[0.0, 5.0]\n",
		},
		{
			name: "fit y = x + 2",
			code: `{{FUNC}}
x = [1, 2, 3, 4]
y = [3, 4, 5, 6]
print(fit_line(x, y))`,
			expected: "[1.0, 2.0]\n",
		},
	],
};
