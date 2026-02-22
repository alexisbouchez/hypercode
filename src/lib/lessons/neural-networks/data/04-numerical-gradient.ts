import type { Lesson } from "../../types";

export const numericalGradient: Lesson = {
	id: "numerical-gradient",
	title: "Numerical Gradients",
	chapterId: "gradients",
	content: `## What Is a Gradient?

The **gradient** of a function $f$ at point $x$ tells us: *if I increase $x$ by a tiny amount, how much does $f(x)$ change?*

$$\\frac{df}{dx} = \\lim_{h \\to 0} \\frac{f(x+h) - f(x)}{h}$$

In neural networks, we need gradients of the **loss** with respect to every **weight**. These gradients point in the direction of steepest ascent. We move in the *opposite* direction to minimize loss.

### The Central Difference Approximation

Instead of limits, we can estimate derivatives numerically using a small step $h$:

$$\\frac{df}{dx} \\approx \\frac{f(x + h) - f(x - h)}{2h}$$

This **central difference** formula is more accurate than the one-sided version because its error is $O(h^2)$ rather than $O(h)$.

Using $h = 10^{-5}$ gives about 10 digits of precision for smooth functions.

### Why Numerical Gradients Matter

Numerical gradients are used to **verify** analytical backpropagation implementations. This is called a **gradient check**. Before trusting your backprop code, perturb each parameter and compare the numerical and analytical gradients — they should match to within $10^{-4}$.

### Your Task

Implement \`numerical_gradient(f, x, h=1e-5)\` using the central difference formula.`,

	starterCode: `def numerical_gradient(f, x, h=1e-5):
    # (f(x+h) - f(x-h)) / (2*h)
    return 0.0

print(round(numerical_gradient(lambda x: x**2, 3.0), 4))   # 6.0
print(round(numerical_gradient(lambda x: x**3, 2.0), 4))   # 12.0
print(round(numerical_gradient(lambda x: x**2, 0.0), 4))   # 0.0
`,

	solution: `def numerical_gradient(f, x, h=1e-5):
    return (f(x + h) - f(x - h)) / (2 * h)

print(round(numerical_gradient(lambda x: x**2, 3.0), 4))
print(round(numerical_gradient(lambda x: x**3, 2.0), 4))
print(round(numerical_gradient(lambda x: x**2, 0.0), 4))
`,

	tests: [
		{
			name: "gradient of x^2 and x^3",
			expected: "6.0\n12.0\n0.0\n",
		},
		{
			name: "gradient of constant is zero",
			code: `{{FUNC}}
print(round(numerical_gradient(lambda x: 5.0, 3.0), 4))`,
			expected: "0.0\n",
		},
		{
			name: "gradient of linear is slope",
			code: `{{FUNC}}
print(round(numerical_gradient(lambda x: 3*x + 7, 100.0), 4))`,
			expected: "3.0\n",
		},
	],
};
