import type { Lesson } from "../../types";

export const differentiability: Lesson = {
	id: "differentiability",
	title: "Differentiability",
	chapterId: "continuity",
	content: `## Differentiability

The derivative formalizes the idea of instantaneous rate of change.

### Definition

A function \\(f\\) is **differentiable** at \\(c\\) if the following limit exists:

$$f'(c) = \\lim_{h \\to 0} \\frac{f(c + h) - f(c)}{h}$$

### Key Facts

- If \\(f\\) is differentiable at \\(c\\), then \\(f\\) is continuous at \\(c\\)
- The converse is **false**: \\(f(x) = |x|\\) is continuous at 0 but not differentiable there

### Numerical Differentiation

We can approximate the derivative using a small \\(h\\):

\`\`\`python
def derivative(f, x, h=1e-8):
    return (f(x + h) - f(x)) / h
\`\`\`

A better approximation uses the **central difference**:

$$f'(x) \\approx \\frac{f(x + h) - f(x - h)}{2h}$$

This is more accurate because it cancels the second-order error term.

### Higher Derivatives

The second derivative can be approximated by:

$$f''(x) \\approx \\frac{f(x + h) - 2f(x) + f(x - h)}{h^2}$$

### Your Task

Implement:
1. \`derivative(f, x, h)\` -- central difference approximation: \\((f(x+h) - f(x-h)) / (2h)\\)
2. \`second_derivative(f, x, h)\` -- second derivative approximation: \\((f(x+h) - 2f(x) + f(x-h)) / h^2\\)
3. \`is_differentiable(f, x, h)\` -- checks if left and right derivatives are approximately equal (within 0.01 of each other)`,

	starterCode: `def derivative(f, x, h=1e-6):
    # Central difference: (f(x+h) - f(x-h)) / (2h)
    pass

def second_derivative(f, x, h=1e-4):
    # (f(x+h) - 2*f(x) + f(x-h)) / h^2
    pass

def is_differentiable(f, x, h=1e-6):
    # Check if left and right derivatives agree within 0.01
    pass

import math
print(round(derivative(math.sin, 0), 4))
print(round(derivative(lambda x: x**3, 2), 4))
print(round(second_derivative(math.sin, 0), 4))
print(is_differentiable(abs, 0))
`,

	solution: `def derivative(f, x, h=1e-6):
    return (f(x + h) - f(x - h)) / (2 * h)

def second_derivative(f, x, h=1e-4):
    return (f(x + h) - 2 * f(x) + f(x - h)) / h ** 2

def is_differentiable(f, x, h=1e-6):
    left = (f(x) - f(x - h)) / h
    right = (f(x + h) - f(x)) / h
    return abs(left - right) < 0.01

import math
print(round(derivative(math.sin, 0), 4))
print(round(derivative(lambda x: x**3, 2), 4))
print(round(second_derivative(math.sin, 0), 4))
print(is_differentiable(abs, 0))
`,

	tests: [
		{
			name: "derivative of sin at 0 is cos(0) = 1",
			code: `{{FUNC}}
import math
print(round(derivative(math.sin, 0), 4))`,
			expected: "1.0\n",
		},
		{
			name: "derivative of x^3 at 2 is 12",
			code: `{{FUNC}}
print(round(derivative(lambda x: x**3, 2), 4))`,
			expected: "12.0\n",
		},
		{
			name: "second derivative of sin at 0 is -sin(0) = 0",
			code: `{{FUNC}}
import math
print(round(second_derivative(math.sin, 0), 4))`,
			expected: "0.0\n",
		},
		{
			name: "|x| is not differentiable at 0",
			code: `{{FUNC}}
print(is_differentiable(abs, 0))`,
			expected: "False\n",
		},
	],
};
