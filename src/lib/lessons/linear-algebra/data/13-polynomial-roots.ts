import type { Lesson } from "../../types";

export const polynomialRoots: Lesson = {
	id: "polynomial-roots",
	title: "Polynomial Roots",
	chapterId: "numerical",
	content: `## Finding Roots of Quadratics

A **quadratic** equation $ax^2 + bx + c = 0$ has roots given by the **quadratic formula**:

$$x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$$

The expression under the square root, $\Delta = b^2 - 4ac$, is the **discriminant**:

- $\Delta > 0$ → two distinct real roots
- $\Delta = 0$ → one repeated real root
- $\Delta < 0$ → no real roots (complex)

\`\`\`python
import math

def quadratic_roots(a, b, c):
    disc = b**2 - 4*a*c
    if disc < 0:
        return None   # complex roots
    x1 = (-b - math.sqrt(disc)) / (2*a)
    x2 = (-b + math.sqrt(disc)) / (2*a)
    return sorted([round(x1, 6), round(x2, 6)])

print(quadratic_roots(1, -3, 2))   # [1.0, 2.0]  (x-1)(x-2)=0
print(quadratic_roots(1, 0, -4))   # [-2.0, 2.0] (x-2)(x+2)=0
print(quadratic_roots(1, 0, 1))    # None (complex roots)
\`\`\`

### Applications

- **Signal processing** — filter design uses polynomial roots
- **Control systems** — stability determined by root locations
- **Numerical methods** — root-finding is fundamental to scientific computing

### Your Task

Implement \`quadratic_roots(a, b, c)\` that returns a sorted list of real roots \`[x1, x2]\` rounded to 6 decimal places, or \`None\` if the roots are complex (discriminant $< 0$).`,

	starterCode: `import math

def quadratic_roots(a, b, c):
    # Return sorted [x1, x2] rounded to 6 decimal places, or None if complex
    pass

print(quadratic_roots(1, -3, 2))
print(quadratic_roots(1, 0, -4))
print(quadratic_roots(1, 0, 1))
`,

	solution: `import math

def quadratic_roots(a, b, c):
    disc = b**2 - 4*a*c
    if disc < 0:
        return None
    x1 = (-b - math.sqrt(disc)) / (2*a)
    x2 = (-b + math.sqrt(disc)) / (2*a)
    return sorted([round(x1, 6), round(x2, 6)])

print(quadratic_roots(1, -3, 2))
print(quadratic_roots(1, 0, -4))
print(quadratic_roots(1, 0, 1))
`,

	tests: [
		{
			name: "x²-3x+2=0 → [1.0, 2.0], x²-4=0 → [-2.0, 2.0], x²+1=0 → None",
			expected: "[1.0, 2.0]\n[-2.0, 2.0]\nNone\n",
		},
		{
			name: "x²-1=0 → [-1.0, 1.0]",
			code: `{{FUNC}}
print(quadratic_roots(1, 0, -1))`,
			expected: "[-1.0, 1.0]\n",
		},
		{
			name: "perfect square x²+2x+1=0 → [-1.0, -1.0]",
			code: `{{FUNC}}
print(quadratic_roots(1, 2, 1))`,
			expected: "[-1.0, -1.0]\n",
		},
		{
			name: "2x²-8=0 → [-2.0, 2.0]",
			code: `{{FUNC}}
print(quadratic_roots(2, 0, -8))`,
			expected: "[-2.0, 2.0]\n",
		},
	],
};
