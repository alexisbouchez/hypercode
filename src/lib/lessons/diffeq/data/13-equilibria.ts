import type { Lesson } from "../../types";

export const equilibria: Lesson = {
	id: "equilibria",
	title: "Finding Equilibria",
	chapterId: "applications",
	content: `## Finding Equilibria

An **equilibrium** (or fixed point) of \`dy/dt = f(y)\` is a value \`y*\` where the derivative is zero:

\`\`\`
f(y*) = 0
\`\`\`

At an equilibrium, the system does not change. Equilibria are the long-term destinations (or starting points) of solutions.

### Examples

| System | Equilibria |
|--------|-----------|
| \`dy/dt = -y\` | \`y* = 0\` |
| \`dy/dt = y(1 - y)\` | \`y* = 0\` and \`y* = 1\` |
| \`dy/dt = sin(y)\` | \`y* = nπ\` for all integers n |

### Numerical Root Finding: Bisection

To find equilibria numerically, we look for **sign changes** in \`f\` over an interval. Between any two points where \`f\` changes sign, there must be a zero (Intermediate Value Theorem).

For each sign change, we narrow down the root using **bisection**:

\`\`\`python
lo, hi = x0, x1
for _ in range(50):
    mid = (lo + hi) / 2
    if f(lo) * f(mid) <= 0:
        hi = mid
    else:
        lo = mid
\`\`\`

50 bisection steps gives precision ≈ interval / 2⁵⁰ ≈ 10⁻¹⁵.

### Your Task

Implement \`find_equilibria(f, a, b)\` that finds all zeros of \`f\` in \`[a, b]\` using sign changes and bisection. Return a list of equilibrium values rounded to 6 decimal places.`,

	starterCode: `def find_equilibria(f, a, b, n=1000):
    dx = (b - a) / n
    result = []
    for i in range(n):
        x0 = a + i * dx
        x1 = x0 + dx
        if f(x0) * f(x1) < 0:
            lo, hi = x0, x1
            for _ in range(50):
                mid = (lo + hi) / 2
                if f(lo) * f(mid) <= 0:
                    hi = mid
                else:
                    lo = mid
            result.append(round((lo + hi) / 2, 6))
    return result

# Linear: f(y) = y → equilibrium at 0
print(find_equilibria(lambda y: y, -1, 1))

# Logistic: f(y) = y*(1-y) → equilibria at 0 and 1
print(find_equilibria(lambda y: y * (1 - y), -0.5, 1.5))
`,

	solution: `def find_equilibria(f, a, b, n=1000):
    dx = (b - a) / n
    result = []
    for i in range(n):
        x0 = a + i * dx
        x1 = x0 + dx
        if f(x0) * f(x1) < 0:
            lo, hi = x0, x1
            for _ in range(50):
                mid = (lo + hi) / 2
                if f(lo) * f(mid) <= 0:
                    hi = mid
                else:
                    lo = mid
            result.append(round((lo + hi) / 2, 6))
    return result

print(find_equilibria(lambda y: y, -1, 1))
print(find_equilibria(lambda y: y * (1 - y), -0.5, 1.5))
`,

	tests: [
		{
			name: "linear f(y)=y has equilibrium at 0",
			code: `{{FUNC}}
print(find_equilibria(lambda y: y, -1, 1))`,
			expected: "[0.0]\n",
		},
		{
			name: "quadratic f(y)=y^2-1 has roots at ±1",
			code: `{{FUNC}}
print(find_equilibria(lambda y: y**2 - 1, -2, 2))`,
			expected: "[-1.0, 1.0]\n",
		},
		{
			name: "cubic f(y)=y(y-1)(y+1) has three roots",
			code: `{{FUNC}}
print(find_equilibria(lambda y: y * (y - 1) * (y + 1), -1.5, 1.5))`,
			expected: "[-1.0, 0.0, 1.0]\n",
		},
		{
			name: "no sign changes → empty list",
			code: `{{FUNC}}
print(find_equilibria(lambda y: y**2 + 1, -2, 2))`,
			expected: "[]\n",
		},
	],
};
