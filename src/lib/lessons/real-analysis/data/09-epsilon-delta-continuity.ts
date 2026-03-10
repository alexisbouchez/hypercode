import type { Lesson } from "../../types";

export const epsilonDeltaContinuity: Lesson = {
	id: "epsilon-delta-continuity",
	title: "Epsilon-Delta Continuity",
	chapterId: "continuity",
	content: `## Epsilon-Delta Definition of Continuity

Continuity is the second fundamental concept in real analysis, defined similarly to limits.

### Definition

A function \\(f\\) is **continuous at** \\(c\\) if for every \\(\\varepsilon > 0\\), there exists \\(\\delta > 0\\) such that:

$$|x - c| < \\delta \\implies |f(x) - f(c)| < \\varepsilon$$

### Intuition

Small changes in the input produce small changes in the output. The function has no "jumps" at \\(c\\).

### Finding Delta

For \\(f(x) = 2x\\) at \\(c = 3\\) with \\(\\varepsilon = 0.1\\):
- We need \\(|2x - 6| < 0.1\\), which means \\(2|x - 3| < 0.1\\)
- So \\(|x - 3| < 0.05\\), meaning \\(\\delta = 0.05 = \\varepsilon / 2\\)

### Numerical Verification

We can check continuity by sampling points in a \\(\\delta\\)-neighborhood:

\`\`\`python
def check_continuity(f, c, epsilon, delta, samples=100):
    import random
    for _ in range(samples):
        x = c + (2 * random.random() - 1) * delta
        if abs(f(x) - f(c)) >= epsilon:
            return False
    return True
\`\`\`

### Your Task

Implement:
1. \`check_continuity(f, c, epsilon, delta, num_points)\` -- checks if \\(|f(x) - f(c)| < \\varepsilon\\) for \`num_points\` evenly spaced points in \\((c - \\delta, c + \\delta)\\)
2. \`find_delta(f, c, epsilon)\` -- finds the largest \\(\\delta\\) from the list \\([1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001]\\) such that continuity holds (using 200 sample points)`,

	starterCode: `def check_continuity(f, c, epsilon, delta, num_points):
    # Check |f(x) - f(c)| < epsilon for num_points evenly spaced
    # points in (c - delta, c + delta)
    pass

def find_delta(f, c, epsilon):
    # Find largest delta from candidates that satisfies continuity
    pass

linear = lambda x: 2 * x
quadratic = lambda x: x ** 2

print(check_continuity(linear, 3, 0.1, 0.05, 200))
print(check_continuity(linear, 3, 0.1, 0.1, 200))
print(find_delta(quadratic, 2, 0.1))
`,

	solution: `def check_continuity(f, c, epsilon, delta, num_points):
    for i in range(num_points):
        x = c - delta + (2 * delta) * (i + 1) / (num_points + 1)
        if abs(f(x) - f(c)) >= epsilon:
            return False
    return True

def find_delta(f, c, epsilon):
    candidates = [1, 0.5, 0.1, 0.05, 0.01, 0.005, 0.001]
    for d in candidates:
        if check_continuity(f, c, epsilon, d, 200):
            return d
    return 0

linear = lambda x: 2 * x
quadratic = lambda x: x ** 2

print(check_continuity(linear, 3, 0.1, 0.05, 200))
print(check_continuity(linear, 3, 0.1, 0.1, 200))
print(find_delta(quadratic, 2, 0.1))
`,

	tests: [
		{
			name: "linear function is continuous with tight delta",
			code: `{{FUNC}}
linear = lambda x: 2 * x
print(check_continuity(linear, 3, 0.1, 0.05, 200))`,
			expected: "True\n",
		},
		{
			name: "linear function fails with too-large delta",
			code: `{{FUNC}}
linear = lambda x: 2 * x
print(check_continuity(linear, 3, 0.1, 0.1, 200))`,
			expected: "False\n",
		},
		{
			name: "find_delta for x^2 at x=2 with epsilon=0.1",
			code: `{{FUNC}}
quadratic = lambda x: x ** 2
print(find_delta(quadratic, 2, 0.1))`,
			expected: "0.01\n",
		},
	],
};
