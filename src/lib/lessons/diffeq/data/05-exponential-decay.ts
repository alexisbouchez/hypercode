import type { Lesson } from "../../types";

export const exponentialDecay: Lesson = {
	id: "exponential-decay",
	title: "Exponential Decay",
	chapterId: "first-order-models",
	content: `## Exponential Decay

The simplest real-world ODE:

$$\\frac{dy}{dt} = -k \\cdot y, \\quad y(0) = y_0$$

The rate of change is proportional to the current value. The exact solution is:

$$y(t) = y_0 \\cdot e^{-kt}$$

### Applications

- **Radioactive decay**: atoms decay at a rate proportional to their count
- **Drug clearance**: concentration in blood decreases proportionally
- **Capacitor discharge**: voltage drops exponentially
- **Population decline**: a species dying off at a fixed rate

### Half-Life

The **half-life** is the time for $y$ to reach half its initial value:

$$\\frac{y_0}{2} = y_0 \\cdot e^{-k \\cdot t_{\\text{half}}} \\implies t_{\\text{half}} = \\frac{\\ln 2}{k} \\approx \\frac{0.693}{k}$$

### Numerical Solution

Using Euler's method with $n$ steps:

\`\`\`python
def exponential_decay(k, y0, t_end, n):
    h = t_end / n
    y = float(y0)
    for _ in range(n):
        y = y + h * (-k * y)
    return y
\`\`\`

### Your Task

Implement \`exponential_decay(k, y0, t_end, n)\` that numerically solves $\\frac{dy}{dt} = -k \\cdot y$ and returns the final value $y(t_{\\text{end}})$.`,

	starterCode: `def exponential_decay(k, y0, t_end, n):
    h = t_end / n
    y = float(y0)
    for _ in range(n):
        y = y + h * (-k * y)
    return y

# k=1, y0=1, t=1: exact answer is e^(-1) ≈ 0.3679
print(round(exponential_decay(1, 1.0, 1.0, 10000), 4))

# k = ln(2): half-life = 1, so y(1) = 50
print(round(exponential_decay(0.693147, 100.0, 1.0, 100000), 1))
`,

	solution: `def exponential_decay(k, y0, t_end, n):
    h = t_end / n
    y = float(y0)
    for _ in range(n):
        y = y + h * (-k * y)
    return y

print(round(exponential_decay(1, 1.0, 1.0, 10000), 4))
print(round(exponential_decay(0.693147, 100.0, 1.0, 100000), 1))
`,

	tests: [
		{
			name: "k=0 means no decay",
			code: `{{FUNC}}
print(round(exponential_decay(0, 100.0, 10.0, 1000), 1))`,
			expected: "100.0\n",
		},
		{
			name: "approximates e^(-1) for k=1, t=1",
			code: `{{FUNC}}
print(round(exponential_decay(1, 1.0, 1.0, 10000), 4))`,
			expected: "0.3679\n",
		},
		{
			name: "large t brings value near zero",
			code: `{{FUNC}}
print(exponential_decay(1, 100.0, 100.0, 10000) < 1)`,
			expected: "True\n",
		},
		{
			name: "half-life: k=ln(2) halves y in t=1",
			code: `{{FUNC}}
print(round(exponential_decay(0.693147, 100.0, 1.0, 100000), 1))`,
			expected: "50.0\n",
		},
	],
};
