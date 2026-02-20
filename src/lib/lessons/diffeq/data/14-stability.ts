import type { Lesson } from "../../types";

export const stability: Lesson = {
	id: "stability",
	title: "Stability Analysis",
	chapterId: "applications",
	content: `## Stability Analysis

Finding an equilibrium is only half the story. We also need to know: will the system **stay** near the equilibrium, or **drift away**?

### Linearization

For \`dy/dt = f(y)\` near an equilibrium \`y*\`:

\`\`\`
f(y) ≈ f(y*) + f'(y*) · (y - y*)
     = f'(y*) · (y - y*)      (since f(y*) = 0)
\`\`\`

The small perturbation \`δ = y - y*\` satisfies:

\`\`\`
dδ/dt = f'(y*) · δ
\`\`\`

This is just exponential growth/decay! The solution is \`δ(t) = δ(0) · e^(f'(y*) · t)\`.

### Stability Criterion

| Condition | Behavior | Classification |
|-----------|----------|----------------|
| \`f'(y*) < 0\` | Perturbations decay | **Stable** |
| \`f'(y*) > 0\` | Perturbations grow | **Unstable** |
| \`f'(y*) = 0\` | Need higher-order analysis | **Neutral** |

### Example

For logistic growth \`f(y) = y(1 - y)\`:
- \`f'(y) = 1 - 2y\`
- At \`y* = 0\`: \`f'(0) = 1 > 0\` → **unstable** (population grows away from 0)
- At \`y* = 1\`: \`f'(1) = -1 < 0\` → **stable** (population returns to carrying capacity)

### Numerical Derivative

We estimate \`f'(y*)\` using the central difference:

\`\`\`
f'(y*) ≈ (f(y* + ε) - f(y* - ε)) / (2ε)
\`\`\`

### Your Task

Implement \`stability(f, x_eq, eps=1e-5)\` that classifies an equilibrium as \`"stable"\`, \`"unstable"\`, or \`"neutral"\`.`,

	starterCode: `def stability(f, x_eq, eps=1e-5):
    df = (f(x_eq + eps) - f(x_eq - eps)) / (2 * eps)
    if df < -1e-10:
        return "stable"
    elif df > 1e-10:
        return "unstable"
    else:
        return "neutral"

# f(y) = -y: stable at 0 (df/dy = -1)
print(stability(lambda y: -y, 0))

# f(y) = y*(1-y): logistic growth
print(stability(lambda y: y * (1 - y), 0.0))   # unstable
print(stability(lambda y: y * (1 - y), 1.0))   # stable
`,

	solution: `def stability(f, x_eq, eps=1e-5):
    df = (f(x_eq + eps) - f(x_eq - eps)) / (2 * eps)
    if df < -1e-10:
        return "stable"
    elif df > 1e-10:
        return "unstable"
    else:
        return "neutral"

print(stability(lambda y: -y, 0))
print(stability(lambda y: y * (1 - y), 0.0))
print(stability(lambda y: y * (1 - y), 1.0))
`,

	tests: [
		{
			name: "f(y)=-y: stable at 0",
			code: `{{FUNC}}
print(stability(lambda y: -y, 0))`,
			expected: "stable\n",
		},
		{
			name: "f(y)=y: unstable at 0",
			code: `{{FUNC}}
print(stability(lambda y: y, 0))`,
			expected: "unstable\n",
		},
		{
			name: "logistic: unstable at y=0",
			code: `{{FUNC}}
print(stability(lambda y: y * (1 - y), 0.0))`,
			expected: "unstable\n",
		},
		{
			name: "logistic: stable at y=1 (carrying capacity)",
			code: `{{FUNC}}
print(stability(lambda y: y * (1 - y), 1.0))`,
			expected: "stable\n",
		},
	],
};
