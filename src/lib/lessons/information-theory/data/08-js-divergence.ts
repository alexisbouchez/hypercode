import type { Lesson } from "../../types";

export const jsDivergence: Lesson = {
	id: "js-divergence",
	title: "Jensen-Shannon Divergence",
	chapterId: "divergence",
	content: `## Jensen-Shannon Divergence

The **Jensen-Shannon divergence** (JSD) fixes KL divergence's asymmetry by averaging the KL from each distribution to their mixture:

$$\\text{JSD}(P \\| Q) = \\frac{1}{2} D_{KL}(P \\| M) + \\frac{1}{2} D_{KL}(Q \\| M)$$

where $M = \\frac{P + Q}{2}$ is the **mixture distribution** (element-wise average).

### Key Properties

- **Symmetric**: $\\text{JSD}(P \\| Q) = \\text{JSD}(Q \\| P)$
- **Bounded**: $0 \\leq \\text{JSD} \\leq 1$ when using $\\log_2$
- **Square root is a metric**: $\\sqrt{\\text{JSD}(P, Q)}$ satisfies the triangle inequality

### Interpretation

| JSD | Meaning |
|-----|---------|
| 0 | $P = Q$ (identical distributions) |
| 1 | $P$ and $Q$ have **disjoint supports** (completely different) |

### Jensen-Shannon Distance

The square root of JSD is a proper **metric**:

$$\\text{JSD-distance}(P, Q) = \\sqrt{\\text{JSD}(P \\| Q)}$$

\`\`\`python
import math

def js_divergence(p, q):
    m = [(p[i] + q[i]) / 2 for i in range(len(p))]
    kl_pm = sum(p[i] * math.log2(p[i] / m[i]) for i in range(len(p)) if p[i] > 0)
    kl_qm = sum(q[i] * math.log2(q[i] / m[i]) for i in range(len(q)) if q[i] > 0)
    return 0.5 * kl_pm + 0.5 * kl_qm

p = [0.5, 0.5]
q = [0.25, 0.75]
print(round(js_divergence(p, q), 4))  # 0.0488
\`\`\`

### Your Task

Implement:
- \`js_divergence(p, q)\` — $0.5 \\cdot D_{KL}(P \\| M) + 0.5 \\cdot D_{KL}(Q \\| M)$ where $M = (P+Q)/2$
- \`js_distance(p, q)\` — $\\sqrt{\\text{JSD}(P \\| Q)}$`,

	starterCode: `import math

def js_divergence(p, q):
    # m = (p + q) / 2 element-wise
    # return 0.5 * KL(p||m) + 0.5 * KL(q||m)
    pass

def js_distance(p, q):
    # sqrt(js_divergence(p, q))
    pass

p = [0.5, 0.5]
q = [0.25, 0.75]
print(round(js_divergence(p, q), 4))
print(round(js_distance(p, q), 4))
`,

	solution: `import math

def js_divergence(p, q):
    m = [(p[i] + q[i]) / 2 for i in range(len(p))]
    kl_pm = sum(p[i] * math.log2(p[i] / m[i]) for i in range(len(p)) if p[i] > 0)
    kl_qm = sum(q[i] * math.log2(q[i] / m[i]) for i in range(len(q)) if q[i] > 0)
    return 0.5 * kl_pm + 0.5 * kl_qm

def js_distance(p, q):
    return math.sqrt(js_divergence(p, q))

p = [0.5, 0.5]
q = [0.25, 0.75]
print(round(js_divergence(p, q), 4))
print(round(js_distance(p, q), 4))
`,

	tests: [
		{
			name: "JSD([0.5,0.5],[0.25,0.75])=0.0488, distance=0.2209",
			expected: "0.0488\n0.2209\n",
		},
		{
			name: "js_divergence([0.5,0.5],[0.5,0.5]) = 0.0",
			code: `{{FUNC}}
print(js_divergence([0.5, 0.5], [0.5, 0.5]))`,
			expected: "0.0\n",
		},
		{
			name: "js_divergence([1.0,0.0],[0.0,1.0]) = 1.0 (max, disjoint supports)",
			code: `{{FUNC}}
print(round(js_divergence([1.0, 0.0], [0.0, 1.0]), 4))`,
			expected: "1.0\n",
		},
		{
			name: "js_distance([1.0,0.0],[0.0,1.0]) = 1.0",
			code: `{{FUNC}}
print(round(js_distance([1.0, 0.0], [0.0, 1.0]), 4))`,
			expected: "1.0\n",
		},
		{
			name: "JSD is symmetric",
			code: `{{FUNC}}
p = [0.7, 0.3]
q = [0.2, 0.8]
print(round(js_divergence(p, q), 8) == round(js_divergence(q, p), 8))`,
			expected: "True\n",
		},
	],
};
