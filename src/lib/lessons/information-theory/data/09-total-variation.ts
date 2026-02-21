import type { Lesson } from "../../types";

export const totalVariation: Lesson = {
	id: "total-variation",
	title: "Total Variation and Hellinger Distance",
	chapterId: "divergence",
	content: `## Total Variation Distance

The **total variation (TV) distance** is the maximum possible difference in probability assigned to any event:

$$\\text{TV}(P, Q) = \\frac{1}{2} \\sum_i |p_i - q_i|$$

The factor of $1/2$ ensures TV $\\in [0, 1]$.

### Properties

- **Metric**: TV satisfies symmetry, non-negativity, and triangle inequality
- **Bounded**: $0 \\leq \\text{TV}(P, Q) \\leq 1$
- **TV = 0** iff $P = Q$; **TV = 1** iff $P$ and $Q$ have disjoint supports

### Hellinger Distance

The **Hellinger distance** is another probability metric based on the $L^2$ norm of square-root probabilities:

$$H(P, Q) = \\sqrt{\\frac{1}{2} \\sum_i \\left(\\sqrt{p_i} - \\sqrt{q_i}\\right)^2}$$

It is also bounded in $[0, 1]$ and has useful relationships to both TV distance and KL divergence.

### Comparison of Distances

| Distance | Formula | Range |
|----------|---------|-------|
| Total Variation | $\\frac{1}{2}\\sum|p_i - q_i|$ | $[0, 1]$ |
| Hellinger | $\\sqrt{\\frac{1}{2}\\sum(\\sqrt{p_i}-\\sqrt{q_i})^2}$ | $[0, 1]$ |
| JS Distance | $\\sqrt{\\text{JSD}(P,Q)}$ | $[0, 1]$ |

\`\`\`python
import math

def total_variation(p, q):
    return 0.5 * sum(abs(p[i] - q[i]) for i in range(len(p)))

def hellinger_distance(p, q):
    return math.sqrt(0.5 * sum((math.sqrt(p[i]) - math.sqrt(q[i]))**2
                                for i in range(len(p))))

p = [0.7, 0.3]
q = [0.4, 0.6]
print(round(total_variation(p, q), 4))    # 0.3
print(round(hellinger_distance(p, q), 4)) # 0.2158
\`\`\`

### Your Task

Implement:
- \`total_variation(p, q)\` — $\\frac{1}{2} \\sum_i |p_i - q_i|$
- \`hellinger_distance(p, q)\` — $\\sqrt{\\frac{1}{2} \\sum_i (\\sqrt{p_i} - \\sqrt{q_i})^2}$`,

	starterCode: `import math

def total_variation(p, q):
    # 0.5 * sum(|p[i] - q[i]|)
    pass

def hellinger_distance(p, q):
    # sqrt(0.5 * sum((sqrt(p[i]) - sqrt(q[i]))^2))
    pass

p = [0.7, 0.3]
q = [0.4, 0.6]
print(round(total_variation(p, q), 4))
print(round(hellinger_distance(p, q), 4))
`,

	solution: `import math

def total_variation(p, q):
    return 0.5 * sum(abs(p[i] - q[i]) for i in range(len(p)))

def hellinger_distance(p, q):
    return math.sqrt(0.5 * sum((math.sqrt(p[i]) - math.sqrt(q[i]))**2
                                for i in range(len(p))))

p = [0.7, 0.3]
q = [0.4, 0.6]
print(round(total_variation(p, q), 4))
print(round(hellinger_distance(p, q), 4))
`,

	tests: [
		{
			name: "TV([0.7,0.3],[0.4,0.6])=0.3, Hellinger=0.2158",
			expected: "0.3\n0.2158\n",
		},
		{
			name: "total_variation([0.5,0.5],[0.5,0.5]) = 0.0",
			code: `{{FUNC}}
print(total_variation([0.5, 0.5], [0.5, 0.5]))`,
			expected: "0.0\n",
		},
		{
			name: "total_variation([1.0,0.0],[0.0,1.0]) = 1.0",
			code: `{{FUNC}}
print(total_variation([1.0, 0.0], [0.0, 1.0]))`,
			expected: "1.0\n",
		},
		{
			name: "hellinger_distance([0.5,0.5],[0.5,0.5]) = 0.0",
			code: `{{FUNC}}
print(hellinger_distance([0.5, 0.5], [0.5, 0.5]))`,
			expected: "0.0\n",
		},
		{
			name: "hellinger_distance([1.0,0.0],[0.0,1.0]) = 1.0",
			code: `{{FUNC}}
print(round(hellinger_distance([1.0, 0.0], [0.0, 1.0]), 4))`,
			expected: "1.0\n",
		},
	],
};
