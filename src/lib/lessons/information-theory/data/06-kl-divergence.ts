import type { Lesson } from "../../types";

export const klDivergence: Lesson = {
	id: "kl-divergence",
	title: "KL Divergence",
	chapterId: "divergence",
	content: `## Kullback-Leibler Divergence

**KL divergence** (also called **relative entropy**) measures how different distribution $Q$ is from a reference distribution $P$:

$$D_{KL}(P \\| Q) = \\sum_i p_i \\log_2 \\frac{p_i}{q_i}$$

Terms where $p_i = 0$ are skipped (since $0 \\log 0 = 0$).

### Critical Property: Asymmetry

$$D_{KL}(P \\| Q) \\neq D_{KL}(Q \\| P) \\text{ in general}$$

KL divergence is **not** a true distance metric — it violates symmetry and the triangle inequality. This asymmetry matters in machine learning: the direction $P \\| Q$ penalizes placing low probability mass where $P$ has high mass.

### Non-Negativity (Gibbs' Inequality)

$$D_{KL}(P \\| Q) \\geq 0$$

with equality if and only if $P = Q$ everywhere.

### Numerical Stability

When $q_i = 0$ but $p_i > 0$, $D_{KL}$ is technically infinite. In practice, add a small $\\varepsilon = 10^{-15}$ to each $q_i$ to avoid division by zero:

$$D_{KL}(P \\| Q) \\approx \\sum_{p_i > 0} p_i \\log_2 \\frac{p_i}{q_i + \\varepsilon}$$

\`\`\`python
import math

def kl_divergence(p, q):
    epsilon = 1e-15
    return sum(p[i] * math.log2(p[i] / (q[i] + epsilon))
               for i in range(len(p)) if p[i] > 0)

p = [0.8, 0.2]
q = [0.6, 0.4]
print(round(kl_divergence(p, q), 4))  # 0.132
\`\`\`

### Your Task

Implement \`kl_divergence(p, q)\`:
- Add $\\varepsilon = 10^{-15}$ to each $q_i$
- Sum only over terms where $p_i > 0$
- Return result in bits`,

	starterCode: `import math

def kl_divergence(p, q):
    # KL(P||Q) = sum(p[i] * log2(p[i] / (q[i] + 1e-15)) for p[i] > 0)
    pass

p = [0.8, 0.2]
q = [0.6, 0.4]
print(round(kl_divergence(p, q), 4))
`,

	solution: `import math

def kl_divergence(p, q):
    epsilon = 1e-15
    return sum(p[i] * math.log2(p[i] / (q[i] + epsilon))
               for i in range(len(p)) if p[i] > 0)

p = [0.8, 0.2]
q = [0.6, 0.4]
print(round(kl_divergence(p, q), 4))
`,

	tests: [
		{
			name: "round(kl_divergence([0.8,0.2],[0.6,0.4]), 4) = 0.132",
			expected: "0.132\n",
		},
		{
			name: "round(kl_divergence([0.5,0.5],[0.25,0.75]), 4) = 0.2075",
			code: `{{FUNC}}
print(round(kl_divergence([0.5, 0.5], [0.25, 0.75]), 4))`,
			expected: "0.2075\n",
		},
		{
			name: "KL is non-negative for different distributions",
			code: `{{FUNC}}
print(kl_divergence([0.7, 0.3], [0.4, 0.6]) > 0)`,
			expected: "True\n",
		},
		{
			name: "KL is asymmetric: KL(P||Q) != KL(Q||P) in general",
			code: `{{FUNC}}
p = [0.9, 0.1]
q = [0.5, 0.5]
print(round(kl_divergence(p, q), 4) != round(kl_divergence(q, p), 4))`,
			expected: "True\n",
		},
		{
			name: "round(kl_divergence([0.3,0.7],[0.6,0.4]), 4) = 0.2651",
			code: `{{FUNC}}
print(round(kl_divergence([0.3, 0.7], [0.6, 0.4]), 4))`,
			expected: "0.2651\n",
		},
	],
};
