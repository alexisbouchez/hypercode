import type { Lesson } from "../../types";

export const entropy: Lesson = {
	id: "entropy",
	title: "Shannon Entropy",
	chapterId: "entropy",
	content: `## Shannon Entropy

**Shannon entropy** measures the average uncertainty in a probability distribution. Given a discrete random variable $X$ with probability mass function $p(x)$:

$$H(X) = -\\sum_{x} p(x) \\log_2 p(x) \\quad \\text{bits}$$

(Terms where $p(x) = 0$ are defined as $0 \\cdot \\log_2 0 = 0$ by convention.)

### Key Properties

| Distribution | Entropy |
|---|---|
| Deterministic ($p=1$ for one outcome) | $H = 0$ |
| Uniform over $n$ outcomes | $H = \\log_2 n$ (maximum) |

### Uniform Entropy

For a uniform distribution over $n$ equally likely outcomes:

$$H_{\\text{uniform}}(n) = \\log_2 n$$

This is the **maximum possible entropy** for any distribution over $n$ outcomes — uniform distributions are the most uncertain.

### Example

For a fair coin ($p = 0.5$ each): $H = -(0.5 \\log_2 0.5 + 0.5 \\log_2 0.5) = 1$ bit.

For a biased coin ($p = 0.9$, $1-p = 0.1$): $H \\approx 0.469$ bits (less uncertainty).

\`\`\`python
import math

def shannon_entropy(probs):
    return sum(-p * math.log2(p) for p in probs if p > 0)

print(shannon_entropy([0.5, 0.5]))  # 1.0
print(shannon_entropy([0.25]*4))    # 2.0
\`\`\`

### Your Task

Implement:
- \`shannon_entropy(probs)\` — $H(X)$ for a list of probabilities (skip zeros)
- \`uniform_entropy(n)\` — $\\log_2 n$
- \`max_entropy_dist(n)\` — returns $[1/n, 1/n, \\ldots]$ (list of $n$ equal probs)`,

	starterCode: `import math

def shannon_entropy(probs):
    # Return -sum(p * log2(p)) for p > 0
    pass

def uniform_entropy(n):
    # Return log2(n)
    pass

def max_entropy_dist(n):
    # Return [1/n] * n
    pass

print(shannon_entropy([0.5, 0.5]))
print(shannon_entropy([0.25]*4))
print(uniform_entropy(4))
`,

	solution: `import math

def shannon_entropy(probs):
    return sum(-p * math.log2(p) for p in probs if p > 0)

def uniform_entropy(n):
    return math.log2(n)

def max_entropy_dist(n):
    return [1/n] * n

print(shannon_entropy([0.5, 0.5]))
print(shannon_entropy([0.25]*4))
print(uniform_entropy(4))
`,

	tests: [
		{
			name: "shannon_entropy([0.5,0.5])=1.0, [0.25]*4=2.0, uniform_entropy(4)=2.0",
			expected: "1.0\n2.0\n2.0\n",
		},
		{
			name: "shannon_entropy([1.0]) = 0.0 (deterministic)",
			code: `{{FUNC}}
print(shannon_entropy([1.0]))`,
			expected: "0.0\n",
		},
		{
			name: "uniform_entropy(8) = 3.0",
			code: `{{FUNC}}
print(uniform_entropy(8))`,
			expected: "3.0\n",
		},
		{
			name: "max_entropy_dist(2) = [0.5, 0.5]",
			code: `{{FUNC}}
print(max_entropy_dist(2))`,
			expected: "[0.5, 0.5]\n",
		},
		{
			name: "round(shannon_entropy([0.7, 0.3]), 4) = 0.8813",
			code: `{{FUNC}}
print(round(shannon_entropy([0.7, 0.3]), 4))`,
			expected: "0.8813\n",
		},
	],
};
