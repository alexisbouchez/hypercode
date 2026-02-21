import type { Lesson } from "../../types";

export const conditionalEntropy: Lesson = {
	id: "conditional-entropy",
	title: "Conditional Entropy",
	chapterId: "entropy",
	content: `## Conditional Entropy

**Conditional entropy** $H(Y | X)$ measures the average uncertainty remaining in $Y$ **after** you observe $X$.

### Chain Rule

The most useful formula uses the chain rule of entropy:

$$H(Y | X) = H(X, Y) - H(X)$$
$$H(X | Y) = H(X, Y) - H(Y)$$

These hold because:
- Knowing $X$ perfectly predicts $Y$ ⟹ $H(Y|X) = 0$
- Independent $X$ and $Y$ ⟹ $H(Y|X) = H(Y)$ (knowing $X$ tells you nothing about $Y$)

### Key Inequalities

$$0 \\leq H(Y|X) \\leq H(Y)$$

- **Lower bound 0**: $Y$ is determined by $X$ (deterministic channel)
- **Upper bound $H(Y)$**: $X$ and $Y$ are independent

### Example

If $X$ and $Y$ are independent and uniform over 2 values each:
$$H(Y|X) = H(X,Y) - H(X) = 2 - 1 = 1 \\text{ bit}$$

If $Y = X$ deterministically:
$$H(Y|X) = H(X,Y) - H(X) = 1 - 1 = 0 \\text{ bits}$$

\`\`\`python
import math

def shannon_entropy(probs):
    return sum(-p * math.log2(p) for p in probs if p > 0)

def joint_entropy(joint_probs):
    result = 0.0
    for row in joint_probs:
        for p in row:
            if p > 0:
                result += -p * math.log2(p)
    return result

def conditional_entropy_yx(joint):
    # H(Y|X) = H(X,Y) - H(X)
    hxy = joint_entropy(joint)
    hx = shannon_entropy([sum(row) for row in joint])
    return hxy - hx
\`\`\`

### Your Task

Implement:
- \`conditional_entropy_yx(joint)\` — $H(Y|X) = H(X,Y) - H(X)$
- \`conditional_entropy_xy(joint)\` — $H(X|Y) = H(X,Y) - H(Y)$

Both take a 2D list of joint probabilities.`,

	starterCode: `import math

def shannon_entropy(probs):
    return sum(-p * math.log2(p) for p in probs if p > 0)

def joint_entropy(joint_probs):
    result = 0.0
    for row in joint_probs:
        for p in row:
            if p > 0:
                result += -p * math.log2(p)
    return result

def conditional_entropy_yx(joint):
    # H(Y|X) = H(X,Y) - H(X)
    pass

def conditional_entropy_xy(joint):
    # H(X|Y) = H(X,Y) - H(Y)
    pass

j = [[0.25, 0.25], [0.25, 0.25]]
print(conditional_entropy_yx(j))
print(conditional_entropy_xy(j))
`,

	solution: `import math

def shannon_entropy(probs):
    return sum(-p * math.log2(p) for p in probs if p > 0)

def joint_entropy(joint_probs):
    result = 0.0
    for row in joint_probs:
        for p in row:
            if p > 0:
                result += -p * math.log2(p)
    return result

def conditional_entropy_yx(joint):
    hxy = joint_entropy(joint)
    hx = shannon_entropy([sum(row) for row in joint])
    return hxy - hx

def conditional_entropy_xy(joint):
    hxy = joint_entropy(joint)
    n_cols = len(joint[0])
    hy = shannon_entropy([sum(joint[i][j] for i in range(len(joint))) for j in range(n_cols)])
    return hxy - hy

j = [[0.25, 0.25], [0.25, 0.25]]
print(conditional_entropy_yx(j))
print(conditional_entropy_xy(j))
`,

	tests: [
		{
			name: "independent 2x2: H(Y|X)=1.0, H(X|Y)=1.0",
			expected: "1.0\n1.0\n",
		},
		{
			name: "H(Y|X) deterministic [[0.5,0],[0,0.5]] = 0.0",
			code: `{{FUNC}}
print(conditional_entropy_yx([[0.5, 0.0], [0.0, 0.5]]))`,
			expected: "0.0\n",
		},
		{
			name: "H(X|Y) deterministic [[0.5,0],[0,0.5]] = 0.0",
			code: `{{FUNC}}
print(conditional_entropy_xy([[0.5, 0.0], [0.0, 0.5]]))`,
			expected: "0.0\n",
		},
		{
			name: "H(Y|X) of uniform 2x2 = H(Y) (independence)",
			code: `{{FUNC}}
j = [[0.25, 0.25], [0.25, 0.25]]
print(conditional_entropy_yx(j) == 1.0)`,
			expected: "True\n",
		},
		{
			name: "H(X|Y) is non-negative",
			code: `{{FUNC}}
import random
j = [[0.3, 0.1], [0.2, 0.4]]
result = conditional_entropy_xy(j)
print(result >= 0)`,
			expected: "True\n",
		},
	],
};
