import type { Lesson } from "../../types";

export const jointEntropy: Lesson = {
	id: "joint-entropy",
	title: "Joint Entropy",
	chapterId: "entropy",
	content: `## Joint Entropy

**Joint entropy** $H(X, Y)$ measures the total uncertainty in a pair of random variables $(X, Y)$ together.

$$H(X, Y) = -\\sum_{x,y} p(x,y) \\log_2 p(x,y)$$

This is just Shannon entropy applied to the **joint distribution** — flatten the probability matrix and sum over all cells.

### Marginal Distributions

From a joint distribution $p(x, y)$, you can recover each variable's distribution by **summing out** the other:

$$p(x) = \\sum_y p(x, y) \\qquad p(y) = \\sum_x p(x, y)$$

In matrix notation: marginal of $X$ = row sums, marginal of $Y$ = column sums.

### Key Bound

$$\\max(H(X), H(Y)) \\leq H(X, Y) \\leq H(X) + H(Y)$$

- The lower bound holds when one variable **determines** the other.
- The upper bound holds when $X$ and $Y$ are **independent**: $p(x, y) = p(x)\\,p(y)$.

### Example

For a uniform $2 \\times 2$ joint distribution (each cell $= 0.25$):
$$H(X, Y) = -4 \\cdot 0.25 \\log_2 0.25 = 2 \\text{ bits}$$

\`\`\`python
import math

def joint_entropy(joint_probs):
    result = 0.0
    for row in joint_probs:
        for p in row:
            if p > 0:
                result += -p * math.log2(p)
    return result

j = [[0.25, 0.25], [0.25, 0.25]]
print(joint_entropy(j))   # 2.0
\`\`\`

### Your Task

Implement:
- \`joint_entropy(joint_probs)\` — entropy of a 2D list (matrix of joint probabilities)
- \`marginal_x(joint)\` — list of row sums $p(x)$
- \`marginal_y(joint)\` — list of column sums $p(y)$`,

	starterCode: `import math

def joint_entropy(joint_probs):
    # Flatten the matrix and compute entropy
    pass

def marginal_x(joint):
    # Return list of row sums
    pass

def marginal_y(joint):
    # Return list of column sums
    pass

j = [[0.25, 0.25], [0.25, 0.25]]
print(joint_entropy(j))
print(marginal_x(j))
print(marginal_y(j))
`,

	solution: `import math

def joint_entropy(joint_probs):
    result = 0.0
    for row in joint_probs:
        for p in row:
            if p > 0:
                result += -p * math.log2(p)
    return result

def marginal_x(joint):
    return [sum(row) for row in joint]

def marginal_y(joint):
    n_cols = len(joint[0])
    return [sum(joint[i][j] for i in range(len(joint))) for j in range(n_cols)]

j = [[0.25, 0.25], [0.25, 0.25]]
print(joint_entropy(j))
print(marginal_x(j))
print(marginal_y(j))
`,

	tests: [
		{
			name: "uniform 2x2: H(X,Y)=2.0, marginals=[0.5,0.5]",
			expected: "2.0\n[0.5, 0.5]\n[0.5, 0.5]\n",
		},
		{
			name: "joint_entropy([[0.5,0.0],[0.0,0.5]]) = 1.0",
			code: `{{FUNC}}
print(joint_entropy([[0.5, 0.0], [0.0, 0.5]]))`,
			expected: "1.0\n",
		},
		{
			name: "marginal_x([[0.5,0.0],[0.0,0.5]]) = [0.5, 0.5]",
			code: `{{FUNC}}
print(marginal_x([[0.5, 0.0], [0.0, 0.5]]))`,
			expected: "[0.5, 0.5]\n",
		},
		{
			name: "marginal_y([[0.5,0.0],[0.0,0.5]]) = [0.5, 0.5]",
			code: `{{FUNC}}
print(marginal_y([[0.5, 0.0], [0.0, 0.5]]))`,
			expected: "[0.5, 0.5]\n",
		},
		{
			name: "round(joint_entropy([[0.3,0.1],[0.2,0.4]]), 4) = 1.8464",
			code: `{{FUNC}}
print(round(joint_entropy([[0.3, 0.1], [0.2, 0.4]]), 4))`,
			expected: "1.8464\n",
		},
	],
};
