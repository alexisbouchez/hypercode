import type { Lesson } from "../../types";

export const channelMatrix: Lesson = {
	id: "channel-matrix",
	title: "Channel Matrix and Mutual Information",
	chapterId: "channel",
	content: `## Channel Matrix

A **discrete memoryless channel** is fully described by its **channel matrix** (also called transition matrix or stochastic matrix):

$$W_{ij} = P(Y = j \\mid X = i)$$

Row $i$ gives the conditional distribution of output $Y$ given input $X = i$. Each row sums to 1.

### Computing Output Probabilities

Given input distribution $p(X)$ and channel matrix $W$, the **marginal output distribution** is:

$$P(Y = j) = \\sum_i P(X = i) \\cdot W_{ij}$$

In matrix form: $\\mathbf{p}_Y = \\mathbf{p}_X \\cdot W$.

### Mutual Information Through a Channel

The mutual information between input and output is:

$$I(X; Y) = H(X) + H(Y) - H(X, Y)$$

The joint distribution needed for $H(X, Y)$ is:

$$P(X = i, Y = j) = P(X = i) \\cdot W_{ij}$$

### Examples

**Noiseless (identity) channel**: $W = I$ (identity matrix) → $I(X; Y) = H(X)$, no information lost.

**Completely noisy**: each row of $W$ is identical → output is independent of input → $I(X; Y) = 0$.

\`\`\`python
import math

def channel_output_probs(input_probs, channel_matrix):
    n_in = len(input_probs)
    n_out = len(channel_matrix[0])
    return [sum(input_probs[i] * channel_matrix[i][j]
                for i in range(n_in)) for j in range(n_out)]

identity = [[1.0, 0.0], [0.0, 1.0]]
print(channel_output_probs([0.5, 0.5], identity))  # [0.5, 0.5]
\`\`\`

### Your Task

Implement:
- \`channel_output_probs(input_probs, channel_matrix)\` — $P(Y=j) = \\sum_i p_i W_{ij}$
- \`channel_mutual_info(input_probs, channel_matrix)\` — $I(X; Y)$ using the joint distribution`,

	starterCode: `import math

def channel_output_probs(input_probs, channel_matrix):
    # P(Y=j) = sum(input_probs[i] * channel_matrix[i][j])
    pass

def channel_mutual_info(input_probs, channel_matrix):
    # Build joint P(X=i,Y=j) = input_probs[i] * channel_matrix[i][j]
    # I(X;Y) = H(X) + H(Y) - H(X,Y)
    pass

identity = [[1.0, 0.0], [0.0, 1.0]]
print(channel_output_probs([0.5, 0.5], identity))
print(channel_mutual_info([0.5, 0.5], identity))
`,

	solution: `import math

def channel_output_probs(input_probs, channel_matrix):
    n_in = len(input_probs)
    n_out = len(channel_matrix[0])
    return [sum(input_probs[i] * channel_matrix[i][j]
                for i in range(n_in)) for j in range(n_out)]

def channel_mutual_info(input_probs, channel_matrix):
    output_probs = channel_output_probs(input_probs, channel_matrix)
    n_in = len(input_probs)
    n_out = len(channel_matrix[0])
    joint = [[input_probs[i] * channel_matrix[i][j]
              for j in range(n_out)] for i in range(n_in)]
    hx = sum(-p * math.log2(p) for p in input_probs if p > 0)
    hy = sum(-p * math.log2(p) for p in output_probs if p > 0)
    hxy = sum(-p * math.log2(p) for row in joint for p in row if p > 0)
    return hx + hy - hxy

identity = [[1.0, 0.0], [0.0, 1.0]]
print(channel_output_probs([0.5, 0.5], identity))
print(channel_mutual_info([0.5, 0.5], identity))
`,

	tests: [
		{
			name: "identity channel: output=[0.5,0.5], MI=1.0",
			expected: "[0.5, 0.5]\n1.0\n",
		},
		{
			name: "fully noisy channel: MI=0.0",
			code: `{{FUNC}}
confused = [[0.5, 0.5], [0.5, 0.5]]
print(channel_mutual_info([0.5, 0.5], confused))`,
			expected: "0.0\n",
		},
		{
			name: "BSC(0.1) with uniform input: MI=0.531",
			code: `{{FUNC}}
bsc = [[0.9, 0.1], [0.1, 0.9]]
print(round(channel_mutual_info([0.5, 0.5], bsc), 4))`,
			expected: "0.531\n",
		},
		{
			name: "channel_output_probs: BSC uniform input -> [0.5, 0.5]",
			code: `{{FUNC}}
bsc = [[0.9, 0.1], [0.1, 0.9]]
print(channel_output_probs([0.5, 0.5], bsc))`,
			expected: "[0.5, 0.5]\n",
		},
		{
			name: "MI(identity channel) = H(X)",
			code: `{{FUNC}}
import math
identity = [[1.0, 0.0], [0.0, 1.0]]
p = [0.7, 0.3]
mi = channel_mutual_info(p, identity)
hx = sum(-x * math.log2(x) for x in p if x > 0)
print(round(mi, 8) == round(hx, 8))`,
			expected: "True\n",
		},
	],
};
