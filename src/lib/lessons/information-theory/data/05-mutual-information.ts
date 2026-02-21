import type { Lesson } from "../../types";

export const mutualInformation: Lesson = {
	id: "mutual-information",
	title: "Mutual Information",
	chapterId: "entropy",
	content: `## Mutual Information

**Mutual information** $I(X; Y)$ measures how much knowing one variable reduces uncertainty about the other.

$$I(X; Y) = H(X) + H(Y) - H(X, Y)$$

Equivalently:
$$I(X; Y) = H(X) - H(X|Y) = H(Y) - H(Y|X)$$

### Properties

- **Non-negative**: $I(X; Y) \\geq 0$ always
- **Symmetric**: $I(X; Y) = I(Y; X)$
- **Zero for independence**: $I(X; Y) = 0$ iff $X \\perp Y$
- **Maximum**: $I(X; Y) \\leq \\min(H(X), H(Y))$

### Normalized Mutual Information

Mutual information values depend on entropy magnitude, making comparisons across different datasets difficult. **Normalized MI** scales it to $[0, 1]$:

$$\\text{NMI}(X, Y) = \\frac{I(X; Y)}{\\sqrt{H(X) \\cdot H(Y)}}$$

If $H(X) = 0$ or $H(Y) = 0$, return $0.0$ (no uncertainty to reduce).

### Example

For independent uniform variables: $H(X) = H(Y) = 1$, $H(X,Y) = 2$, so $I = 1 + 1 - 2 = 0$.

For fully correlated variables ($Y = X$): $H(X,Y) = H(X) = H(Y) = 1$, so $I = 1$.

\`\`\`python
import math

def mutual_information(joint):
    mx = [sum(row) for row in joint]
    n_cols = len(joint[0])
    my = [sum(joint[i][j] for i in range(len(joint))) for j in range(n_cols)]
    hx = sum(-p * math.log2(p) for p in mx if p > 0)
    hy = sum(-p * math.log2(p) for p in my if p > 0)
    hxy = sum(-p * math.log2(p) for row in joint for p in row if p > 0)
    return hx + hy - hxy
\`\`\`

### Your Task

Implement:
- \`mutual_information(joint)\` — $I(X;Y) = H(X) + H(Y) - H(X,Y)$
- \`normalized_mi(joint)\` — $I(X;Y) / \\sqrt{H(X) \\cdot H(Y)}$; return $0.0$ if denominator is zero`,

	starterCode: `import math

def mutual_information(joint):
    # I(X;Y) = H(X) + H(Y) - H(X,Y)
    pass

def normalized_mi(joint):
    # NMI = I(X;Y) / sqrt(H(X) * H(Y)), return 0.0 if H(X)=0 or H(Y)=0
    pass

j1 = [[0.5, 0.0], [0.0, 0.5]]  # perfectly correlated
j2 = [[0.25, 0.25], [0.25, 0.25]]  # independent
print(mutual_information(j1))
print(normalized_mi(j1))
`,

	solution: `import math

def mutual_information(joint):
    mx = [sum(row) for row in joint]
    n_cols = len(joint[0])
    my = [sum(joint[i][j] for i in range(len(joint))) for j in range(n_cols)]
    hx = sum(-p * math.log2(p) for p in mx if p > 0)
    hy = sum(-p * math.log2(p) for p in my if p > 0)
    hxy = sum(-p * math.log2(p) for row in joint for p in row if p > 0)
    return hx + hy - hxy

def normalized_mi(joint):
    mx = [sum(row) for row in joint]
    n_cols = len(joint[0])
    my = [sum(joint[i][j] for i in range(len(joint))) for j in range(n_cols)]
    hx = sum(-p * math.log2(p) for p in mx if p > 0)
    hy = sum(-p * math.log2(p) for p in my if p > 0)
    mi = mutual_information(joint)
    denom = math.sqrt(hx * hy)
    if denom == 0:
        return 0.0
    return mi / denom

j1 = [[0.5, 0.0], [0.0, 0.5]]
j2 = [[0.25, 0.25], [0.25, 0.25]]
print(mutual_information(j1))
print(normalized_mi(j1))
`,

	tests: [
		{
			name: "perfectly correlated: MI=1.0, NMI=1.0",
			expected: "1.0\n1.0\n",
		},
		{
			name: "independent: MI=0.0",
			code: `{{FUNC}}
j = [[0.25, 0.25], [0.25, 0.25]]
print(mutual_information(j))`,
			expected: "0.0\n",
		},
		{
			name: "independent: NMI=0.0",
			code: `{{FUNC}}
j = [[0.25, 0.25], [0.25, 0.25]]
print(normalized_mi(j))`,
			expected: "0.0\n",
		},
		{
			name: "MI is non-negative",
			code: `{{FUNC}}
j = [[0.4, 0.1], [0.1, 0.4]]
print(mutual_information(j) >= 0)`,
			expected: "True\n",
		},
		{
			name: "round(MI [[0.4,0.1],[0.1,0.4]], 4) = 0.2781",
			code: `{{FUNC}}
j = [[0.4, 0.1], [0.1, 0.4]]
print(round(mutual_information(j), 4))`,
			expected: "0.2781\n",
		},
	],
};
