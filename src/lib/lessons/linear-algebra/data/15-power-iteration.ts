import type { Lesson } from "../../types";

export const powerIteration: Lesson = {
	id: "power-iteration",
	title: "Power Iteration",
	chapterId: "numerical",
	content: `## Finding the Dominant Eigenvalue

**Power iteration** is an iterative algorithm that finds the **largest eigenvalue** (in magnitude) of a matrix.

Starting from a random vector, repeatedly multiply by $\mathbf{A}$ and normalize:

\`\`\`python
import math

def power_iteration(A, num_iter=100):
    n = len(A)
    # Start with a ones vector
    v = [1.0] * n
    eigenvalue = 0.0
    for _ in range(num_iter):
        # Multiply A @ v
        Av = [sum(A[i][j] * v[j] for j in range(n)) for i in range(n)]
        # Rayleigh quotient: λ ≈ (v · Av) / (v · v)
        num = sum(v[i] * Av[i] for i in range(n))
        den = sum(v[i] * v[i] for i in range(n))
        eigenvalue = num / den
        # Normalize
        norm = math.sqrt(sum(x**2 for x in Av))
        v = [x / norm for x in Av]
    return round(eigenvalue, 4)

A = [[3, 0], [0, 5]]
print(power_iteration(A))   # 5.0
\`\`\`

### Why It Works

Multiplying any vector by $\mathbf{A}$ amplifies the component in the direction of the largest eigenvector. After enough iterations, the vector aligns with that eigenvector, and the **Rayleigh quotient** gives the eigenvalue.

### The Rayleigh Quotient

For a vector $\mathbf{v}$ and matrix $\mathbf{A}$:

$$\lambda \approx \frac{\mathbf{v}^T \mathbf{A} \mathbf{v}}{\mathbf{v}^T \mathbf{v}}$$

This converges to the dominant eigenvalue as $\mathbf{v}$ aligns with the dominant eigenvector.

### Applications

- **PageRank** — Google's algorithm is power iteration on the web graph
- **PCA** — the first principal component corresponds to the dominant eigenvector
- **Markov chains** — finding stationary distributions

### Your Task

Implement \`power_iteration(A, num_iter=100)\` that returns the dominant eigenvalue of $\mathbf{A}$, rounded to 4 decimal places.`,

	starterCode: `import math

def power_iteration(A, num_iter=100):
    # Use Rayleigh quotient to find dominant eigenvalue
    # Return rounded to 4 decimal places
    pass

A = [[3, 0], [0, 5]]
print(power_iteration(A))
`,

	solution: `import math

def power_iteration(A, num_iter=100):
    n = len(A)
    v = [1.0] * n
    eigenvalue = 0.0
    for _ in range(num_iter):
        Av = [sum(A[i][j] * v[j] for j in range(n)) for i in range(n)]
        num = sum(v[i] * Av[i] for i in range(n))
        den = sum(v[i] * v[i] for i in range(n))
        eigenvalue = num / den
        norm = math.sqrt(sum(x**2 for x in Av))
        v = [x / norm for x in Av]
    return round(eigenvalue, 4)

A = [[3, 0], [0, 5]]
print(power_iteration(A))
`,

	tests: [
		{
			name: "diagonal matrix [[3,0],[0,5]] → dominant eigenvalue 5.0",
			expected: "5.0\n",
		},
		{
			name: "symmetric 2×2 [[4,1],[1,3]] → dominant eigenvalue 4.618",
			code: `{{FUNC}}
A = [[4, 1], [1, 3]]
print(power_iteration(A))`,
			expected: "4.618\n",
		},
		{
			name: "identity matrix → eigenvalue 1.0",
			code: `{{FUNC}}
I = [[1, 0], [0, 1]]
print(power_iteration(I))`,
			expected: "1.0\n",
		},
		{
			name: "scaling matrix [[6,0],[0,6]] → eigenvalue 6.0",
			code: `{{FUNC}}
A = [[6, 0], [0, 6]]
print(power_iteration(A))`,
			expected: "6.0\n",
		},
	],
};
