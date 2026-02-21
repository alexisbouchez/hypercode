import type { Lesson } from "../../types";

export const normalization: Lesson = {
	id: "normalization",
	title: "Normalization",
	chapterId: "qubits",
	content: `## Valid Quantum States

Not every pair of numbers is a valid qubit. The **normalization condition** requires:

$$|\alpha|^2 + |\beta|^2 = 1$$

This ensures the probabilities of all outcomes sum to 1. The quantity $\sqrt{\alpha^2 + \beta^2}$ is called the **norm** of the state.

\`\`\`python
import math

def norm(state):
    return math.sqrt(state[0]**2 + state[1]**2)

def is_normalized(state):
    return abs(norm(state) - 1.0) < 1e-9

print(is_normalized([1.0, 0.0]))   # True
print(is_normalized([0.6, 0.8]))   # True  (0.36 + 0.64 = 1)
print(is_normalized([3.0, 4.0]))   # False (9 + 16 = 25, norm = 5)
\`\`\`

To fix an unnormalized state, divide each amplitude by the norm:

\`\`\`python
def normalize(state):
    n = norm(state)
    return [state[0] / n, state[1] / n]

print(normalize([3.0, 4.0]))  # [0.6, 0.8]
\`\`\`

### Your Task

Implement \`norm(state)\`, \`is_normalized(state)\`, and \`normalize(state)\`.`,

	starterCode: `import math

def norm(state):
    # Return sqrt(alpha^2 + beta^2)
    pass

def is_normalized(state):
    # Return True if norm is within 1e-9 of 1.0
    pass

def normalize(state):
    # Divide each amplitude by the norm
    pass

print(is_normalized([1.0, 0.0]))
print(is_normalized([0.6, 0.8]))
print(is_normalized([3.0, 4.0]))
normalized = normalize([3.0, 4.0])
print(round(normalized[0], 1))
print(round(normalized[1], 1))
`,

	solution: `import math

def norm(state):
    return math.sqrt(state[0]**2 + state[1]**2)

def is_normalized(state):
    return abs(norm(state) - 1.0) < 1e-9

def normalize(state):
    n = norm(state)
    return [state[0] / n, state[1] / n]

print(is_normalized([1.0, 0.0]))
print(is_normalized([0.6, 0.8]))
print(is_normalized([3.0, 4.0]))
normalized = normalize([3.0, 4.0])
print(round(normalized[0], 1))
print(round(normalized[1], 1))
`,

	tests: [
		{
			name: "norm of [1.0, 0.0] is 1.0",
			code: `{{FUNC}}
print(round(norm([1.0, 0.0]), 4))`,
			expected: "1.0\n",
		},
		{
			name: "norm of [3.0, 4.0] is 5.0",
			code: `{{FUNC}}
print(round(norm([3.0, 4.0]), 1))`,
			expected: "5.0\n",
		},
		{
			name: "is_normalized correctly identifies valid states",
			code: `{{FUNC}}
print(is_normalized([1.0, 0.0]))
print(is_normalized([0.6, 0.8]))
print(is_normalized([3.0, 4.0]))`,
			expected: "True\nTrue\nFalse\n",
		},
		{
			name: "normalize scales [3, 4] to [0.6, 0.8]",
			code: `{{FUNC}}
n = normalize([3.0, 4.0])
print(round(n[0], 1))
print(round(n[1], 1))`,
			expected: "0.6\n0.8\n",
		},
	],
};
