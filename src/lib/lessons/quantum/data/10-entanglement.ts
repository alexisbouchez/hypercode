import type { Lesson } from "../../types";

export const entanglement: Lesson = {
	id: "entanglement",
	title: "Detecting Entanglement",
	chapterId: "multi-qubit",
	content: `## Separable vs. Entangled States

A 2-qubit state is **separable** if it can be written as a tensor product of two single-qubit states:

[α₀₀, α₀₁, α₁₀, α₁₁] = [a, b] ⊗ [c, d]

This requires: **α₀₀ · α₁₁ = α₀₁ · α₁₀**

If this equality does not hold, the state is **entangled** — it cannot be factored into independent parts.

\`\`\`python
def is_entangled(state):
    return abs(state[0] * state[3] - state[1] * state[2]) > 1e-9

import math
s = 1 / math.sqrt(2)

ket_00   = [1.0, 0.0, 0.0, 0.0]  # Separable: |0⟩⊗|0⟩
phi_plus = [s,   0.0, 0.0, s  ]  # Entangled: Bell state

print(is_entangled(ket_00))    # False
print(is_entangled(phi_plus))  # True
\`\`\`

Entangled states exhibit **quantum correlations**: measuring one qubit instantly determines the outcome of measuring the other, regardless of the distance between them. This is what Einstein called "spooky action at a distance."

### Your Task

Implement \`is_entangled(state)\` that checks whether a 2-qubit state is entangled.`,

	starterCode: `import math

def is_entangled(state):
    # Entangled if |state[0]*state[3] - state[1]*state[2]| > 1e-9
    pass

s = 1 / math.sqrt(2)

ket_00    = [1.0, 0.0, 0.0, 0.0]  # |00⟩ — separable
phi_plus  = [s,   0.0, 0.0, s  ]  # Bell Φ+ — entangled
product   = [0.5, 0.5, 0.5, 0.5]  # |+⟩⊗|+⟩ — separable

print(is_entangled(ket_00))
print(is_entangled(phi_plus))
print(is_entangled(product))
`,

	solution: `import math

def is_entangled(state):
    return abs(state[0] * state[3] - state[1] * state[2]) > 1e-9

s = 1 / math.sqrt(2)

ket_00    = [1.0, 0.0, 0.0, 0.0]
phi_plus  = [s,   0.0, 0.0, s  ]
product   = [0.5, 0.5, 0.5, 0.5]

print(is_entangled(ket_00))
print(is_entangled(phi_plus))
print(is_entangled(product))
`,

	tests: [
		{
			name: "|00⟩ is not entangled",
			code: `{{FUNC}}
print(is_entangled([1.0, 0.0, 0.0, 0.0]))`,
			expected: "False\n",
		},
		{
			name: "Bell state Φ+ is entangled",
			code: `{{FUNC}}
import math
s = 1 / math.sqrt(2)
print(is_entangled([s, 0.0, 0.0, s]))`,
			expected: "True\n",
		},
		{
			name: "product state |+⟩⊗|+⟩ is not entangled",
			code: `{{FUNC}}
print(is_entangled([0.5, 0.5, 0.5, 0.5]))`,
			expected: "False\n",
		},
		{
			name: "Bell state Ψ+ is entangled",
			code: `{{FUNC}}
import math
s = 1 / math.sqrt(2)
psi_plus = [0.0, s, s, 0.0]
print(is_entangled(psi_plus))`,
			expected: "True\n",
		},
	],
};
