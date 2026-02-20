import type { Lesson } from "../../types";

export const bellStates: Lesson = {
	id: "bell-states",
	title: "Bell States",
	chapterId: "multi-qubit",
	content: `## Quantum Entanglement

The **Bell states** are the simplest examples of quantum entanglement — two qubits whose fates are linked no matter how far apart they are.

The most famous Bell state is **Φ+** (phi-plus):

**Φ+ = (|00⟩ + |11⟩) / √2**

To create it:
1. Start with |00⟩
2. Apply H to the first qubit → (|0⟩ + |1⟩)/√2 ⊗ |0⟩ = (|00⟩ + |10⟩)/√2
3. Apply CNOT → (|00⟩ + |11⟩)/√2

Applying H to the first qubit of a 4-element state [a, b, c, d]:

\`\`\`
H⊗I [a, b, c, d] = [(a+c)/√2, (b+d)/√2, (a-c)/√2, (b-d)/√2]
\`\`\`

\`\`\`python
import math

def h_on_first(state):
    s = 1 / math.sqrt(2)
    return [
        s * (state[0] + state[2]),
        s * (state[1] + state[3]),
        s * (state[0] - state[2]),
        s * (state[1] - state[3]),
    ]

def cnot(state):
    return [state[0], state[1], state[3], state[2]]

ket_00 = [1.0, 0.0, 0.0, 0.0]
phi_plus = cnot(h_on_first(ket_00))
# [1/√2, 0, 0, 1/√2] ≈ [0.7071, 0, 0, 0.7071]
\`\`\`

### Your Task

Implement \`h_on_first(state)\` that applies H to the first qubit of a 2-qubit state. Then create the Bell state Φ+.`,

	starterCode: `import math

def h_on_first(state):
    # Apply Hadamard to first qubit of 4-element state
    # Result: [s*(a+c), s*(b+d), s*(a-c), s*(b-d)] where s=1/sqrt(2)
    pass

def cnot(state):
    return [state[0], state[1], state[3], state[2]]

ket_00 = [1.0, 0.0, 0.0, 0.0]
phi_plus = cnot(h_on_first(ket_00))

print(round(phi_plus[0], 4))  # amplitude of |00⟩
print(round(phi_plus[1], 4))  # amplitude of |01⟩
print(round(phi_plus[2], 4))  # amplitude of |10⟩
print(round(phi_plus[3], 4))  # amplitude of |11⟩
`,

	solution: `import math

def h_on_first(state):
    s = 1 / math.sqrt(2)
    return [
        s * (state[0] + state[2]),
        s * (state[1] + state[3]),
        s * (state[0] - state[2]),
        s * (state[1] - state[3]),
    ]

def cnot(state):
    return [state[0], state[1], state[3], state[2]]

ket_00 = [1.0, 0.0, 0.0, 0.0]
phi_plus = cnot(h_on_first(ket_00))

print(round(phi_plus[0], 4))
print(round(phi_plus[1], 4))
print(round(phi_plus[2], 4))
print(round(phi_plus[3], 4))
`,

	tests: [
		{
			name: "h_on_first applied to |00⟩",
			code: `{{FUNC}}
result = h_on_first([1.0, 0.0, 0.0, 0.0])
print(round(result[0], 4))
print(round(result[1], 4))
print(round(result[2], 4))
print(round(result[3], 4))`,
			expected: "0.7071\n0.0\n0.7071\n0.0\n",
		},
		{
			name: "Bell state Φ+ has amplitude 1/√2 on |00⟩ and |11⟩",
			code: `{{FUNC}}
phi_plus = cnot(h_on_first([1.0, 0.0, 0.0, 0.0]))
print(round(phi_plus[0], 4))
print(round(phi_plus[1], 4))
print(round(phi_plus[2], 4))
print(round(phi_plus[3], 4))`,
			expected: "0.7071\n0.0\n0.0\n0.7071\n",
		},
		{
			name: "Bell state is normalized",
			code: `{{FUNC}}
import math
phi_plus = cnot(h_on_first([1.0, 0.0, 0.0, 0.0]))
n = math.sqrt(sum(a**2 for a in phi_plus))
print(round(n, 4))`,
			expected: "1.0\n",
		},
	],
};
