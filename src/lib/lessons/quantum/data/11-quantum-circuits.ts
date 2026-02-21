import type { Lesson } from "../../types";

export const quantumCircuits: Lesson = {
	id: "quantum-circuits",
	title: "Quantum Circuits",
	chapterId: "multi-qubit",
	content: `## Composing Gates

A **quantum circuit** is a sequence of gates applied to qubits. Gates are applied left to right (first gate first).

We can represent a circuit as a list of gate functions and apply them in order:

\`\`\`python
def apply_gates(state, gates):
    for gate in gates:
        state = gate(state)
    return state
\`\`\`

**Example: $H \to X \to H = Z$**

Applying these three gates in sequence to $|0\rangle$ is equivalent to applying the Z gate:

\`\`\`python
import math

def hadamard(state):
    s = 1 / math.sqrt(2)
    a, b = state[0], state[1]
    return [s * (a + b), s * (a - b)]

def x_gate(state):
    return [state[1], state[0]]

zero = [1.0, 0.0]
result = apply_gates(zero, [hadamard, x_gate, hadamard])
print(round(result[0], 4))  # 1.0 — same as Z|0⟩ = |0⟩
print(round(result[1], 4))  # 0.0
\`\`\`

This is called a **circuit identity**: $H \cdot X \cdot H = Z$.

### Your Task

Implement \`apply_gates(state, gates)\` and verify three circuit identities.`,

	starterCode: `import math

def apply_gates(state, gates):
    # Apply each gate in sequence, passing output as input to the next
    pass

def hadamard(state):
    s = 1 / math.sqrt(2)
    a, b = state[0], state[1]
    return [s * (a + b), s * (a - b)]

def x_gate(state):
    return [state[1], state[0]]

def z_gate(state):
    return [state[0], -state[1]]

zero = [1.0, 0.0]
one  = [0.0, 1.0]

# H then X then H should equal Z applied to |0⟩
hxh = apply_gates(zero, [hadamard, x_gate, hadamard])
z   = z_gate(zero)
print(round(hxh[0], 4) == round(z[0], 4))
print(round(hxh[1], 4) == round(z[1], 4))

# X applied twice = identity
xx = apply_gates(one, [x_gate, x_gate])
print(xx == one)

# H applied twice = identity
hh = apply_gates(zero, [hadamard, hadamard])
print(round(hh[0], 4))
print(round(hh[1], 4))
`,

	solution: `import math

def apply_gates(state, gates):
    for gate in gates:
        state = gate(state)
    return state

def hadamard(state):
    s = 1 / math.sqrt(2)
    a, b = state[0], state[1]
    return [s * (a + b), s * (a - b)]

def x_gate(state):
    return [state[1], state[0]]

def z_gate(state):
    return [state[0], -state[1]]

zero = [1.0, 0.0]
one  = [0.0, 1.0]

hxh = apply_gates(zero, [hadamard, x_gate, hadamard])
z   = z_gate(zero)
print(round(hxh[0], 4) == round(z[0], 4))
print(round(hxh[1], 4) == round(z[1], 4))

xx = apply_gates(one, [x_gate, x_gate])
print(xx == one)

hh = apply_gates(zero, [hadamard, hadamard])
print(round(hh[0], 4))
print(round(hh[1], 4))
`,

	tests: [
		{
			name: "apply_gates applies a single gate correctly",
			code: `{{FUNC}}
def x_gate(state):
    return [state[1], state[0]]
result = apply_gates([1.0, 0.0], [x_gate])
print(result)`,
			expected: "[0.0, 1.0]\n",
		},
		{
			name: "apply_gates with two gates",
			code: `{{FUNC}}
def x_gate(state):
    return [state[1], state[0]]
result = apply_gates([1.0, 0.0], [x_gate, x_gate])
print(result)`,
			expected: "[1.0, 0.0]\n",
		},
		{
			name: "H·X·H = Z identity on |0⟩",
			code: `{{FUNC}}
import math
def hadamard(state):
    s = 1 / math.sqrt(2)
    a, b = state[0], state[1]
    return [s * (a + b), s * (a - b)]
def x_gate(state):
    return [state[1], state[0]]
result = apply_gates([1.0, 0.0], [hadamard, x_gate, hadamard])
print(round(result[0], 4))
print(round(result[1], 4))`,
			expected: "1.0\n0.0\n",
		},
		{
			name: "H·X·H = Z identity on |1⟩",
			code: `{{FUNC}}
import math
def hadamard(state):
    s = 1 / math.sqrt(2)
    a, b = state[0], state[1]
    return [s * (a + b), s * (a - b)]
def x_gate(state):
    return [state[1], state[0]]
result = apply_gates([0.0, 1.0], [hadamard, x_gate, hadamard])
print(round(result[0], 4))
print(round(result[1], 4))`,
			expected: "0.0\n-1.0\n",
		},
	],
};
