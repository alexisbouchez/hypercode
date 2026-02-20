import type { Lesson } from "../../types";

export const zGate: Lesson = {
	id: "z-gate",
	title: "The Z Gate",
	chapterId: "gates",
	content: `## The Phase Flip Gate

The **Pauli-Z gate** flips the *phase* (sign) of the |1⟩ amplitude while leaving |0⟩ unchanged:

- Z|0⟩ = |0⟩
- Z|1⟩ = −|1⟩

On a general state [α, β]:

\`\`\`
Z[α, β] = [α, -β]
\`\`\`

\`\`\`python
def z_gate(state):
    return [state[0], -state[1]]
\`\`\`

The Z gate has no classical equivalent — it introduces a **phase** that is invisible when measuring in the computational basis, but becomes observable when combined with other gates.

For example, Z transforms |+⟩ into |−⟩:

\`\`\`python
import math
s = 1 / math.sqrt(2)
plus = [s, s]       # |+⟩
minus = z_gate(plus) # |−⟩ = [s, -s]
print(round(minus[0], 4))   # 0.7071
print(round(minus[1], 4))   # -0.7071
\`\`\`

### Your Task

Implement \`z_gate(state)\` that applies the Pauli-Z phase flip.`,

	starterCode: `import math

def z_gate(state):
    # Keep alpha, negate beta
    pass

s = 1 / math.sqrt(2)
plus = [s, s]
minus = z_gate(plus)

print(round(minus[0], 4))
print(round(minus[1], 4))

print(z_gate([1.0, 0.0]))
print(z_gate(z_gate([0.0, 1.0])) == [0.0, 1.0])
`,

	solution: `import math

def z_gate(state):
    return [state[0], -state[1]]

s = 1 / math.sqrt(2)
plus = [s, s]
minus = z_gate(plus)

print(round(minus[0], 4))
print(round(minus[1], 4))

print(z_gate([1.0, 0.0]))
print(z_gate(z_gate([0.0, 1.0])) == [0.0, 1.0])
`,

	tests: [
		{
			name: "Z|0⟩ = |0⟩ (unchanged)",
			code: `{{FUNC}}
result = z_gate([1.0, 0.0])
print(result[0])
print(abs(result[1]))`,
			expected: "1.0\n0.0\n",
		},
		{
			name: "Z|1⟩ = -|1⟩ (phase flipped)",
			code: `{{FUNC}}
result = z_gate([0.0, 1.0])
print(result[0])
print(result[1])`,
			expected: "0.0\n-1.0\n",
		},
		{
			name: "Z transforms |+⟩ to |−⟩",
			code: `{{FUNC}}
import math
s = 1 / math.sqrt(2)
minus = z_gate([s, s])
print(round(minus[0], 4))
print(round(minus[1], 4))`,
			expected: "0.7071\n-0.7071\n",
		},
		{
			name: "Z applied twice is identity",
			code: `{{FUNC}}
print(z_gate(z_gate([0.0, 1.0])) == [0.0, 1.0])
print(z_gate(z_gate([0.6, 0.8])) == [0.6, 0.8])`,
			expected: "True\nTrue\n",
		},
	],
};
