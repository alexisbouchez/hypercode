import type { Lesson } from "../../types";

export const sTGates: Lesson = {
	id: "s-t-gates",
	title: "S and T Phase Gates",
	chapterId: "advanced-gates",
	content: `## S and T Phase Gates

The **S gate** (also called the phase gate or $\\sqrt{Z}$) and **T gate** ($\\sqrt{S} = \\sqrt[4]{Z}$) are fundamental single-qubit phase gates.

### S Gate

The S gate applies a $\\frac{\\pi}{2}$ phase to $|1\\rangle$:

$$S = \\begin{pmatrix} 1 & 0 \\\\ 0 & i \\end{pmatrix}$$

$$S|0\\rangle = |0\\rangle \\qquad S|1\\rangle = i|1\\rangle$$

### T Gate

The T gate applies a $\\frac{\\pi}{4}$ phase to $|1\\rangle$:

$$T = \\begin{pmatrix} 1 & 0 \\\\ 0 & e^{i\\pi/4} \\end{pmatrix}$$

$$T|1\\rangle = e^{i\\pi/4}|1\\rangle = \\frac{1+i}{\\sqrt{2}}|1\\rangle$$

### Why These Gates Matter

Phase gates do not change measurement probabilities on their own — $|\\alpha|^2$ and $|\\beta|^2$ are unchanged. Their power lies in **interference**: applying S or T before a Hadamard creates a different superposition.

The T gate, combined with H and CNOT, forms a **universal gate set** for quantum computation.

### The S² = Z Relationship

Applying S twice gives the Pauli-Z gate:

$$S^2 = \\begin{pmatrix} 1 & 0 \\\\ 0 & i^2 \\end{pmatrix} = \\begin{pmatrix} 1 & 0 \\\\ 0 & -1 \\end{pmatrix} = Z$$

### Implementation

To apply S to a state $[\\alpha, \\beta]$:

$$S[\\alpha, \\beta] = [\\alpha,\\ i\\beta]$$

To apply T to a state $[\\alpha, \\beta]$:

$$T[\\alpha, \\beta] = [\\alpha,\\ e^{i\\pi/4}\\beta]$$

\`\`\`python
import cmath, math

def s_gate(state):
    return [complex(state[0]), state[1] * 1j]

def t_gate(state):
    return [complex(state[0]), state[1] * cmath.exp(1j * math.pi / 4)]
\`\`\`

### Your Task

Implement \`s_gate(state)\` and \`t_gate(state)\` that apply the S and T gates to a 2-element complex state vector.`,

	starterCode: `import cmath, math

def s_gate(state):
    # S = [[1,0],[0,i]]
    pass

def t_gate(state):
    # T = [[1,0],[0,e^(i*pi/4)]]
    pass

sq2 = 1/math.sqrt(2)
result = s_gate([sq2, sq2])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))
`,

	solution: `import cmath, math

def s_gate(state):
    return [complex(state[0]), state[1] * 1j]

def t_gate(state):
    return [complex(state[0]), state[1] * cmath.exp(1j * math.pi / 4)]

sq2 = 1/math.sqrt(2)
result = s_gate([sq2, sq2])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))
`,

	tests: [
		{
			name: "S on |+⟩ preserves amplitudes",
			expected: "0.7071\n0.7071\n",
		},
		{
			name: "S on |0⟩ is unchanged",
			code: `{{FUNC}}
result = s_gate([1, 0])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))`,
			expected: "1.0\n0.0\n",
		},
		{
			name: "T on |1⟩ preserves norm",
			code: `{{FUNC}}
result = t_gate([0, 1])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))`,
			expected: "0.0\n1.0\n",
		},
		{
			name: "S² = Z: S(S(|1⟩)) = -|1⟩",
			code: `{{FUNC}}
result = s_gate(s_gate([0, 1]))
print(round(result[0].real, 4))
print(round(result[1].real, 4))`,
			expected: "0.0\n-1.0\n",
		},
	],
};
