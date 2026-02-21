import type { Lesson } from "../../types";

export const quantumFourierTransform: Lesson = {
	id: "qft",
	title: "Quantum Fourier Transform",
	chapterId: "qft",
	content: `## The Quantum Fourier Transform

The **Quantum Fourier Transform (QFT)** is the quantum analogue of the Discrete Fourier Transform. It is a unitary operation that maps basis states as:

$$\\text{QFT}|j\\rangle = \\frac{1}{\\sqrt{N}}\\sum_{k=0}^{N-1} e^{2\\pi i jk/N}|k\\rangle$$

where $N = 2^n$ for an $n$-qubit system.

For a general superposition $|\\psi\\rangle = \\sum_j c_j|j\\rangle$, linearity gives the $k$-th output amplitude:

$$\\text{QFT}|\\psi\\rangle_k = \\frac{1}{\\sqrt{N}}\\sum_{j=0}^{N-1} c_j\\, e^{2\\pi ijk/N}$$

### Connection to the Hadamard Gate

For a single qubit ($N=2$), the QFT is exactly the **Hadamard gate** $H$:

$$H = \\frac{1}{\\sqrt{2}}\\begin{pmatrix}1&1\\\\1&-1\\end{pmatrix}$$

Applying $H$ to $|0\\rangle = [1, 0]$ gives $[1/\\sqrt{2},\\, 1/\\sqrt{2}]$ — equal superposition.

### Unitarity

The QFT is **unitary**: it preserves the norm of the state vector. If $\\sum_j |c_j|^2 = 1$, then $\\sum_k |\\text{QFT}(\\psi)_k|^2 = 1$.

\`\`\`python
import cmath, math

def qft(state):
    N = len(state)
    result = []
    for k in range(N):
        amp = sum(state[j] * cmath.exp(2j * math.pi * j * k / N)
                  for j in range(N))
        result.append(amp / math.sqrt(N))
    return result

# QFT on |0⟩ is the Hadamard: each amplitude = 1/√2
result = qft([1.0, 0.0])
print(round(abs(result[0]), 4))  # 0.7071
print(round(abs(result[1]), 4))  # 0.7071
\`\`\`

### Applications

The QFT is a key subroutine in:
- **Shor's algorithm** — integer factorization in polynomial time
- **Quantum phase estimation** — extracting eigenphases of unitary operators
- **Hidden subgroup problems** — a broad class of quantum speedups

### Your Task

Implement \`qft(state)\` that takes an $N$-element amplitude list and returns the QFT output as a list of complex amplitudes. Use the formula:

$$\\text{result}[k] = \\frac{1}{\\sqrt{N}}\\sum_{j=0}^{N-1} \\text{state}[j]\\cdot e^{2\\pi ijk/N}$$`,

	starterCode: `import cmath, math

def qft(state):
    # Compute the Quantum Fourier Transform of state
    pass

result = qft([1.0, 0.0])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))
`,

	solution: `import cmath, math

def qft(state):
    N = len(state)
    result = []
    for k in range(N):
        amp = sum(state[j] * cmath.exp(2j * math.pi * j * k / N) for j in range(N))
        result.append(amp / math.sqrt(N))
    return result

result = qft([1.0, 0.0])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))
`,

	tests: [
		{
			name: "qft([1.0, 0.0]) → Hadamard on |0⟩: both amplitudes 0.7071",
			expected: "0.7071\n0.7071\n",
		},
		{
			name: "qft([1.0,0.0,0.0,0.0]) → uniform superposition: all abs = 0.5",
			code: `{{FUNC}}
result = qft([1.0, 0.0, 0.0, 0.0])
for x in result:
    print(round(abs(x), 4))`,
			expected: "0.5\n0.5\n0.5\n0.5\n",
		},
		{
			name: "qft preserves norm: sum of |amp|² = 1.0",
			code: `{{FUNC}}
state = [0.5, 0.5, 0.5, 0.5]
result = qft(state)
print(round(sum(abs(x)**2 for x in result), 4))`,
			expected: "1.0\n",
		},
		{
			name: "qft([0.0, 1.0]) → QFT on |1⟩: both abs = 0.7071",
			code: `{{FUNC}}
result = qft([0.0, 1.0])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))`,
			expected: "0.7071\n0.7071\n",
		},
	],
};
