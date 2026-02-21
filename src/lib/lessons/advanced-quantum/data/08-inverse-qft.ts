import type { Lesson } from "../../types";

export const inverseQFT: Lesson = {
	id: "inverse-qft",
	title: "Inverse Quantum Fourier Transform",
	chapterId: "qft",
	content: `## The Inverse QFT

The **Inverse Quantum Fourier Transform (IQFT)** is the conjugate transpose (adjoint) of the QFT. Since the QFT is unitary, its inverse is obtained by negating the exponent:

$$\\text{IQFT}(\\psi)_k = \\frac{1}{\\sqrt{N}}\\sum_{j=0}^{N-1} c_j\\, e^{-2\\pi ijk/N}$$

This is the defining property of a unitary operator: $U^\\dagger U = I$.

### Round-Trip Identity

Applying QFT then IQFT recovers the original state (up to floating-point precision):

$$\\text{IQFT}(\\text{QFT}(|\\psi\\rangle)) = |\\psi\\rangle$$

### Example: Recovering $|0\\rangle$

The QFT of $|0\\rangle = [1,0,0,0]$ is the uniform superposition $[\\tfrac{1}{2}, \\tfrac{1}{2}, \\tfrac{1}{2}, \\tfrac{1}{2}]$.

Applying IQFT to this uniform superposition:

$$\\text{IQFT}(\\psi)_0 = \\frac{1}{2}\\sum_{j=0}^{3} \\frac{1}{2} \\cdot e^{0} = \\frac{1}{2} \\cdot \\frac{4}{2} = 1$$

$$\\text{IQFT}(\\psi)_1 = \\frac{1}{2}\\sum_{j=0}^{3} \\frac{1}{2} \\cdot e^{-2\\pi ij/4} = \\frac{1}{4}(1 + (-i) + (-1) + i) = 0$$

All higher components vanish similarly — returning $|0\\rangle$ exactly.

\`\`\`python
import cmath, math

def iqft(state):
    N = len(state)
    result = []
    for k in range(N):
        amp = sum(state[j] * cmath.exp(-2j * math.pi * j * k / N)
                  for j in range(N))
        result.append(amp / math.sqrt(N))
    return result

# IQFT of uniform superposition recovers |0⟩
result = iqft([0.5, 0.5, 0.5, 0.5])
print(round(abs(result[0]), 4))  # 1.0
print(round(abs(result[1]), 4))  # 0.0
\`\`\`

### Role in Quantum Algorithms

The IQFT is used in **Quantum Phase Estimation**: after building up phase information in the ancilla register via controlled-$U$ operations, the IQFT converts that phase into a readable binary fraction.

### Your Task

Implement \`iqft(state)\` using the formula above (note the negative sign in the exponent compared to QFT).`,

	starterCode: `import cmath, math

def iqft(state):
    # Compute the Inverse QFT: use -2πi in the exponent
    pass

result = iqft([0.5, 0.5, 0.5, 0.5])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))
`,

	solution: `import cmath, math

def iqft(state):
    N = len(state)
    result = []
    for k in range(N):
        amp = sum(state[j] * cmath.exp(-2j * math.pi * j * k / N) for j in range(N))
        result.append(amp / math.sqrt(N))
    return result

result = iqft([0.5, 0.5, 0.5, 0.5])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))
`,

	tests: [
		{
			name: "iqft([0.5,0.5,0.5,0.5]) → recovers |0⟩: abs[0]=1.0, abs[1]=0.0",
			expected: "1.0\n0.0\n",
		},
		{
			name: "iqft(qft([1,0,0,0])) round-trip → [1,0,0,0]: abs[0]=1.0, abs[1]=0.0",
			code: `{{FUNC}}
import cmath, math

def qft(state):
    N = len(state)
    result = []
    for k in range(N):
        amp = sum(state[j] * cmath.exp(2j * math.pi * j * k / N) for j in range(N))
        result.append(amp / math.sqrt(N))
    return result

state = [1.0, 0.0, 0.0, 0.0]
recovered = iqft(qft(state))
print(round(abs(recovered[0]), 4))
print(round(abs(recovered[1]), 4))`,
			expected: "1.0\n0.0\n",
		},
		{
			name: "iqft([1.0, 0.0]) → uniform superposition: both abs = 0.7071",
			code: `{{FUNC}}
result = iqft([1.0, 0.0])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))`,
			expected: "0.7071\n0.7071\n",
		},
		{
			name: "iqft preserves norm: sum of |amp|² = 1.0",
			code: `{{FUNC}}
state = [0.6, 0.0, 0.8, 0.0]
result = iqft(state)
print(round(sum(abs(x)**2 for x in result), 4))`,
			expected: "1.0\n",
		},
	],
};
