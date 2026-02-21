import type { Lesson } from "../../types";

export const controlledPhase: Lesson = {
	id: "controlled-phase",
	title: "Controlled-Phase Gate",
	chapterId: "qft",
	content: `## The Controlled-Phase Gate $CR_k$

The **controlled-phase gate** $CR_k$ is fundamental to the Quantum Fourier Transform. It applies a phase $e^{2\\pi i/2^k}$ to the $|11\\rangle$ component only, leaving all other basis states unchanged:

$$CR_k = \\begin{pmatrix}1&0&0&0\\\\0&1&0&0\\\\0&0&1&0\\\\0&0&0&e^{2\\pi i/2^k}\\end{pmatrix}$$

The state vector for a 2-qubit system is represented as a 4-element list ordered by basis state:

$$|\\psi\\rangle = c_{00}|00\\rangle + c_{01}|01\\rangle + c_{10}|10\\rangle + c_{11}|11\\rangle$$

So the list \`[c00, c01, c10, c11]\` maps index 0 → $|00\\rangle$, index 1 → $|01\\rangle$, index 2 → $|10\\rangle$, index 3 → $|11\\rangle$.

### Special Cases

- $k=1$: phase $= e^{i\\pi} = -1$ (a $Z$ gate on the target, conditioned on control)
- $k=2$: phase $= e^{i\\pi/2} = i$ (the $S$ gate, conditioned)
- $k=3$: phase $= e^{i\\pi/4} = \\frac{1+i}{\\sqrt{2}}$ (the $T$ gate, conditioned)

\`\`\`python
import cmath, math

def cphase(k, state):
    phase = cmath.exp(2j * math.pi / (2**k))
    result = list(state)
    result[3] = state[3] * phase
    return result

# k=1 applies e^(iπ) = -1 to |11⟩ amplitude
result = cphase(1, [0, 0, 0, 1.0])
print(round(abs(result[3]), 4))    # 1.0
print(round(result[3].real, 4))    # -1.0
\`\`\`

### Why CR_k Matters

In the QFT circuit, $CR_k$ gates with increasing $k$ build up finer phase increments. The $k$-th gate contributes a phase of $2\\pi / 2^k$ radians, so larger $k$ values give smaller phase kicks — encoding higher-frequency information into the transform.

### Your Task

Implement \`cphase(k, state)\` where:
- \`k\` is a positive integer (the phase order)
- \`state\` is a 4-element list representing amplitudes $[c_{00}, c_{01}, c_{10}, c_{11}]$

The function returns a new list with the $|11\\rangle$ amplitude multiplied by $e^{2\\pi i/2^k}$.`,

	starterCode: `import cmath, math

def cphase(k, state):
    # Apply phase e^(2πi/2^k) to the |11⟩ component (index 3)
    pass

result = cphase(1, [0, 0, 0, 1.0])
print(round(abs(result[3]), 4))
print(round(result[3].real, 4))
`,

	solution: `import cmath, math

def cphase(k, state):
    phase = cmath.exp(2j * math.pi / (2**k))
    result = list(state)
    result[3] = state[3] * phase
    return result

result = cphase(1, [0, 0, 0, 1.0])
print(round(abs(result[3]), 4))
print(round(result[3].real, 4))
`,

	tests: [
		{
			name: "cphase(1, [0,0,0,1.0]) applies e^(iπ)=-1: abs=1.0, real=-1.0",
			expected: "1.0\n-1.0\n",
		},
		{
			name: "cphase(2, [0,0,0,1.0]) applies e^(iπ/2)=i: abs=1.0, imag=1.0",
			code: `{{FUNC}}
result = cphase(2, [0, 0, 0, 1.0])
print(round(abs(result[3]), 4))
print(round(result[3].imag, 4))`,
			expected: "1.0\n1.0\n",
		},
		{
			name: "cphase(1, [1.0,0,0,0]) leaves |00⟩ unchanged: abs=1.0",
			code: `{{FUNC}}
result = cphase(1, [1.0, 0, 0, 0])
print(round(abs(result[0]), 4))`,
			expected: "1.0\n",
		},
		{
			name: "cphase(3, [0,0,0,1.0]) applies e^(iπ/4): abs=1.0, real=0.7071",
			code: `{{FUNC}}
result = cphase(3, [0, 0, 0, 1.0])
print(round(abs(result[3]), 4))
print(round(result[3].real, 4))`,
			expected: "1.0\n0.7071\n",
		},
	],
};
