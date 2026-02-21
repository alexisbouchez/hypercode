import type { Lesson } from "../../types";

export const phaseEstimation: Lesson = {
	id: "phase-estimation",
	title: "Quantum Phase Estimation",
	chapterId: "qft",
	content: `## Quantum Phase Estimation (QPE)

**Quantum Phase Estimation** solves a fundamental problem: given a unitary operator $U$ and one of its eigenstates $|\\psi\\rangle$, estimate the eigenphase $\\phi$ such that:

$$U|\\psi\\rangle = e^{2\\pi i\\phi}|\\psi\\rangle$$

The algorithm uses $n$ ancilla qubits, giving precision $1/2^n$.

### The QPE State

After the QFT-based interference step, the amplitude for measuring outcome $m$ is:

$$\\text{amplitude}(m) = \\frac{1}{N}\\sum_{k=0}^{N-1} e^{2\\pi i(\\phi N - m)k/N}$$

where $N = 2^n$. This is a geometric series that peaks sharply when $m = \\phi N$.

**When $\\phi N$ is an integer** (exact case), the sum equals 1 for $m = \\phi N$ and 0 for all other $m$ — perfect estimation!

For example, with $\\phi = 0.25$ and $n = 2$ bits ($N=4$):
- $\\phi N = 1$, so amplitude is 1 for $m=1$ and 0 for $m \\in \\{0, 2, 3\\}$
- Measurement returns 1 with certainty; estimate $= 1/4 = 0.25$ ✓

### Algorithm Summary

1. Initialize $n$ ancilla qubits in $|0\\rangle^{\\otimes n}$
2. Apply Hadamard to each ancilla → uniform superposition
3. Apply controlled-$U^{2^k}$ for $k = 0, 1, \\ldots, n-1$
4. Apply inverse QFT to ancilla register
5. Measure ancilla → outcome $m$; estimate $\\hat{\\phi} = m / 2^n$

\`\`\`python
import cmath, math

def qpe_state(phase, n_bits):
    N = 2 ** n_bits
    amplitudes = []
    for m in range(N):
        amp = 0
        for k in range(N):
            amp += cmath.exp(2j * math.pi * (phase * N - m) * k / N)
        amp /= N
        amplitudes.append(amp)
    return amplitudes

def phase_estimation(phase, n_bits):
    amps = qpe_state(phase, n_bits)
    probs = [abs(a)**2 for a in amps]
    best = max(range(len(probs)), key=lambda i: probs[i])
    return best / (2**n_bits)

print(phase_estimation(0.25, 2))  # 0.25
\`\`\`

### Precision Scaling

With $n$ ancilla bits, QPE can distinguish phases that differ by at least $1/2^n$. Doubling precision requires one extra qubit — an exponential improvement over classical repeated sampling.

### Your Task

Implement both functions:
- \`qpe_state(phase, n_bits)\` — returns the $N = 2^{n}$ complex amplitudes
- \`phase_estimation(phase, n_bits)\` — returns the estimated phase as a float`,

	starterCode: `import cmath, math

def qpe_state(phase, n_bits):
    # Compute amplitudes: for each m, sum over k of exp(2πi*(phase*N - m)*k/N) / N
    pass

def phase_estimation(phase, n_bits):
    # Return best / N where best is the most probable measurement outcome
    pass

print(phase_estimation(0.25, 2))
`,

	solution: `import cmath, math

def qpe_state(phase, n_bits):
    N = 2 ** n_bits
    amplitudes = []
    for m in range(N):
        amp = 0
        for k in range(N):
            amp += cmath.exp(2j * math.pi * (phase * N - m) * k / N)
        amp /= N
        amplitudes.append(amp)
    return amplitudes

def phase_estimation(phase, n_bits):
    amps = qpe_state(phase, n_bits)
    probs = [abs(a)**2 for a in amps]
    best = max(range(len(probs)), key=lambda i: probs[i])
    return best / (2**n_bits)

print(phase_estimation(0.25, 2))
`,

	tests: [
		{
			name: "phase_estimation(0.25, 2) → 0.25 exactly",
			expected: "0.25\n",
		},
		{
			name: "phase_estimation(0.5, 3) → 0.5",
			code: `{{FUNC}}
print(phase_estimation(0.5, 3))`,
			expected: "0.5\n",
		},
		{
			name: "phase_estimation(0.125, 4) → 0.125",
			code: `{{FUNC}}
print(phase_estimation(0.125, 4))`,
			expected: "0.125\n",
		},
		{
			name: "phase_estimation(0.75, 2) → 0.75",
			code: `{{FUNC}}
print(phase_estimation(0.75, 2))`,
			expected: "0.75\n",
		},
	],
};
