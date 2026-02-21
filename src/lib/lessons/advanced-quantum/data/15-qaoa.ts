import type { Lesson } from "../../types";

export const qaoa: Lesson = {
	id: "qaoa",
	title: "QAOA: Quantum Approximate Optimization",
	chapterId: "variational",
	content: `## QAOA: Quantum Approximate Optimization Algorithm

The **Quantum Approximate Optimization Algorithm** (QAOA), introduced by Farhi, Goldstone, and Gutmann (2014), is a leading candidate for demonstrating quantum advantage on combinatorial optimization problems.

### The MaxCut Problem

Given a graph $G = (V, E)$, **MaxCut** asks for a partition of vertices into two sets $S$ and $\\bar{S}$ that maximizes the number of edges crossing between them.

MaxCut is NP-hard in general, but QAOA provides a systematic quantum approximation scheme that improves with the number of layers $p$.

### Cost Hamiltonian

For each edge $(u, v)$, the cost Hamiltonian encodes a +1 energy whenever the edge is cut:

$$C = \\sum_{(u,v) \\in E} \\frac{1 - Z_u Z_v}{2}$$

When qubits $u$ and $v$ are in different states ($Z_u Z_v = -1$), this contributes 1; when in the same state ($Z_u Z_v = +1$), it contributes 0. So $\\langle C \\rangle$ is exactly the expected number of cut edges.

### The QAOA Circuit

Starting from the uniform superposition $|+\\rangle^{\\otimes n}$ (all vertices equally in both sets), QAOA applies $p$ alternating layers:

$$|\\gamma, \\beta\\rangle = e^{-i\\beta_p B}\\, e^{-i\\gamma_p C} \\cdots e^{-i\\beta_1 B}\\, e^{-i\\gamma_1 C}\\, |+\\rangle^{\\otimes n}$$

- **Phase separator** $e^{-i\\gamma C}$: applies phase $e^{-i\\gamma}$ to basis states where edge $(u,v)$ is cut
- **Mixer** $e^{-i\\beta B}$ where $B = \\sum_u X_u$: applies $R_x(2\\beta)$ independently to each qubit, exploring the superposition

As $p \\to \\infty$ with optimal parameters, QAOA converges to the exact MaxCut solution.

### 2-Node Example (p=1)

For the simplest graph with one edge $(0, 1)$, the MaxCut optimum is 1 (the edge is always cut when the two nodes are in opposite sets).

The QAOA state-vector evolution for basis states $|00\\rangle, |01\\rangle, |10\\rangle, |11\\rangle$:

**Initial:** $\\tfrac{1}{2}(|00\\rangle + |01\\rangle + |10\\rangle + |11\\rangle)$

**After phase separator** (cost = 1 for $|01\\rangle$ and $|10\\rangle$, cost = 0 for $|00\\rangle$ and $|11\\rangle$):

$$\\tfrac{1}{2}\\bigl(|00\\rangle + e^{-i\\gamma}|01\\rangle + e^{-i\\gamma}|10\\rangle + |11\\rangle\\bigr)$$

**After mixer** $R_x(2\\beta) \\otimes R_x(2\\beta)$: each qubit independently mixed via:

$$R_x(2\\beta) = \\begin{pmatrix}\\cos\\beta & -i\\sin\\beta \\\\ -i\\sin\\beta & \\cos\\beta\\end{pmatrix}$$

**Optimal parameters:** $\\gamma = \\pi/2$, $\\beta = \\pi/8$ achieve $\\langle C \\rangle = 1$ — the exact maximum cut!

### Why QAOA Finds the Optimum Here

At $\\gamma = \\pi/2$ the phase separator applies $e^{-i\\pi/2} = -i$ to the cut states, creating destructive interference for the non-cut states $|00\\rangle$ and $|11\\rangle$ after the mixer. At $\\beta = \\pi/8$ the mixer balances the interference precisely so that all probability flows into $|01\\rangle$ and $|10\\rangle$.

### Your Task

Implement:

1. \`qaoa_maxcut(gamma, beta)\` — simulate the p=1 QAOA circuit on a 2-node graph with edge (0,1), returning the expected cut value $\\langle C \\rangle = P(|01\\rangle) + P(|10\\rangle)$
2. \`qaoa_optimize()\` — grid-search over $\\gamma \\in [0, \\pi)$ and $\\beta \\in [0, \\pi/2)$ (20 steps each) to find the maximum $\\langle C \\rangle$, returned rounded to 4 decimal places`,

	starterCode: `import math, cmath

def qaoa_maxcut(gamma, beta):
    # Initial |+⟩⊗|+⟩ state: amplitudes for |00⟩, |01⟩, |10⟩, |11⟩
    A, B, C, D = complex(0.5), complex(0.5), complex(0.5), complex(0.5)
    # Phase separator: apply e^(-i*gamma) to |01⟩ and |10⟩
    # Mixer: Rx(2*beta) on qubit 0, then Rx(2*beta) on qubit 1
    # Return P(|01⟩) + P(|10⟩)
    pass

def qaoa_optimize():
    # Grid search: gamma in [0, pi), beta in [0, pi/2), 20 steps each
    # Return round(best, 4)
    pass

print(round(qaoa_maxcut(0, math.pi / 4), 4))
`,

	solution: `import math, cmath

def qaoa_maxcut(gamma, beta):
    A, B, C, D = complex(0.5), complex(0.5), complex(0.5), complex(0.5)

    # Phase separator: e^(-i*gamma) on |01⟩ (B) and |10⟩ (C)
    B *= cmath.exp(-1j * gamma)
    C *= cmath.exp(-1j * gamma)

    # Mixer: Rx(2*beta) ⊗ Rx(2*beta)
    c = math.cos(beta)
    s = -1j * math.sin(beta)

    # Apply Rx to qubit 0 (MSB): mixes |0x⟩ with |1x⟩
    A, C = c * A + s * C, s * A + c * C
    B, D = c * B + s * D, s * B + c * D

    # Apply Rx to qubit 1 (LSB): mixes |x0⟩ with |x1⟩
    A, B = c * A + s * B, s * A + c * B
    C, D = c * C + s * D, s * C + c * D

    return abs(B) ** 2 + abs(C) ** 2

def qaoa_optimize():
    best = 0.0
    for g in range(20):
        for b in range(20):
            val = qaoa_maxcut(g * math.pi / 20, b * math.pi / 40)
            if val > best:
                best = val
    return round(best, 4)

print(round(qaoa_maxcut(0, math.pi / 4), 4))
`,

	tests: [
		{
			name: "qaoa_maxcut(0, π/4) = 0.5 (no phase separation)",
			expected: "0.5\n",
		},
		{
			name: "qaoa_maxcut(π/2, π/8) = 1.0 (optimal parameters)",
			code: `{{FUNC}}
print(round(qaoa_maxcut(math.pi / 2, math.pi / 8), 4))`,
			expected: "1.0\n",
		},
		{
			name: "qaoa_optimize() finds max cut value 1.0",
			code: `{{FUNC}}
print(qaoa_optimize())`,
			expected: "1.0\n",
		},
		{
			name: "qaoa_maxcut(π, π/4) = 0.5",
			code: `{{FUNC}}
print(round(qaoa_maxcut(math.pi, math.pi / 4), 4))`,
			expected: "0.5\n",
		},
	],
};
