import type { Lesson } from "../../types";

export const vqe: Lesson = {
	id: "vqe",
	title: "Variational Quantum Eigensolver (VQE)",
	chapterId: "variational",
	content: `## Variational Quantum Eigensolver (VQE)

The **Variational Quantum Eigensolver** is the prototypical hybrid quantum-classical algorithm. It uses a quantum computer to evaluate an energy function and a classical optimizer to minimize it, finding the ground state of a Hamiltonian.

### The Variational Principle

The key mathematical foundation is the **Rayleigh–Ritz variational principle**: for any normalized trial state $|\\psi(\\theta)\\rangle$,

$$E(\\theta) = \\langle\\psi(\\theta)|\\, H\\, |\\psi(\\theta)\\rangle \\geq E_0$$

where $E_0$ is the true ground state energy. Minimizing $E(\\theta)$ over the parameters $\\theta$ gives the best approximation within the ansatz family.

### The Ansatz

For the simplest case $H = Z$, we use the single-parameter ansatz:

$$|\\psi(\\theta)\\rangle = R_y(\\theta)|0\\rangle = \\cos\\frac{\\theta}{2}|0\\rangle + \\sin\\frac{\\theta}{2}|1\\rangle$$

The $R_y$ gate rotates the Bloch vector within the $XZ$-plane, covering all real-amplitude states from $|0\\rangle$ (at $\\theta=0$) to $|1\\rangle$ (at $\\theta=\\pi$).

### Energy Landscape

Substituting into the $Z$ expectation value formula from the previous lesson:

$$E(\\theta) = \\langle\\psi(\\theta)|Z|\\psi(\\theta)\\rangle = \\cos^2\\frac{\\theta}{2} - \\sin^2\\frac{\\theta}{2} = \\cos\\theta$$

The energy landscape is a simple cosine:

- Maximum at $\\theta = 0$: $E = +1$ (state $|0\\rangle$, eigenvalue $+1$ of $Z$)
- Minimum at $\\theta = \\pi$: $E = -1$ (state $|1\\rangle$, eigenvalue $-1$ of $Z$, the ground state)

### Gradient Descent Optimization

The classical optimizer computes the gradient:

$$\\frac{dE}{d\\theta} = -\\sin\\theta$$

and updates:

$$\\theta \\leftarrow \\theta - \\eta \\cdot \\frac{dE}{d\\theta} = \\theta + \\eta\\sin\\theta$$

Starting near $\\theta = 0$ and running gradient descent converges to $\\theta = \\pi$.

**Note:** On a real quantum computer, the gradient is estimated using the **parameter shift rule**: $\\frac{dE}{d\\theta} = \\frac{1}{2}[E(\\theta + \\pi/2) - E(\\theta - \\pi/2)]$, which requires only two extra circuit evaluations.

### VQE Workflow

\`\`\`
Initialize θ randomly
Repeat:
  1. Prepare |ψ(θ)⟩ on quantum hardware
  2. Measure ⟨H⟩ = E(θ)         ← quantum step
  3. Compute gradient classically
  4. Update θ ← θ - η·∇E        ← classical step
Until convergence
\`\`\`

### Scaling to Real Problems

For molecular Hamiltonians like $H_2$, VQE expresses $H$ as a sum of Pauli strings (e.g., $H = c_0 I + c_1 Z_0 + c_2 Z_1 + c_3 Z_0 Z_1 + c_4 X_0 X_1$) and measures each term separately. The ansatz uses chemically motivated circuits (UCCSD). VQE is expected to show quantum advantage for strongly correlated molecules beyond the reach of classical methods.

### Your Task

Implement:

1. \`energy(theta)\` — return $E(\\theta) = \\cos\\theta$
2. \`vqe_minimize()\` — run gradient descent for 100 steps with learning rate 0.3 starting at $\\theta = 0.1$, and return the minimized energy rounded to 4 decimal places`,

	starterCode: `import math

def energy(theta):
    # Energy of Ry(theta)|0⟩ under H = Z: cos(theta)
    pass

def vqe_minimize():
    # Gradient descent: dE/dtheta = -sin(theta)
    # 100 steps, lr=0.3, start at theta=0.1
    # Return round(energy(theta), 4)
    pass

print(energy(0))
`,

	solution: `import math

def energy(theta):
    return math.cos(theta)

def vqe_minimize():
    theta = 0.1
    lr = 0.3
    for _ in range(100):
        grad = -math.sin(theta)
        theta -= lr * grad
    return round(energy(theta), 4)

print(energy(0))
`,

	tests: [
		{
			name: "energy(0) = cos(0) = 1.0",
			expected: "1.0\n",
		},
		{
			name: "energy(π) = cos(π) = -1.0",
			code: `{{FUNC}}
print(energy(math.pi))`,
			expected: "-1.0\n",
		},
		{
			name: "energy(π/2) = cos(π/2) ≈ 0.0",
			code: `{{FUNC}}
print(round(energy(math.pi / 2), 4))`,
			expected: "0.0\n",
		},
		{
			name: "vqe_minimize() converges to ground state energy -1.0",
			code: `{{FUNC}}
print(vqe_minimize())`,
			expected: "-1.0\n",
		},
	],
};
