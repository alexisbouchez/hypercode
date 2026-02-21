import type { Lesson } from "../../types";

export const pauliExpectation: Lesson = {
	id: "pauli-expectation",
	title: "Pauli Expectation Values",
	chapterId: "variational",
	content: `## Pauli Expectation Values

The **expectation value** of an observable $H$ in quantum state $|\\psi\\rangle$ is:

$$\\langle H \\rangle = \\langle\\psi|\\, H\\, |\\psi\\rangle$$

This is the average outcome of measuring $H$ many times when the system is in state $|\\psi\\rangle$. Expectation values are the fundamental output of variational quantum algorithms such as VQE and QAOA.

### The Pauli Matrices

Every single-qubit Hermitian observable can be written as a linear combination of the three **Pauli matrices** and the identity:

$$X = \\begin{pmatrix}0 & 1 \\\\ 1 & 0\\end{pmatrix}, \\qquad Y = \\begin{pmatrix}0 & -i \\\\ i & 0\\end{pmatrix}, \\qquad Z = \\begin{pmatrix}1 & 0 \\\\ 0 & -1\\end{pmatrix}$$

Each has eigenvalues $\\pm 1$.

### Computing Expectation Values

For a qubit state $|\\psi\\rangle = \\alpha|0\\rangle + \\beta|1\\rangle$, write out the matrix products:

**Z expectation:**

$$\\langle Z \\rangle = \\langle\\psi|Z|\\psi\\rangle = |\\alpha|^2 - |\\beta|^2$$

This is simply the probability of measuring $|0\\rangle$ minus the probability of measuring $|1\\rangle$.

**X expectation:**

$$\\langle X \\rangle = \\langle\\psi|X|\\psi\\rangle = \\alpha^* \\beta + \\beta^* \\alpha = 2\\,\\text{Re}(\\alpha^*\\beta)$$

**Y expectation:**

$$\\langle Y \\rangle = \\langle\\psi|Y|\\psi\\rangle = i(\\beta^*\\alpha - \\alpha^*\\beta) = 2\\,\\text{Im}(\\alpha^*\\beta)$$

### Special States

| State | $\\langle Z \\rangle$ | $\\langle X \\rangle$ | $\\langle Y \\rangle$ |
|---|---|---|---|
| $|0\\rangle = (1, 0)$ | $+1$ | $0$ | $0$ |
| $|1\\rangle = (0, 1)$ | $-1$ | $0$ | $0$ |
| $|+\\rangle = \\frac{1}{\\sqrt{2}}(1, 1)$ | $0$ | $+1$ | $0$ |
| $|-\\rangle = \\frac{1}{\\sqrt{2}}(1, -1)$ | $0$ | $-1$ | $0$ |
| $|+i\\rangle = \\frac{1}{\\sqrt{2}}(1, i)$ | $0$ | $0$ | $+1$ |

These correspond exactly to the six face-centres of the Bloch sphere: states aligned with the positive and negative $X$, $Y$, and $Z$ axes have expectation value $+1$ or $-1$ along their axis and $0$ along the others.

### Bloch Sphere Connection

A pure state on the Bloch sphere at polar angle $\\theta$ and azimuthal angle $\\phi$ is:

$$|\\psi\\rangle = \\cos\\frac{\\theta}{2}|0\\rangle + e^{i\\phi}\\sin\\frac{\\theta}{2}|1\\rangle$$

Its Pauli expectation values give the Cartesian coordinates of the Bloch vector:

$$\\langle X \\rangle = \\sin\\theta\\cos\\phi,\\qquad \\langle Y \\rangle = \\sin\\theta\\sin\\phi,\\qquad \\langle Z \\rangle = \\cos\\theta$$

### Your Task

Implement \`pauli_expectation(state, pauli)\` where \`state\` is a 2-element list $[\\alpha, \\beta]$ (possibly complex) and \`pauli\` is \`'X'\`, \`'Y'\`, or \`'Z'\`. Return the real-valued expectation value.`,

	starterCode: `def pauli_expectation(state, pauli):
    # state = [alpha, beta] (may be complex)
    # Return <psi|P|psi> for P in {'X', 'Y', 'Z'}
    pass

print(pauli_expectation([1, 0], 'Z'))
`,

	solution: `def pauli_expectation(state, pauli):
    alpha, beta = complex(state[0]), complex(state[1])
    if pauli == 'Z':
        return (abs(alpha) ** 2 - abs(beta) ** 2).real
    elif pauli == 'X':
        return (alpha.conjugate() * beta + beta.conjugate() * alpha).real
    elif pauli == 'Y':
        return (1j * (beta.conjugate() * alpha - alpha.conjugate() * beta)).real
    return 0.0

print(pauli_expectation([1, 0], 'Z'))
`,

	tests: [
		{
			name: "⟨0|Z|0⟩ = 1.0",
			expected: "1.0\n",
		},
		{
			name: "⟨1|Z|1⟩ = -1.0",
			code: `{{FUNC}}
print(pauli_expectation([0, 1], 'Z'))`,
			expected: "-1.0\n",
		},
		{
			name: "⟨+|X|+⟩ = 1.0",
			code: `{{FUNC}}
import math
sq2 = 1 / math.sqrt(2)
print(round(pauli_expectation([sq2, sq2], 'X'), 4))`,
			expected: "1.0\n",
		},
		{
			name: "⟨0|X|0⟩ = 0.0",
			code: `{{FUNC}}
print(pauli_expectation([1, 0], 'X'))`,
			expected: "0.0\n",
		},
	],
};
