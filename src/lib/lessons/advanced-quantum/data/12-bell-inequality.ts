import type { Lesson } from "../../types";

export const bellInequality: Lesson = {
	id: "bell-inequality",
	title: "Bell Inequalities & CHSH",
	chapterId: "protocols",
	content: `## Bell Inequalities & the CHSH Test

**Bell's theorem** (1964) proves that no theory of local hidden variables can reproduce all predictions of quantum mechanics. The **CHSH inequality** (Clauser–Horne–Shimony–Holt) provides a practical experimental test.

### Setup

Alice and Bob each receive one qubit of a shared Bell pair. They independently choose measurement angles from sets $\\{a, a'\\}$ and $\\{b, b'\\}$ respectively. Each measurement outcome is $\\pm 1$.

The **correlation function** for angles $a$ and $b$ is:

$$E(a, b) = \\langle A(a)\\, B(b) \\rangle$$

where $A(a), B(b) \\in \\{+1, -1\\}$ are the measurement results.

### The CHSH Inequality

Any local hidden variable (LHV) theory must satisfy:

$$|S| = |E(a,b) - E(a,b') + E(a',b) + E(a',b')| \\leq 2$$

This is the **classical bound**. If $|S| > 2$, the correlations cannot come from any LHV theory.

### Quantum Prediction

For a maximally entangled Bell pair, the quantum correlation function is:

$$E(a, b) = -\\cos\\!\\bigl(2(a - b)\\bigr)$$

The **Tsirelson bound** gives the maximum quantum value:

$$|S|_{\\text{max}} = 2\\sqrt{2} \\approx 2.828$$

This is achieved with the optimal angle settings:

$$a = 0,\\quad b = \\frac{\\pi}{8},\\quad a' = \\frac{\\pi}{4},\\quad b' = \\frac{3\\pi}{8}$$

### Verifying the Optimal CHSH Value

At these angles:

$$E\\!\\left(0, \\tfrac{\\pi}{8}\\right) = -\\cos\\!\\left(-\\tfrac{\\pi}{4}\\right) = -\\frac{1}{\\sqrt{2}}$$

$$E\\!\\left(0, \\tfrac{3\\pi}{8}\\right) = -\\cos\\!\\left(-\\tfrac{3\\pi}{4}\\right) = +\\frac{1}{\\sqrt{2}}$$

$$E\\!\\left(\\tfrac{\\pi}{4}, \\tfrac{\\pi}{8}\\right) = -\\cos\\!\\left(\\tfrac{\\pi}{4}\\right) = -\\frac{1}{\\sqrt{2}}$$

$$E\\!\\left(\\tfrac{\\pi}{4}, \\tfrac{3\\pi}{8}\\right) = -\\cos\\!\\left(-\\tfrac{\\pi}{4}\\right) = -\\frac{1}{\\sqrt{2}}$$

$$S = \\left(-\\frac{1}{\\sqrt{2}}\\right) - \\left(+\\frac{1}{\\sqrt{2}}\\right) + \\left(-\\frac{1}{\\sqrt{2}}\\right) + \\left(-\\frac{1}{\\sqrt{2}}\\right) = -2\\sqrt{2}$$

$$|S| = 2\\sqrt{2} \\approx 2.8284 \\quad \\checkmark$$

### Physical Significance

Experiments (Aspect 1982, and many since) consistently measure $|S| \\approx 2.8$, ruling out local hidden variables. **Quantum entanglement is not just a computational trick — it is a fundamental feature of nature that cannot be explained classically.**

### Your Task

Implement:

1. \`bell_correlation(a, b)\` — return $E(a, b) = -\\cos(2(a - b))$
2. \`chsh_value(a, b, a_prime, b_prime)\` — return $S = E(a,b) - E(a,b') + E(a',b) + E(a',b')$`,

	starterCode: `import math

def bell_correlation(a, b):
    # Quantum correlation: -cos(2*(a - b))
    pass

def chsh_value(a, b, a_prime, b_prime):
    # CHSH expression: E(a,b) - E(a,b') + E(a',b) + E(a',b')
    pass

print(round(bell_correlation(0, 0), 4))
`,

	solution: `import math

def bell_correlation(a, b):
    return -math.cos(2 * (a - b))

def chsh_value(a, b, a_prime, b_prime):
    return (bell_correlation(a, b) - bell_correlation(a, b_prime) +
            bell_correlation(a_prime, b) + bell_correlation(a_prime, b_prime))

print(round(bell_correlation(0, 0), 4))
`,

	tests: [
		{
			name: "bell_correlation(0, 0) = -cos(0) = -1.0",
			expected: "-1.0\n",
		},
		{
			name: "bell_correlation(0, π/4) = -cos(π/2) ≈ 0.0",
			code: `{{FUNC}}
print(round(abs(bell_correlation(0, math.pi / 4)), 4))`,
			expected: "0.0\n",
		},
		{
			name: "bell_correlation(0, π/2) = -cos(π) = 1.0",
			code: `{{FUNC}}
print(round(bell_correlation(0, math.pi / 2), 4))`,
			expected: "1.0\n",
		},
		{
			name: "chsh_value at optimal angles → |S| = 2√2 ≈ 2.8284",
			code: `{{FUNC}}
v = chsh_value(0, math.pi / 8, math.pi / 4, 3 * math.pi / 8)
print(round(abs(v), 4))`,
			expected: "2.8284\n",
		},
	],
};
