import type { Lesson } from "../../types";

export const teleportation: Lesson = {
	id: "teleportation",
	title: "Quantum Teleportation",
	chapterId: "protocols",
	content: `## Quantum Teleportation

**Quantum teleportation** transmits an unknown qubit state $|\\psi\\rangle = \\alpha|0\\rangle + \\beta|1\\rangle$ from Alice to Bob using only a pre-shared Bell pair and 2 classical bits of communication.

Crucially, no quantum channel is used after the setup — only classical communication. The quantum information "teleports" because the entanglement carries the correlations.

### The Protocol

**Setup:** Alice and Bob share the Bell pair $|\\Phi^+\\rangle = \\frac{|00\\rangle + |11\\rangle}{\\sqrt{2}}$.

**Step 1 — Bell Measurement:** Alice performs a joint measurement on her target qubit and her half of the Bell pair. This projects onto one of four Bell states, yielding two classical bits $(m_0, m_1)$.

**Step 2 — Classical Communication:** Alice sends $(m_0, m_1)$ to Bob.

**Step 3 — Correction:** Bob applies $X^{m_1} Z^{m_0}$ to his qubit.

### Bob's State Before Correction

| $(m_0, m_1)$ | Bob's qubit |
|---|---|
| $(0, 0)$ | $\\alpha|0\\rangle + \\beta|1\\rangle$ |
| $(0, 1)$ | $\\beta|0\\rangle + \\alpha|1\\rangle$ |
| $(1, 0)$ | $\\alpha|0\\rangle - \\beta|1\\rangle$ |
| $(1, 1)$ | $-\\beta|0\\rangle + \\alpha|1\\rangle$ |

### Applying the Correction

The $X$ gate swaps amplitudes; the $Z$ gate negates the second:

- $m_1 = 1$: swap $\\alpha \\leftrightarrow \\beta$
- $m_0 = 1$: negate $\\beta$

After both corrections, Bob always holds the original $|\\psi\\rangle$.

\`\`\`python
import math

def teleport_correction(bob_state, m0, m1):
    alpha, beta = complex(bob_state[0]), complex(bob_state[1])
    if m1:
        alpha, beta = beta, alpha   # X gate
    if m0:
        beta = -beta                # Z gate
    norm = math.sqrt(abs(alpha)**2 + abs(beta)**2)
    return [alpha / norm, beta / norm]

def teleport(state):
    alpha, beta = complex(state[0]), complex(state[1])
    sq2 = 1 / math.sqrt(2)
    raw = [alpha * sq2, beta * sq2]
    norm = math.sqrt(abs(raw[0])**2 + abs(raw[1])**2)
    return [raw[0]/norm, raw[1]/norm]

result = teleport([1, 0])
print(round(abs(result[0]), 4))  # 1.0
print(round(abs(result[1]), 4))  # 0.0
\`\`\`

### No-Cloning Theorem

Teleportation does not violate the **no-cloning theorem**: Alice's original qubit is destroyed by her Bell measurement. The state is transferred, not copied.

### Your Task

Implement two functions:

1. \`teleport_correction(bob_state, m0, m1)\` — applies $X^{m_1} Z^{m_0}$ to Bob's 2-element state list, then normalizes.
2. \`teleport(state)\` — simulates the deterministic $(0,0)$ outcome path, returning the normalized recovered state.`,

	starterCode: `import math

def teleport_correction(bob_state, m0, m1):
    # Apply X if m1=1 (swap amplitudes), Z if m0=1 (negate second), then normalize
    pass

def teleport(state):
    # Simulate (0,0) outcome: scale by 1/√2 then normalize
    pass

result = teleport([1, 0])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))
`,

	solution: `import math

def teleport_correction(bob_state, m0, m1):
    alpha, beta = complex(bob_state[0]), complex(bob_state[1])
    if m1:
        alpha, beta = beta, alpha
    if m0:
        beta = -beta
    norm = math.sqrt(abs(alpha)**2 + abs(beta)**2)
    return [alpha / norm, beta / norm]

def teleport(state):
    alpha, beta = complex(state[0]), complex(state[1])
    sq2 = 1 / math.sqrt(2)
    raw = [alpha * sq2, beta * sq2]
    norm = math.sqrt(abs(raw[0])**2 + abs(raw[1])**2)
    return [raw[0]/norm, raw[1]/norm]

result = teleport([1, 0])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))
`,

	tests: [
		{
			name: "teleport([1, 0]) → |0⟩ recovered: abs[0]=1.0, abs[1]=0.0",
			expected: "1.0\n0.0\n",
		},
		{
			name: "teleport([0, 1]) → |1⟩ recovered: abs[0]=0.0, abs[1]=1.0",
			code: `{{FUNC}}
result = teleport([0, 1])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))`,
			expected: "0.0\n1.0\n",
		},
		{
			name: "teleport([1/√2, 1/√2]) → |+⟩ recovered: both abs = 0.7071",
			code: `{{FUNC}}
import math
sq2 = 1 / math.sqrt(2)
result = teleport([sq2, sq2])
print(round(abs(result[0]), 4))
print(round(abs(result[1]), 4))`,
			expected: "0.7071\n0.7071\n",
		},
		{
			name: "teleport_correction([-1/√2, 1/√2], 1, 1) → [0.7071, 0.7071]",
			code: `{{FUNC}}
import math
sq2 = 1 / math.sqrt(2)
corrected = teleport_correction([-sq2, sq2], 1, 1)
print(round(abs(corrected[0]), 4))
print(round(abs(corrected[1]), 4))`,
			expected: "0.7071\n0.7071\n",
		},
	],
};
