import type { Lesson } from "../../types";

export const faradaysLawLesson: Lesson = {
	id: "faradays-law",
	title: "Faraday's Law",
	chapterId: "em-induction",
	content: `## Faraday's Law of Induction

A **changing magnetic flux** through a coil induces an electromotive force (EMF). Michael Faraday discovered this in 1831:

\`\`\`
EMF = N × ΔΦ / Δt
\`\`\`

- **N** — number of turns in the coil
- **ΔΦ** — change in magnetic flux (Wb)
- **Δt** — time over which the flux changes (s)
- **EMF** — induced voltage (volts)

### Lenz's Law

The induced EMF always opposes the change that caused it (the negative sign in the full equation, EMF = −N dΦ/dt). This is why braking electromagnets resist motion, and transformers have opposing primary and secondary currents.

### Applications

Faraday's law is the operating principle of:
- **Generators** — rotating coil changes flux → AC electricity
- **Transformers** — changing primary flux induces secondary EMF
- **Induction cooktops** — changing field induces currents in the pot

### Examples

| N | ΔΦ (Wb) | Δt (s) | EMF (V) |
|---|---------|--------|---------|
| 1 | 0.1 | 1 | **0.1000** |
| 100 | 0.5 | 0.1 | **500.0000** |
| 50 | 0.02 | 0.01 | **100.0000** |
| 200 | 1 | 2 | **100.0000** |

### Your Task

Implement \`induced_emf(N, delta_phi, delta_t)\` returning the magnitude of the induced EMF.`,

	starterCode: `def induced_emf(N, delta_phi, delta_t):
    # EMF = N * delta_phi / delta_t
    return 0

print(f"{induced_emf(1, 0.1, 1):.4f}")          # 0.1000
print(f"{induced_emf(100, 0.5, 0.1):.4f}")      # 500.0000
print(f"{induced_emf(50, 0.02, 0.01):.4f}")     # 100.0000
print(f"{induced_emf(200, 1, 2):.4f}")          # 100.0000
`,

	solution: `def induced_emf(N, delta_phi, delta_t):
    return N * delta_phi / delta_t

print(f"{induced_emf(1, 0.1, 1):.4f}")          # 0.1000
print(f"{induced_emf(100, 0.5, 0.1):.4f}")      # 500.0000
print(f"{induced_emf(50, 0.02, 0.01):.4f}")     # 100.0000
print(f"{induced_emf(200, 1, 2):.4f}")          # 100.0000
`,

	tests: [
		{
			name: "N=1, ΔΦ=0.1 Wb, Δt=1 s → EMF=0.1000 V",
			code: `{{FUNC}}
print(f"{induced_emf(1, 0.1, 1):.4f}")`,
			expected: "0.1000\n",
		},
		{
			name: "N=100, ΔΦ=0.5 Wb, Δt=0.1 s → EMF=500.0000 V",
			code: `{{FUNC}}
print(f"{induced_emf(100, 0.5, 0.1):.4f}")`,
			expected: "500.0000\n",
		},
		{
			name: "N=50, ΔΦ=0.02 Wb, Δt=0.01 s → EMF=100.0000 V",
			code: `{{FUNC}}
print(f"{induced_emf(50, 0.02, 0.01):.4f}")`,
			expected: "100.0000\n",
		},
		{
			name: "N=200, ΔΦ=1 Wb, Δt=2 s → EMF=100.0000 V",
			code: `{{FUNC}}
print(f"{induced_emf(200, 1, 2):.4f}")`,
			expected: "100.0000\n",
		},
	],
};
