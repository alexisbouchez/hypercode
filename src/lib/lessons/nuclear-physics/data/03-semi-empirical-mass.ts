import type { Lesson } from "../../types";

export const semiEmpiricalMass: Lesson = {
	id: "semi-empirical-mass",
	title: "Semi-Empirical Mass Formula",
	chapterId: "nuclear-structure",
	content: `## Semi-Empirical Mass Formula

The **Bethe-Weizsäcker formula** (1935) models nuclear binding energy as a sum of five physical terms, treating the nucleus as a liquid drop.

### The Formula

$$B(Z, A) = a_V A - a_S A^{2/3} - a_C \\frac{Z(Z-1)}{A^{1/3}} - a_A \\frac{(A-2Z)^2}{A} + \\delta$$

**Volume term** $a_V A$: Each nucleon binds to its neighbours. Energy grows with volume ($\\propto A$).

**Surface term** $-a_S A^{2/3}$: Nucleons on the surface have fewer neighbours — a correction proportional to surface area ($\\propto A^{2/3}$).

**Coulomb term** $-a_C Z(Z-1)/A^{1/3}$: Electrostatic repulsion between protons reduces binding.

**Asymmetry term** $-a_A (A-2Z)^2/A$: The Pauli exclusion principle favours equal numbers of protons and neutrons.

**Pairing term** $\\delta$: Nucleon pairs (spin-up/down) have extra stability.

$$\\delta = \\begin{cases} +11.2/\\sqrt{A} & \\text{if } Z \\text{ and } N \\text{ both even} \\\\ -11.2/\\sqrt{A} & \\text{if } Z \\text{ and } N \\text{ both odd} \\\\ 0 & \\text{if } A \\text{ odd} \\end{cases}$$

### Empirical Coefficients

| Coefficient | Value (MeV) | Term |
|-------------|-------------|------|
| $a_V$ | 15.85 | Volume |
| $a_S$ | 18.34 | Surface |
| $a_C$ | 0.711 | Coulomb |
| $a_A$ | 23.23 | Asymmetry |

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`pairing_term(Z, A)\` — returns $\\delta$ in MeV (+11.2/√A, −11.2/√A, or 0)
- \`semf_binding_energy(Z, A)\` — returns total binding energy $B(Z,A)$ in MeV
- \`semf_binding_per_nucleon(Z, A)\` — returns $B(Z,A)/A$ in MeV/nucleon`,

	starterCode: `import math

def pairing_term(Z, A):
    N = A - Z
    # +11.2/sqrt(A) if Z and N both even
    # -11.2/sqrt(A) if Z and N both odd
    # 0 otherwise
    pass

def semf_binding_energy(Z, A):
    aV = 15.85
    aS = 18.34
    aC = 0.711
    aA = 23.23
    # TODO: implement all five terms
    pass

def semf_binding_per_nucleon(Z, A):
    # TODO: return semf_binding_energy / A
    pass

print(round(semf_binding_energy(26, 56), 2))
print(round(pairing_term(6, 12), 4))
print(round(semf_binding_per_nucleon(26, 56), 4))
print(round(semf_binding_per_nucleon(92, 238), 4))
`,

	solution: `import math

def pairing_term(Z, A):
    N = A - Z
    if Z % 2 == 0 and N % 2 == 0:
        return 11.2 / math.sqrt(A)
    elif Z % 2 == 1 and N % 2 == 1:
        return -11.2 / math.sqrt(A)
    else:
        return 0.0

def semf_binding_energy(Z, A):
    aV = 15.85
    aS = 18.34
    aC = 0.711
    aA = 23.23
    volume = aV * A
    surface = aS * A ** (2/3)
    coulomb = aC * Z * (Z - 1) / A ** (1/3)
    asymmetry = aA * (A - 2 * Z) ** 2 / A
    delta = pairing_term(Z, A)
    return volume - surface - coulomb - asymmetry + delta

def semf_binding_per_nucleon(Z, A):
    return semf_binding_energy(Z, A) / A

print(round(semf_binding_energy(26, 56), 2))
print(round(pairing_term(6, 12), 4))
print(round(semf_binding_per_nucleon(26, 56), 4))
print(round(semf_binding_per_nucleon(92, 238), 4))
`,

	tests: [
		{
			name: "semf_binding_energy(26, 56) ≈ 493.22 MeV for Fe-56",
			code: `{{FUNC}}
print(round(semf_binding_energy(26, 56), 2))`,
			expected: "493.22\n",
		},
		{
			name: "pairing_term(6, 12) ≈ 3.2332 MeV (even-even C-12)",
			code: `{{FUNC}}
print(round(pairing_term(6, 12), 4))`,
			expected: "3.2332\n",
		},
		{
			name: "semf_binding_per_nucleon(26, 56) ≈ 8.8074 MeV/nucleon (Fe-56 near peak)",
			code: `{{FUNC}}
print(round(semf_binding_per_nucleon(26, 56), 4))`,
			expected: "8.8074\n",
		},
		{
			name: "semf_binding_per_nucleon(92, 238) ≈ 7.662 MeV/nucleon (U-238)",
			code: `{{FUNC}}
print(round(semf_binding_per_nucleon(92, 238), 4))`,
			expected: "7.662\n",
		},
	],
};
