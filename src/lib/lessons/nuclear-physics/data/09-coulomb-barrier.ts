import type { Lesson } from "../../types";

export const coulombBarrier: Lesson = {
	id: "coulomb-barrier",
	title: "Coulomb Barrier",
	chapterId: "nuclear-reactions",
	content: `## Coulomb Barrier

Two nuclei approaching each other experience a strong electrostatic repulsion. The **Coulomb barrier** is the potential energy at the point of nuclear contact — the height that must be overcome (or tunneled through) for a nuclear reaction to occur.

$$V_C = \\frac{k_e Z_1 Z_2 e^2}{R_1 + R_2}$$

where:
- $k_e = 8.988 \\times 10^9$ N·m²/C² (Coulomb constant)
- $e = 1.602 \\times 10^{-19}$ C (elementary charge)
- $R_i = 1.2 \\times 10^{-15} \\cdot A_i^{1/3}$ m (nuclear radius)

### Converting to MeV

$$V_C [\\text{MeV}] = \\frac{V_C [\\text{J}]}{1.602 \\times 10^{-13} \\text{ J/MeV}}$$

### Contact Distance

The sum of the two nuclear radii gives the **contact distance** in femtometers (fm, $10^{-15}$ m):

$$d = R_1 + R_2 = 1.2 \\left(A_1^{1/3} + A_2^{1/3}\\right) \\text{ fm}$$

### Why It Matters

The D+T fusion reaction has a Coulomb barrier of only ~0.44 MeV (both particles have $Z=1$), making it the easiest fusion reaction. Heavy-ion fusion (e.g., Pb+Pb) requires barriers of hundreds of MeV.

### Your Task

Implement:
- \`coulomb_barrier_J(Z1, A1, Z2, A2)\` — barrier in Joules
- \`coulomb_barrier_MeV(Z1, A1, Z2, A2)\` — barrier in MeV
- \`contact_distance_fm(A1, A2)\` — contact distance in femtometers

All physical constants must be defined **inside** each function.`,

	starterCode: `def coulomb_barrier_J(Z1, A1, Z2, A2):
    ke = 8.988e9
    e = 1.602e-19
    # R = 1.2e-15 * (A1^(1/3) + A2^(1/3))
    # V = ke * Z1 * Z2 * e^2 / R
    pass

def coulomb_barrier_MeV(Z1, A1, Z2, A2):
    ke = 8.988e9
    e = 1.602e-19
    # Compute in Joules, then divide by 1.602e-13
    pass

def contact_distance_fm(A1, A2):
    # R = 1.2 * (A1^(1/3) + A2^(1/3)) in fm
    pass
`,

	solution: `def coulomb_barrier_J(Z1, A1, Z2, A2):
    ke = 8.988e9
    e = 1.602e-19
    R = 1.2e-15 * (A1 ** (1/3) + A2 ** (1/3))
    return ke * Z1 * Z2 * e**2 / R

def coulomb_barrier_MeV(Z1, A1, Z2, A2):
    ke = 8.988e9
    e = 1.602e-19
    R = 1.2e-15 * (A1 ** (1/3) + A2 ** (1/3))
    J = ke * Z1 * Z2 * e**2 / R
    return J / (1.602e-13)

def contact_distance_fm(A1, A2):
    return 1.2 * (A1 ** (1/3) + A2 ** (1/3))
`,

	tests: [
		{
			name: "Coulomb barrier p+p ≈ 0.5999 MeV",
			code: `{{FUNC}}
print(round(coulomb_barrier_MeV(1, 1, 1, 1), 4))`,
			expected: "0.5999\n",
		},
		{
			name: "Coulomb barrier D+T ≈ 0.444 MeV",
			code: `{{FUNC}}
print(round(coulomb_barrier_MeV(1, 2, 1, 3), 4))`,
			expected: "0.444\n",
		},
		{
			name: "Coulomb barrier alpha + U-238 ≈ 28.3614 MeV",
			code: `{{FUNC}}
print(round(coulomb_barrier_MeV(2, 4, 92, 238), 4))`,
			expected: "28.3614\n",
		},
		{
			name: "Contact distance Pb-208 + Pb-208 ≈ 14.22 fm",
			code: `{{FUNC}}
print(round(contact_distance_fm(208, 208), 4))`,
			expected: "14.22\n",
		},
	],
};
