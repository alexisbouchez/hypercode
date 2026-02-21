import type { Lesson } from "../../types";

export const invariantMass: Lesson = {
	id: "invariant-mass",
	title: "Invariant Mass",
	chapterId: "kinematics",
	content: `## Invariant Mass

One of the most powerful concepts in special relativity is the **invariant mass** (or rest mass energy) of a particle or system. Unlike energy and momentum individually, the combination

$$M^2 = E^2 - |\\mathbf{p}|^2 c^2$$

is the same in every inertial frame. In natural units ($c = 1$) this becomes simply:

$$M^2 = E^2 - |\\mathbf{p}|^2$$

### Energy–Momentum Relation

For a single particle of rest mass $m$ with 3-momentum $p$:

$$E = \\sqrt{p^2 + m^2}$$

This replaces the non-relativistic $E = p^2 / 2m$.

### Lorentz Factor from Energy

The Lorentz factor can be recovered from a particle's total energy and rest mass:

$$\\gamma = \\frac{E}{mc^2} = \\frac{E}{m} \\quad (\\text{natural units})$$

### Reconstructing Resonances

Detectors measure the 4-momenta of decay products. The invariant mass of the system reveals the parent particle's rest mass — this is how the Higgs boson was discovered in 2012.

### Your Task

Implement the following functions (all quantities in GeV, natural units, $c = 1$):

- \`invariant_mass(E_GeV, px_GeV, py_GeV, pz_GeV)\` — returns $M = \\sqrt{\\max(0,\\, E^2 - p_x^2 - p_y^2 - p_z^2)}$
- \`particle_energy(m_GeV, p_GeV)\` — returns $E = \\sqrt{m^2 + p^2}$
- \`lorentz_gamma_from_E(E_GeV, m_GeV)\` — returns $\\gamma = E/m$`,

	starterCode: `import math

def invariant_mass(E_GeV, px_GeV, py_GeV, pz_GeV):
    # M = sqrt(max(0, E^2 - px^2 - py^2 - pz^2))
    pass

def particle_energy(m_GeV, p_GeV):
    # E = sqrt(m^2 + p^2)
    pass

def lorentz_gamma_from_E(E_GeV, m_GeV):
    # gamma = E / m
    pass

print(invariant_mass(1.0, 0.0, 0.0, 0.0))
print(round(particle_energy(0.938272, 1.0), 6))
print(round(lorentz_gamma_from_E(10.0, 0.938272), 6))
`,

	solution: `import math

def invariant_mass(E_GeV, px_GeV, py_GeV, pz_GeV):
    return math.sqrt(max(0, E_GeV**2 - px_GeV**2 - py_GeV**2 - pz_GeV**2))

def particle_energy(m_GeV, p_GeV):
    return math.sqrt(m_GeV**2 + p_GeV**2)

def lorentz_gamma_from_E(E_GeV, m_GeV):
    return E_GeV / m_GeV

print(invariant_mass(1.0, 0.0, 0.0, 0.0))
print(round(particle_energy(0.938272, 1.0), 6))
print(round(lorentz_gamma_from_E(10.0, 0.938272), 6))
`,

	tests: [
		{
			name: "invariant_mass(1.0, 0, 0, 0) = 1.0 GeV (particle at rest)",
			code: `{{FUNC}}
print(invariant_mass(1.0, 0.0, 0.0, 0.0))`,
			expected: "1.0\n",
		},
		{
			name: "particle_energy(0.938272, 1.0) — proton with 1 GeV/c momentum",
			code: `{{FUNC}}
print(round(particle_energy(0.938272, 1.0), 6))`,
			expected: "1.37126\n",
		},
		{
			name: "lorentz_gamma_from_E(10.0, 0.938272) — 10 GeV proton Lorentz factor",
			code: `{{FUNC}}
print(round(lorentz_gamma_from_E(10.0, 0.938272), 6))`,
			expected: "10.65789\n",
		},
		{
			name: "invariant_mass of moving pion (E=0.2 GeV) recovers pion rest mass 0.135 GeV",
			code: `{{FUNC}}
import math
p_pi = math.sqrt(0.2**2 - 0.135**2)
print(round(invariant_mass(0.2, 0.0, 0.0, p_pi), 4))`,
			expected: "0.135\n",
		},
	],
};
