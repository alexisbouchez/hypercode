import type { Lesson } from "../../types";

export const massDefect: Lesson = {
	id: "mass-defect",
	title: "Mass Defect and Binding Energy",
	chapterId: "nuclear-structure",
	content: `## Mass Defect and Binding Energy

One of the most profound results of nuclear physics is that a nucleus weighs **less** than the sum of its parts. This missing mass is converted into the energy that holds the nucleus together.

### Mass Defect

For a nucleus with $Z$ protons and $N$ neutrons, the **mass defect** is:

$$\\Delta m = Z \\cdot m_p + N \\cdot m_n - M_{\\text{nucleus}}$$

where all masses are in atomic mass units (u):
- $m_p = 1.007276$ u (proton mass)
- $m_n = 1.008665$ u (neutron mass)
- $M_{\\text{nucleus}}$ is the measured nuclear mass

### Binding Energy

By Einstein's $E = mc^2$, the mass defect corresponds to an energy:

$$BE = \\Delta m \\cdot 931.494 \\text{ MeV/u}$$

The conversion factor $931.494$ MeV/u comes from $1 \\text{ u} \\cdot c^2$.

### Binding Energy Per Nucleon

The **binding energy per nucleon** $BE/A$ is a measure of nuclear stability:

$$\\frac{BE}{A} = \\frac{\\Delta m \\cdot 931.494}{Z + N}$$

| Nucleus | $BE$ (MeV) | $BE/A$ (MeV/nucleon) |
|---------|-----------|----------------------|
| He-4    | 27.27     | 6.82                 |
| C-12    | 89.09     | 7.42                 |
| Fe-56   | 478.96    | 8.55  ← most stable  |
| U-238   | 1754.65   | 7.37                 |

Iron-56 sits at the peak of the binding energy curve — lighter nuclei release energy by **fusion**, heavier nuclei by **fission**.

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`mass_defect(Z, N, M_nucleus_u)\` — returns $\\Delta m$ in u
- \`binding_energy_MeV(Z, N, M_nucleus_u)\` — returns $BE$ in MeV
- \`binding_energy_per_nucleon(Z, N, M_nucleus_u)\` — returns $BE/A$ in MeV/nucleon

Use $m_p = 1.007276$ u, $m_n = 1.008665$ u, and $1 \\text{ u} = 931.494$ MeV/$c^2$.`,

	starterCode: `import math

def mass_defect(Z, N, M_nucleus_u):
    mp_u = 1.007276  # u
    mn_u = 1.008665  # u
    # TODO: return Z*mp_u + N*mn_u - M_nucleus_u
    pass

def binding_energy_MeV(Z, N, M_nucleus_u):
    MeV_per_u = 931.494
    mp_u = 1.007276
    mn_u = 1.008665
    # TODO: return mass_defect * MeV_per_u
    pass

def binding_energy_per_nucleon(Z, N, M_nucleus_u):
    MeV_per_u = 931.494
    mp_u = 1.007276
    mn_u = 1.008665
    # TODO: return binding_energy / (Z + N)
    pass

print(round(binding_energy_MeV(2, 2, 4.002602), 4))
print(round(binding_energy_per_nucleon(26, 30, 55.934939), 4))
print(round(mass_defect(6, 6, 12.000000), 6))
print(round(binding_energy_per_nucleon(92, 146, 238.050788), 4))
`,

	solution: `import math

def mass_defect(Z, N, M_nucleus_u):
    mp_u = 1.007276  # u
    mn_u = 1.008665  # u
    return Z * mp_u + N * mn_u - M_nucleus_u

def binding_energy_MeV(Z, N, M_nucleus_u):
    MeV_per_u = 931.494
    mp_u = 1.007276
    mn_u = 1.008665
    dm = Z * mp_u + N * mn_u - M_nucleus_u
    return dm * MeV_per_u

def binding_energy_per_nucleon(Z, N, M_nucleus_u):
    MeV_per_u = 931.494
    mp_u = 1.007276
    mn_u = 1.008665
    dm = Z * mp_u + N * mn_u - M_nucleus_u
    BE = dm * MeV_per_u
    return BE / (Z + N)

print(round(binding_energy_MeV(2, 2, 4.002602), 4))
print(round(binding_energy_per_nucleon(26, 30, 55.934939), 4))
print(round(mass_defect(6, 6, 12.000000), 6))
print(round(binding_energy_per_nucleon(92, 146, 238.050788), 4))
`,

	tests: [
		{
			name: "binding_energy_MeV for He-4 ≈ 27.2741 MeV",
			code: `{{FUNC}}
print(round(binding_energy_MeV(2, 2, 4.002602), 4))`,
			expected: "27.2741\n",
		},
		{
			name: "binding_energy_per_nucleon for Fe-56 ≈ 8.5529 MeV/nucleon (most stable)",
			code: `{{FUNC}}
print(round(binding_energy_per_nucleon(26, 30, 55.934939), 4))`,
			expected: "8.5529\n",
		},
		{
			name: "mass_defect for C-12 ≈ 0.095646 u",
			code: `{{FUNC}}
print(round(mass_defect(6, 6, 12.000000), 6))`,
			expected: "0.095646\n",
		},
		{
			name: "binding_energy_per_nucleon for U-238 ≈ 7.3725 MeV/nucleon",
			code: `{{FUNC}}
print(round(binding_energy_per_nucleon(92, 146, 238.050788), 4))`,
			expected: "7.3725\n",
		},
	],
};
