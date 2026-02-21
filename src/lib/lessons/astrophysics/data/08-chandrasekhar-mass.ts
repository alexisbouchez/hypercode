import type { Lesson } from "../../types";

export const chandrasekharMass: Lesson = {
	id: "chandrasekhar-mass",
	title: "The Chandrasekhar Mass",
	chapterId: "compact-objects",
	content: `## The Chandrasekhar Mass

When a star exhausts its nuclear fuel, it may leave behind a **white dwarf** — a dense remnant supported not by fusion but by **electron degeneracy pressure** (a quantum mechanical effect from the Pauli exclusion principle).

### Maximum Mass

There is a fundamental upper limit on white dwarf mass, the **Chandrasekhar mass**:

$$M_{\\rm Ch} = \\frac{5.87}{\\mu_e^2}\\ M_\\odot$$

where $\\mu_e$ is the mean molecular weight per electron:
- $\\mu_e = 2$ for carbon/oxygen white dwarfs → $M_{\\rm Ch} \\approx 1.47\\ M_\\odot$
- $\\mu_e = 1$ for hydrogen → $M_{\\rm Ch} = 5.87\\ M_\\odot$

Above this mass, degeneracy pressure cannot halt gravitational collapse and the star implodes — triggering a **Type Ia supernova**.

### White Dwarf Mass–Radius Relation

Unlike normal stars, white dwarfs shrink as mass increases (non-relativistic approximation):

$$R_{\\rm WD} \\approx R_0 \\left(\\frac{M}{M_{\\rm Ch}}\\right)^{-1/3}$$

where $R_0 \\approx 0.01\\ R_\\odot$ is a characteristic radius for typical white dwarfs.

### Gravitational Binding Energy

The gravitational binding energy of a uniform sphere:

$$E_{\\rm bind} = -\\frac{3GM^2}{5R}$$

For a 0.6 $M_\\odot$ white dwarf, this is a few $\\times 10^{42}$ J.

### Your Task

Implement three functions. Use $G = 6.674 \\times 10^{-11}$ m³ kg⁻¹ s⁻², defined **inside** each function.

- \`chandrasekhar_mass_solar(mu_e)\` — returns $M_{\\rm Ch}$ in solar masses
- \`wd_radius_solar(M_solar, mu_e)\` — returns white dwarf radius in solar radii
- \`gravitational_binding_energy_J(M_kg, R_m)\` — returns $E_{\\rm bind}$ in joules (negative)`,

	starterCode: `import math

def chandrasekhar_mass_solar(mu_e):
    # TODO: return 5.87 / mu_e**2
    pass

def wd_radius_solar(M_solar, mu_e):
    # TODO: return 0.01 * (M_solar / M_Ch)**(-1/3)
    pass

def gravitational_binding_energy_J(M_kg, R_m):
    G = 6.674e-11
    # TODO: return -3 * G * M_kg**2 / (5 * R_m)
    pass

print(round(chandrasekhar_mass_solar(2), 4))
print(round(chandrasekhar_mass_solar(1), 4))
print(round(wd_radius_solar(0.6, 2), 6))
`,

	solution: `import math

def chandrasekhar_mass_solar(mu_e):
    return 5.87 / mu_e**2

def wd_radius_solar(M_solar, mu_e):
    M_ch = 5.87 / mu_e**2
    return 0.01 * (M_solar / M_ch)**(-1/3)

def gravitational_binding_energy_J(M_kg, R_m):
    G = 6.674e-11
    return -3 * G * M_kg**2 / (5 * R_m)

print(round(chandrasekhar_mass_solar(2), 4))
print(round(chandrasekhar_mass_solar(1), 4))
print(round(wd_radius_solar(0.6, 2), 6))
`,

	tests: [
		{
			name: "chandrasekhar_mass_solar(2) ≈ 1.4675 M_sun (C/O white dwarf)",
			code: `{{FUNC}}
print(round(chandrasekhar_mass_solar(2), 4))`,
			expected: "1.4675\n",
		},
		{
			name: "chandrasekhar_mass_solar(1) = 5.87 M_sun (hydrogen)",
			code: `{{FUNC}}
print(round(chandrasekhar_mass_solar(1), 4))`,
			expected: "5.87\n",
		},
		{
			name: "wd_radius_solar(0.6, 2) ≈ 0.013473 R_sun (typical 0.6 M_sun WD)",
			code: `{{FUNC}}
print(round(wd_radius_solar(0.6, 2), 6))`,
			expected: "0.013473\n",
		},
		{
			name: "gravitational_binding_energy_J for 0.6 M_sun WD ≈ -6.08e+42 J",
			code: `{{FUNC}}
M_wd = 1.989e30 * 0.6
R_wd = 6.957e8 * wd_radius_solar(0.6, 2)
print(round(gravitational_binding_energy_J(M_wd, R_wd) / 1e44, 4))`,
			expected: "-0.0608\n",
		},
	],
};
