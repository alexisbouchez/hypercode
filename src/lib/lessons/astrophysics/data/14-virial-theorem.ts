import type { Lesson } from "../../types";

export const virialTheorem: Lesson = {
	id: "virial-theorem",
	title: "The Virial Theorem and Dark Matter",
	chapterId: "galactic",
	content: `## The Virial Theorem and Dark Matter

For a gravitationally bound system in dynamical equilibrium, the **virial theorem** states:

$$2\\langle K \\rangle + \\langle U \\rangle = 0$$

The total energy is $E = K + U = -K = U/2$, meaning the system is bound with $E < 0$.

### Measuring Cluster Masses

For a galaxy cluster of $N$ galaxies with line-of-sight velocity dispersion $\\sigma$ and characteristic radius $R$, the virial mass is:

$$M_{\\text{virial}} = \\frac{5\\,\\sigma^2\\,R}{G}$$

This lets astronomers weigh galaxy clusters using only the velocities of member galaxies — no knowledge of the underlying mass distribution is required.

### Zwicky and Dark Matter

In 1933, Fritz Zwicky applied the virial theorem to the **Coma Cluster** ($\\sigma \\approx 1000$ km/s, $R \\approx 2$ Mpc) and found a virial mass orders of magnitude larger than the luminous mass. He called this missing mass **dunkle Materie** — dark matter. For the Coma Cluster:

$$M_{\\text{virial}} = \\frac{5 \\times (10^6\\text{ m/s})^2 \\times (2 \\times 3.086 \\times 10^{22}\\text{ m})}{6.674 \\times 10^{-11}} \\approx 2.3 \\times 10^{15}\\,M_\\odot$$

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`virial_mass_kg(sigma_m_s, R_m)\` — returns $M = 5\\sigma^2 R / G$ in kg
- \`virial_mass_solar(sigma_km_s, R_Mpc)\` — returns virial mass in solar masses (convenience units: $\\sigma$ in km/s, $R$ in Mpc where $1\\text{ Mpc} = 3.086 \\times 10^{22}$ m)
- \`kinetic_energy_J(M_kg, sigma_m_s)\` — returns $K = \\frac{1}{2} M \\sigma^2$ in joules

Use $G = 6.674 \\times 10^{-11}$ N m² kg⁻², $M_\\odot = 1.989 \\times 10^{30}$ kg.`,

	starterCode: `import math

def virial_mass_kg(sigma_m_s, R_m):
    G = 6.674e-11
    # TODO: return 5 * sigma_m_s**2 * R_m / G
    pass

def virial_mass_solar(sigma_km_s, R_Mpc):
    G = 6.674e-11
    M_sun = 1.989e30
    # TODO: convert units then return virial mass in solar masses
    pass

def kinetic_energy_J(M_kg, sigma_m_s):
    # TODO: return 0.5 * M_kg * sigma_m_s**2
    pass

print(round(virial_mass_solar(1000, 2) / 1e15, 4))
`,

	solution: `import math

def virial_mass_kg(sigma_m_s, R_m):
    G = 6.674e-11
    return 5 * sigma_m_s**2 * R_m / G

def virial_mass_solar(sigma_km_s, R_Mpc):
    G = 6.674e-11
    M_sun = 1.989e30
    sigma_m_s = sigma_km_s * 1000
    R_m = R_Mpc * 3.086e22
    return 5 * sigma_m_s**2 * R_m / G / M_sun

def kinetic_energy_J(M_kg, sigma_m_s):
    return 0.5 * M_kg * sigma_m_s**2

print(round(virial_mass_solar(1000, 2) / 1e15, 4))
`,

	tests: [
		{
			name: "Coma Cluster virial mass ≈ 2.3247e15 solar masses",
			code: `{{FUNC}}
print(round(virial_mass_solar(1000, 2) / 1e15, 4))`,
			expected: "2.3247\n",
		},
		{
			name: "Smaller cluster virial mass (sigma=500 km/s, R=1 Mpc) ≈ 2.9059e14 solar masses",
			code: `{{FUNC}}
print(round(virial_mass_solar(500, 1) / 1e14, 4))`,
			expected: "2.9059\n",
		},
		{
			name: "kinetic_energy_J for 1e14 Msun cluster at sigma=500 km/s",
			code: `{{FUNC}}
M = 1e14 * 1.989e30
KE = kinetic_energy_J(M, 500e3)
print(round(KE / 1e55, 4))`,
			expected: "2.4863\n",
		},
		{
			name: "virial_mass_kg and virial_mass_solar agree",
			code: `{{FUNC}}
M_kg = virial_mass_kg(500e3, 1 * 3.086e22)
M_solar = virial_mass_solar(500, 1)
ratio = M_kg / (M_solar * 1.989e30)
print(round(ratio, 6))`,
			expected: "1.0\n",
		},
	],
};
