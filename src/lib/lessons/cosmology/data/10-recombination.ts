import type { Lesson } from "../../types";

export const recombination: Lesson = {
	id: "recombination",
	title: "Recombination",
	chapterId: "thermal-history",
	content: `## Recombination

**Recombination** is one of the most important events in cosmic history. At redshift z ≈ 1089 (about 380,000 years after the Big Bang), the universe had cooled enough that free electrons could combine with protons to form neutral hydrogen atoms.

### Why Recombination Matters

Before recombination the universe was opaque: photons were constantly scattered by free electrons. The **mean free path** of a photon was tiny. At recombination, the electron density dropped dramatically and the universe became **transparent**. Photons could travel freely for the first time — these are the CMB photons we observe today.

### Temperature at Recombination

$$T_{rec} = T_0 \\cdot (1 + z_{rec}) = 2.725 \\times 1090 \\approx 2970 \\text{ K}$$

### Thomson Scattering

Before recombination, photons scattered off free electrons via **Thomson scattering**. The optical depth through a column of ionised gas:

$$\\tau = \\sigma_T \\cdot x_e \\cdot n_b \\cdot L$$

where σ_T = 6.652 × 10⁻²⁹ m² is the Thomson cross section, x_e is the ionisation fraction, n_b is the baryon number density, and L is the path length.

### Baryon Number Density

The present-day baryon number density:

$$n_b = \\frac{\\Omega_b \\cdot \\rho_c}{m_p}$$

where ρ_c = 3H²/(8πG) is the critical density and m_p = 1.673 × 10⁻²⁷ kg is the proton mass.

### Your Task

Implement four functions. All constants must be defined **inside** each function body.

- \`recombination_redshift()\` — returns the standard value 1089.0
- \`recombination_temperature_K()\` — returns T₀ × 1090 with T₀ = 2.725 K inside the function
- \`thomson_optical_depth(x_e, n_b_m3, path_length_m)\` — returns τ = σ_T × x_e × n_b × L
- \`baryon_number_density_m3(Omega_b, H0_km_s_Mpc)\` — returns n_b = Ω_b × ρ_c / m_p`,

	starterCode: `import math

def recombination_redshift():
    # TODO: return 1089.0
    pass

def recombination_temperature_K():
    T0 = 2.725  # K
    # TODO: return T0 * 1090
    pass

def thomson_optical_depth(x_e, n_b_m3, path_length_m):
    sigma_T = 6.652e-29  # m^2, Thomson cross section
    # TODO: return sigma_T * x_e * n_b_m3 * path_length_m
    pass

def baryon_number_density_m3(Omega_b, H0_km_s_Mpc):
    G = 6.674e-11      # m^3 / (kg * s^2)
    m_p = 1.673e-27    # kg, proton mass
    # H0_SI = H0_km_s_Mpc * 1000 / 3.0857e22
    # rho_c = 3 * H0_SI^2 / (8 * pi * G)
    # TODO: return Omega_b * rho_c / m_p
    pass

print(recombination_redshift())
print(round(recombination_temperature_K(), 3))
print(round(thomson_optical_depth(1.0, 1e10, 1e22), 4))
print(round(baryon_number_density_m3(0.05, 70), 4))
`,

	solution: `import math

def recombination_redshift():
    return 1089.0

def recombination_temperature_K():
    T0 = 2.725  # K
    return T0 * 1090

def thomson_optical_depth(x_e, n_b_m3, path_length_m):
    sigma_T = 6.652e-29  # m^2, Thomson cross section
    return sigma_T * x_e * n_b_m3 * path_length_m

def baryon_number_density_m3(Omega_b, H0_km_s_Mpc):
    G = 6.674e-11      # m^3 / (kg * s^2)
    m_p = 1.673e-27    # kg, proton mass
    H0_SI = H0_km_s_Mpc * 1000 / 3.0857e22
    rho_c = 3 * H0_SI**2 / (8 * math.pi * G)
    return Omega_b * rho_c / m_p

print(recombination_redshift())
print(round(recombination_temperature_K(), 3))
print(round(thomson_optical_depth(1.0, 1e10, 1e22), 4))
print(round(baryon_number_density_m3(0.05, 70), 4))
`,

	tests: [
		{
			name: "recombination_redshift() = 1089.0",
			code: `{{FUNC}}
print(recombination_redshift())`,
			expected: "1089.0\n",
		},
		{
			name: "recombination_temperature_K() ≈ 2970.25 K",
			code: `{{FUNC}}
print(round(recombination_temperature_K(), 3))`,
			expected: "2970.25\n",
		},
		{
			name: "thomson_optical_depth(1.0, 1e10, 1e22) = 6652.0",
			code: `{{FUNC}}
print(round(thomson_optical_depth(1.0, 1e10, 1e22), 4))`,
			expected: "6652.0\n",
		},
		{
			name: "baryon_number_density_m3(0.05, 70) ≈ 0.2751 m⁻³",
			code: `{{FUNC}}
print(round(baryon_number_density_m3(0.05, 70), 4))`,
			expected: "0.2751\n",
		},
	],
};
