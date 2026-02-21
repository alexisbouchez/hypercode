import type { Lesson } from "../../types";

export const cmbTemperature: Lesson = {
	id: "cmb-temperature",
	title: "CMB Temperature",
	chapterId: "thermal-history",
	content: `## CMB Temperature

After the Big Bang, the universe was a hot, dense plasma of photons, electrons, and protons. As the universe expanded and cooled, photons decoupled from matter at **recombination** (z ≈ 1089, T ≈ 3000 K). These photons now form the **Cosmic Microwave Background (CMB)** — the oldest light in the universe — at a temperature of T₀ = 2.725 K.

### Temperature Scaling with Redshift

As the universe expands, the photon temperature scales inversely with the scale factor:

$$T(z) = T_0 \\cdot (1 + z)$$

At recombination (z = 1089): T ≈ 2970 K.

### Photon Number Density

The photon number density is given by the Bose-Einstein distribution:

$$n_\\gamma = \\frac{2\\zeta(3)}{\\pi^2} \\left(\\frac{k_B T}{\\hbar c}\\right)^3$$

where ζ(3) ≈ 1.20206 is the Apéry constant. At T₀ = 2.725 K, this gives roughly **411 photons/cm³** — outnumbering baryons by a factor of ~10⁹.

Constants: k_B = 1.381 × 10⁻²³ J/K, ℏ = 1.055 × 10⁻³⁴ J·s, c = 2.998 × 10⁸ m/s

### Wien's Displacement Law

The peak wavelength of the CMB blackbody spectrum:

$$\\lambda_{\\max} = \\frac{b}{T}$$

where b = 2.898 × 10⁻³ m·K. At T₀ = 2.725 K, λ_max ≈ 1.064 mm — in the microwave band.

### Your Task

Implement three functions. All constants must be defined **inside** each function body.

- \`cmb_temperature_at_z(z)\` — returns CMB temperature T(z) = T₀ × (1+z) in Kelvin
- \`photon_number_density_m3(T_K)\` — returns photon number density in m⁻³
- \`cmb_peak_wavelength_mm(T_K)\` — returns Wien peak wavelength in millimetres`,

	starterCode: `import math

def cmb_temperature_at_z(z):
    T0 = 2.725  # K, CMB temperature today
    # TODO: return T0 * (1 + z)
    pass

def photon_number_density_m3(T_K):
    # Constants must be defined here
    # zeta3 = 1.20206, kB = 1.381e-23, hbar = 1.055e-34, c = 2.998e8
    # TODO: return 2 * zeta3 / pi^2 * (kB * T / (hbar * c))^3
    pass

def cmb_peak_wavelength_mm(T_K):
    b = 2.898e-3  # Wien constant, m*K
    # TODO: return b / T_K in millimetres
    pass

print(round(cmb_temperature_at_z(1089), 3))
print(round(cmb_temperature_at_z(0), 4))
print(round(photon_number_density_m3(2.725) / 1e6, 0))
print(round(cmb_peak_wavelength_mm(2.725), 4))
`,

	solution: `import math

def cmb_temperature_at_z(z):
    T0 = 2.725  # K, CMB temperature today
    return T0 * (1 + z)

def photon_number_density_m3(T_K):
    zeta3 = 1.20206
    kB = 1.381e-23   # J/K
    hbar = 1.055e-34  # J*s
    c = 2.998e8       # m/s
    return 2 * zeta3 / math.pi**2 * (kB * T_K / (hbar * c))**3

def cmb_peak_wavelength_mm(T_K):
    b = 2.898e-3  # Wien constant, m*K
    return b / T_K * 1000

print(round(cmb_temperature_at_z(1089), 3))
print(round(cmb_temperature_at_z(0), 4))
print(round(photon_number_density_m3(2.725) / 1e6, 0))
print(round(cmb_peak_wavelength_mm(2.725), 4))
`,

	tests: [
		{
			name: "cmb_temperature_at_z(1089) ≈ 2970.25 K (recombination)",
			code: `{{FUNC}}
print(round(cmb_temperature_at_z(1089), 3))`,
			expected: "2970.25\n",
		},
		{
			name: "cmb_temperature_at_z(0) = 2.725 K (today)",
			code: `{{FUNC}}
print(round(cmb_temperature_at_z(0), 4))`,
			expected: "2.725\n",
		},
		{
			name: "photon_number_density_m3(2.725) ≈ 410e6 m⁻³ (~410 photons/cm³)",
			code: `{{FUNC}}
print(round(photon_number_density_m3(2.725) / 1e6, 0))`,
			expected: "410.0\n",
		},
		{
			name: "cmb_peak_wavelength_mm(2.725) ≈ 1.0635 mm (microwave band)",
			code: `{{FUNC}}
print(round(cmb_peak_wavelength_mm(2.725), 4))`,
			expected: "1.0635\n",
		},
	],
};
