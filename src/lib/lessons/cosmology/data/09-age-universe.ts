import type { Lesson } from "../../types";

export const ageOfUniverse: Lesson = {
	id: "age-of-universe",
	title: "Age of the Universe",
	chapterId: "thermal-history",
	content: `## Age of the Universe

How old is the universe? The answer depends on the cosmological model — specifically the Hubble constant H₀ and the density parameters.

### The Hubble Time

A simple first estimate is the **Hubble time** — the time it would take for the universe to reach its current size if it had always expanded at today's rate:

$$t_H = \\frac{1}{H_0}$$

With H₀ = 70 km/s/Mpc ≈ 2.27 × 10⁻¹⁸ s⁻¹:

$$t_H \\approx 13.97 \\text{ Gyr}$$

### Matter-Only Universe

For a flat, matter-dominated universe the exact age is:

$$t_0 = \\frac{2}{3} t_H \\approx 9.31 \\text{ Gyr}$$

This is younger than the oldest stars, which ruled out a pure matter universe.

### Flat ΛCDM (the real universe)

The current standard model includes matter (Ω_m ≈ 0.3) and dark energy (Ω_Λ = 1 − Ω_m ≈ 0.7). The exact age is:

$$t_0 = \\frac{2}{3H_0\\sqrt{1-\\Omega_m}} \\ln\\left(\\frac{1 + \\sqrt{1-\\Omega_m}}{\\sqrt{\\Omega_m}}\\right)$$

With H₀ = 70 km/s/Mpc and Ω_m = 0.3: **t₀ ≈ 13.47 Gyr**

With the Planck values (H₀ = 67.4, Ω_m = 0.315): **t₀ ≈ 13.80 Gyr**

### Converting to Gyr

1 Gyr = 10⁹ years. The conversion factor: 1 Gyr ≈ 3.156 × 10¹⁶ s.

### Your Task

Implement three functions. All constants must be defined **inside** each function body.

- \`hubble_time_Gyr(H0_km_s_Mpc)\` — returns 1/H₀ in Gyr
- \`age_matter_only_Gyr(H0_km_s_Mpc)\` — returns (2/3) × t_H in Gyr
- \`age_LCDM_Gyr(H0_km_s_Mpc, Omega_m)\` — returns flat ΛCDM age in Gyr`,

	starterCode: `import math

def hubble_time_Gyr(H0_km_s_Mpc):
    # H0_SI = H0_km_s_Mpc * 1000 / 3.0857e22  (convert to s^-1)
    # 1 Gyr = 3.156e16 s
    # TODO: return 1 / H0_SI / 3.156e16
    pass

def age_matter_only_Gyr(H0_km_s_Mpc):
    # TODO: return (2/3) * hubble_time (inline the calculation)
    pass

def age_LCDM_Gyr(H0_km_s_Mpc, Omega_m):
    # TODO: return flat LCDM age using the log formula
    pass

print(round(hubble_time_Gyr(70), 2))
print(round(age_matter_only_Gyr(70), 2))
print(round(age_LCDM_Gyr(70, 0.3), 2))
print(round(age_LCDM_Gyr(67.4, 0.315), 2))
`,

	solution: `import math

def hubble_time_Gyr(H0_km_s_Mpc):
    H0_SI = H0_km_s_Mpc * 1000 / 3.0857e22
    return 1 / H0_SI / 3.156e16

def age_matter_only_Gyr(H0_km_s_Mpc):
    H0_SI = H0_km_s_Mpc * 1000 / 3.0857e22
    t_H = 1 / H0_SI / 3.156e16
    return (2/3) * t_H

def age_LCDM_Gyr(H0_km_s_Mpc, Omega_m):
    H0_SI = H0_km_s_Mpc * 1000 / 3.0857e22
    return (2 / (3 * H0_SI * math.sqrt(1 - Omega_m))) * math.log((1 + math.sqrt(1 - Omega_m)) / math.sqrt(Omega_m)) / 3.156e16

print(round(hubble_time_Gyr(70), 2))
print(round(age_matter_only_Gyr(70), 2))
print(round(age_LCDM_Gyr(70, 0.3), 2))
print(round(age_LCDM_Gyr(67.4, 0.315), 2))
`,

	tests: [
		{
			name: "hubble_time_Gyr(70) ≈ 13.97 Gyr",
			code: `{{FUNC}}
print(round(hubble_time_Gyr(70), 2))`,
			expected: "13.97\n",
		},
		{
			name: "age_matter_only_Gyr(70) ≈ 9.31 Gyr (too young for stars)",
			code: `{{FUNC}}
print(round(age_matter_only_Gyr(70), 2))`,
			expected: "9.31\n",
		},
		{
			name: "age_LCDM_Gyr(70, 0.3) ≈ 13.47 Gyr",
			code: `{{FUNC}}
print(round(age_LCDM_Gyr(70, 0.3), 2))`,
			expected: "13.47\n",
		},
		{
			name: "age_LCDM_Gyr(67.4, 0.315) ≈ 13.8 Gyr (Planck cosmology)",
			code: `{{FUNC}}
print(round(age_LCDM_Gyr(67.4, 0.315), 2))`,
			expected: "13.8\n",
		},
	],
};
