import type { Lesson } from "../../types";

export const matterRadiationEquality: Lesson = {
	id: "matter-radiation-equality",
	title: "Matter-Radiation Equality",
	chapterId: "thermal-history",
	content: `## Matter-Radiation Equality

In the very early universe, **radiation dominated** — photons and relativistic neutrinos made up the dominant energy content. As the universe expanded, however, the energy densities diluted at different rates:

- **Radiation**: ρ_r ∝ a⁻⁴ (energy density drops faster because photon wavelengths also redshift)
- **Matter**: ρ_m ∝ a⁻³

Eventually these two densities became equal at the **matter-radiation equality** epoch.

### Scale Factor and Redshift of Equality

At equality, ρ_r = ρ_m. Using density parameters (normalised to critical density today):

$$a_{eq} = \\frac{\\Omega_r}{\\Omega_m}$$

$$z_{eq} = \\frac{1}{a_{eq}} - 1 = \\frac{\\Omega_m}{\\Omega_r} - 1$$

With Ω_m ≈ 0.3 and Ω_r ≈ 9.4 × 10⁻⁵ (photons + neutrinos):

$$z_{eq} \\approx 3190$$

### Temperature at Equality

Using T ∝ (1 + z):

$$T_{eq} = T_0 \\cdot (1 + z_{eq}) = T_0 \\cdot \\frac{\\Omega_m}{\\Omega_r}$$

### Significance

Matter-radiation equality marks the transition from a radiation-dominated to a matter-dominated universe. Before equality, radiation pressure suppressed the growth of density fluctuations. After equality, matter could begin to cluster, eventually forming the large-scale structure we see today.

### Your Task

Implement three functions. All constants must be defined **inside** each function body.

- \`scale_factor_equality(Omega_m, Omega_r)\` — returns a_eq = Ω_r / Ω_m
- \`redshift_equality(Omega_m, Omega_r)\` — returns z_eq = Ω_m / Ω_r − 1
- \`temperature_equality_K(Omega_m, Omega_r)\` — returns T_eq = T₀ × (Ω_m / Ω_r) with T₀ = 2.725 K`,

	starterCode: `import math

def scale_factor_equality(Omega_m, Omega_r):
    # TODO: return Omega_r / Omega_m
    pass

def redshift_equality(Omega_m, Omega_r):
    # TODO: return Omega_m / Omega_r - 1
    pass

def temperature_equality_K(Omega_m, Omega_r):
    T0 = 2.725  # K
    # TODO: return T0 * (Omega_m / Omega_r)
    pass

print(round(scale_factor_equality(0.3, 9.4e-5), 7))
print(round(redshift_equality(0.3, 9.4e-5), 1))
print(round(temperature_equality_K(0.3, 9.4e-5), 1))
print(round(redshift_equality(0.27, 8.5e-5), 1))
`,

	solution: `import math

def scale_factor_equality(Omega_m, Omega_r):
    return Omega_r / Omega_m

def redshift_equality(Omega_m, Omega_r):
    return Omega_m / Omega_r - 1

def temperature_equality_K(Omega_m, Omega_r):
    T0 = 2.725  # K
    return T0 * (Omega_m / Omega_r)

print(round(scale_factor_equality(0.3, 9.4e-5), 7))
print(round(redshift_equality(0.3, 9.4e-5), 1))
print(round(temperature_equality_K(0.3, 9.4e-5), 1))
print(round(redshift_equality(0.27, 8.5e-5), 1))
`,

	tests: [
		{
			name: "scale_factor_equality(0.3, 9.4e-5) ≈ 0.0003133",
			code: `{{FUNC}}
print(round(scale_factor_equality(0.3, 9.4e-5), 7))`,
			expected: "0.0003133\n",
		},
		{
			name: "redshift_equality(0.3, 9.4e-5) ≈ 3190.5",
			code: `{{FUNC}}
print(round(redshift_equality(0.3, 9.4e-5), 1))`,
			expected: "3190.5\n",
		},
		{
			name: "temperature_equality_K(0.3, 9.4e-5) ≈ 8696.8 K",
			code: `{{FUNC}}
print(round(temperature_equality_K(0.3, 9.4e-5), 1))`,
			expected: "8696.8\n",
		},
		{
			name: "redshift_equality(0.27, 8.5e-5) ≈ 3175.5 (alternate cosmology)",
			code: `{{FUNC}}
print(round(redshift_equality(0.27, 8.5e-5), 1))`,
			expected: "3175.5\n",
		},
	],
};
