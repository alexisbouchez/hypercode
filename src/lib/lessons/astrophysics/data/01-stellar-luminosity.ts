import type { Lesson } from "../../types";

export const stellarLuminosity: Lesson = {
	id: "stellar-luminosity",
	title: "Stellar Luminosity",
	chapterId: "stellar-physics",
	content: `## Stellar Luminosity

A star radiates as a near-perfect blackbody. Its total luminosity is governed by the **Stefan-Boltzmann law**:

$$L = 4\\pi R^2 \\sigma T_{\\text{eff}}^4$$

where:
- $R$ is the stellar radius in metres
- $T_{\\text{eff}}$ is the effective surface temperature in Kelvin
- $\\sigma = 5.6704 \\times 10^{-8}$ W m⁻² K⁻⁴ is the Stefan-Boltzmann constant

### Solar Units

Astronomers often express luminosity relative to the Sun:

$$\\frac{L}{L_\\odot} = \\left(\\frac{R}{R_\\odot}\\right)^2 \\left(\\frac{T}{T_\\odot}\\right)^4$$

The Sun's parameters: $R_\\odot = 6.957 \\times 10^8$ m, $T_\\odot = 5778$ K, $L_\\odot = 3.828 \\times 10^{26}$ W.

A star with twice the radius and the same temperature has **four times** the luminosity. A star twice as hot (same radius) has **sixteen times** the luminosity.

### Inverse: Effective Temperature

Given the luminosity and radius, you can recover the effective temperature:

$$T_{\\text{eff}} = \\left(\\frac{L}{4\\pi R^2 \\sigma}\\right)^{1/4}$$

This is used to determine stellar surface temperatures from measured luminosities and radii.

| Star | $R/R_\\odot$ | $T$ (K) | $L/L_\\odot$ |
|------|-------------|---------|--------------|
| Sun  | 1.0         | 5778    | 1.0          |
| Sirius A | 1.71   | 9940    | 25.4         |
| Betelgeuse | 700  | 3500    | ~100,000     |

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`stefan_boltzmann_luminosity(R_m, T_K)\` — returns $L$ in watts
- \`luminosity_solar(R_m, T_K)\` — returns $L / L_\\odot$ (dimensionless)
- \`effective_temperature(L_W, R_m)\` — returns $T_{\\text{eff}}$ in Kelvin

Use $\\sigma = 5.6704 \\times 10^{-8}$ W m⁻² K⁻⁴ and $L_\\odot = 3.828 \\times 10^{26}$ W.`,

	starterCode: `import math

def stefan_boltzmann_luminosity(R_m, T_K):
    sigma = 5.6704e-8  # W/m^2/K^4
    # TODO: return 4 * pi * R_m^2 * sigma * T_K^4
    pass

def luminosity_solar(R_m, T_K):
    L_sun = 3.828e26  # W
    # TODO: return L / L_sun using sigma = 5.6704e-8
    pass

def effective_temperature(L_W, R_m):
    sigma = 5.6704e-8  # W/m^2/K^4
    # TODO: return (L / (4 * pi * R^2 * sigma))^0.25
    pass

print(round(stefan_boltzmann_luminosity(6.957e8, 5778) / 1e26, 4))
print(round(luminosity_solar(6.957e8, 5778), 4))
print(round(luminosity_solar(6.957e8 * 10, 5778), 2))
print(round(effective_temperature(3.828e26, 6.957e8), 0))
`,

	solution: `import math

def stefan_boltzmann_luminosity(R_m, T_K):
    sigma = 5.6704e-8  # W/m^2/K^4
    return 4 * math.pi * R_m**2 * sigma * T_K**4

def luminosity_solar(R_m, T_K):
    sigma = 5.6704e-8  # W/m^2/K^4
    L_sun = 3.828e26   # W
    return 4 * math.pi * R_m**2 * sigma * T_K**4 / L_sun

def effective_temperature(L_W, R_m):
    sigma = 5.6704e-8  # W/m^2/K^4
    return (L_W / (4 * math.pi * R_m**2 * sigma))**0.25

print(round(stefan_boltzmann_luminosity(6.957e8, 5778) / 1e26, 4))
print(round(luminosity_solar(6.957e8, 5778), 4))
print(round(luminosity_solar(6.957e8 * 10, 5778), 2))
print(round(effective_temperature(3.828e26, 6.957e8), 0))
`,

	tests: [
		{
			name: "stefan_boltzmann_luminosity(R_sun, T_sun) ≈ 3.8439×10²⁶ W",
			code: `{{FUNC}}
print(round(stefan_boltzmann_luminosity(6.957e8, 5778) / 1e26, 4))`,
			expected: "3.8439\n",
		},
		{
			name: "luminosity_solar(R_sun, T_sun) ≈ 1.0042 L_sun",
			code: `{{FUNC}}
print(round(luminosity_solar(6.957e8, 5778), 4))`,
			expected: "1.0042\n",
		},
		{
			name: "luminosity_solar(10 R_sun, T_sun) ≈ 100.42 L_sun (area scales as R²)",
			code: `{{FUNC}}
print(round(luminosity_solar(6.957e8 * 10, 5778), 2))`,
			expected: "100.42\n",
		},
		{
			name: "effective_temperature(L_sun, R_sun) ≈ 5772 K",
			code: `{{FUNC}}
print(round(effective_temperature(3.828e26, 6.957e8), 0))`,
			expected: "5772.0\n",
		},
	],
};
