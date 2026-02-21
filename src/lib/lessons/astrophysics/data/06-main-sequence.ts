import type { Lesson } from "../../types";

export const mainSequence: Lesson = {
	id: "main-sequence",
	title: "Main Sequence Scaling Laws",
	chapterId: "stellar-evolution",
	content: `## Main Sequence Scaling Laws

On the main sequence, stars fuse hydrogen in their cores. Their bulk properties — luminosity, radius, and temperature — scale predictably with mass via empirical power laws.

### Luminosity

For solar-type main-sequence stars, luminosity scales steeply with mass:

$$\\frac{L}{L_\\odot} = \\left(\\frac{M}{M_\\odot}\\right)^4$$

This strong dependence explains why massive stars are so short-lived: they burn fuel far faster.

### Radius

Stellar radius scales more gently with mass:

$$\\frac{R}{R_\\odot} = \\left(\\frac{M}{M_\\odot}\\right)^{0.8}$$

### Effective Temperature

The effective temperature follows from the Stefan–Boltzmann law ($L = 4\\pi R^2 \\sigma T_{\\rm eff}^4$). Combining the luminosity and radius scaling laws:

$$T_{\\rm eff} \\propto \\frac{L^{0.25}}{R^{0.5}} \\propto M^{4 \\times 0.25 - 0.8 \\times 0.5} = M^{0.6}$$

In absolute terms:

$$T_{\\rm eff} = T_\\odot \\cdot \\left(\\frac{M}{M_\\odot}\\right)^{0.6}$$

where $T_\\odot = 5778$ K is the solar effective temperature.

| $M/M_\\odot$ | $L/L_\\odot$ | $R/R_\\odot$ | $T_{\\rm eff}$ (K) |
|---|---|---|---|
| 0.5 | 0.0625 | 0.574 | 4118 |
| 1.0 | 1.0 | 1.0 | 5778 |
| 2.0 | 16.0 | 1.741 | 8758 |
| 10.0 | 10000 | 6.310 | 18263 |

These approximations are valid for main-sequence stars in the range $0.5$–$50\\ M_\\odot$.

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`main_sequence_luminosity(M_solar)\` — returns $L/L_\\odot = (M/M_\\odot)^4$
- \`main_sequence_radius(M_solar)\` — returns $R/R_\\odot = (M/M_\\odot)^{0.8}$
- \`main_sequence_temperature(M_solar)\` — returns $T_{\\rm eff}$ in K, using $T_\\odot = 5778$ K`,

	starterCode: `import math

def main_sequence_luminosity(M_solar):
    # TODO: return (M_solar)^4
    pass

def main_sequence_radius(M_solar):
    # TODO: return (M_solar)^0.8
    pass

def main_sequence_temperature(M_solar):
    T_sun = 5778  # K
    # TODO: return T_sun * M_solar^0.6
    pass

print(main_sequence_luminosity(1.0))
print(round(main_sequence_luminosity(10.0), 0))
print(round(main_sequence_radius(2.0), 4))
print(round(main_sequence_temperature(2.0), 2))
`,

	solution: `import math

def main_sequence_luminosity(M_solar):
    return M_solar**4

def main_sequence_radius(M_solar):
    return M_solar**0.8

def main_sequence_temperature(M_solar):
    T_sun = 5778  # K
    return T_sun * M_solar**0.6

print(main_sequence_luminosity(1.0))
print(round(main_sequence_luminosity(10.0), 0))
print(round(main_sequence_radius(2.0), 4))
print(round(main_sequence_temperature(2.0), 2))
`,

	tests: [
		{
			name: "main_sequence_luminosity(1.0) = 1.0 (Sun on main sequence)",
			code: `{{FUNC}}
print(main_sequence_luminosity(1.0))`,
			expected: "1.0\n",
		},
		{
			name: "main_sequence_luminosity(10.0) = 10000.0 (10 solar-mass star)",
			code: `{{FUNC}}
print(round(main_sequence_luminosity(10.0), 0))`,
			expected: "10000.0\n",
		},
		{
			name: "main_sequence_radius(2.0) ≈ 1.7411 R_sun",
			code: `{{FUNC}}
print(round(main_sequence_radius(2.0), 4))`,
			expected: "1.7411\n",
		},
		{
			name: "main_sequence_temperature(2.0) ≈ 8757.81 K",
			code: `{{FUNC}}
print(round(main_sequence_temperature(2.0), 2))`,
			expected: "8757.81\n",
		},
	],
};
