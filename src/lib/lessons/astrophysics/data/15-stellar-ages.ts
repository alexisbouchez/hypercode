import type { Lesson } from "../../types";

export const stellarAges: Lesson = {
	id: "stellar-ages",
	title: "Stellar Ages and Main-Sequence Lifetimes",
	chapterId: "galactic",
	content: `## Stellar Ages and Main-Sequence Lifetimes

A star's lifetime on the main sequence is set by two competing factors: how much hydrogen fuel it has ($\\propto M$) and how fast it burns it ($L \\propto M^4$). The result is a steep mass dependence:

$$t_{\\text{MS}} \\approx t_\\odot \\left(\\frac{M}{M_\\odot}\\right)^{-2.5}$$

where $t_\\odot \\approx 10$ Gyr is the Sun's main-sequence lifetime.

| Mass ($M_\\odot$) | Lifetime |
|------------------|----------|
| 10 | 32 Myr |
| 2 | 1.77 Gyr |
| 1 | 10 Gyr |
| 0.5 | 56.6 Gyr |

### Main-Sequence Turnoff

The **main-sequence turnoff** is the key to aging stellar clusters. Stars more massive than the turnoff mass have already evolved off the main sequence; the turnoff mass equals the mass whose lifetime equals the cluster age:

$$M_{\\text{to}} = M_\\odot \\left(\\frac{t_{\\text{age}}}{t_\\odot}\\right)^{-0.4}$$

### Turnoff Temperature

Using the mass-temperature scaling $T \\propto M^{0.6}$:

$$T_{\\text{to}} = T_\\odot \\left(\\frac{t_{\\text{age}}}{10\\text{ Gyr}}\\right)^{-0.4 \\times 0.6}$$

At 1 Gyr, the turnoff temperature is about 10,000 K — an A-type star.

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`main_sequence_lifetime_Gyr(M_solar)\` — returns $t_{\\text{MS}} = 10 \\times M^{-2.5}$ in Gyr
- \`turnoff_mass_solar(age_Gyr)\` — returns $M_{\\text{to}} = (t/10)^{-0.4}$ in $M_\\odot$
- \`turnoff_temperature_K(age_Gyr)\` — returns $T_{\\text{to}} = 5778 \\times (t/10)^{-0.24}$ in K

All three functions take a single numeric argument.`,

	starterCode: `import math

def main_sequence_lifetime_Gyr(M_solar):
    # TODO: return 10.0 * M_solar**(-2.5)
    pass

def turnoff_mass_solar(age_Gyr):
    # TODO: return (age_Gyr / 10.0)**(-0.4)
    pass

def turnoff_temperature_K(age_Gyr):
    T_sun = 5778
    # TODO: return T_sun * (age_Gyr / 10.0)**(-0.4 * 0.6)
    pass

print(round(main_sequence_lifetime_Gyr(1.0), 2))
print(round(turnoff_mass_solar(1.0), 4))
print(round(turnoff_temperature_K(1.0), 2))
`,

	solution: `import math

def main_sequence_lifetime_Gyr(M_solar):
    return 10.0 * M_solar**(-2.5)

def turnoff_mass_solar(age_Gyr):
    return (age_Gyr / 10.0)**(-0.4)

def turnoff_temperature_K(age_Gyr):
    T_sun = 5778
    return T_sun * (age_Gyr / 10.0)**(-0.4 * 0.6)

print(round(main_sequence_lifetime_Gyr(1.0), 2))
print(round(turnoff_mass_solar(1.0), 4))
print(round(turnoff_temperature_K(1.0), 2))
`,

	tests: [
		{
			name: "main_sequence_lifetime_Gyr(1.0) = 10.0 Gyr (the Sun)",
			code: `{{FUNC}}
print(round(main_sequence_lifetime_Gyr(1.0), 2))`,
			expected: "10.0\n",
		},
		{
			name: "main_sequence_lifetime_Gyr(2.0) ≈ 1.7678 Gyr",
			code: `{{FUNC}}
print(round(main_sequence_lifetime_Gyr(2.0), 4))`,
			expected: "1.7678\n",
		},
		{
			name: "turnoff_mass_solar(1.0) ≈ 2.5119 Msun (stars leaving MS at 1 Gyr)",
			code: `{{FUNC}}
print(round(turnoff_mass_solar(1.0), 4))`,
			expected: "2.5119\n",
		},
		{
			name: "turnoff_temperature_K(1.0) ≈ 10041.01 K at 1 Gyr",
			code: `{{FUNC}}
print(round(turnoff_temperature_K(1.0), 2))`,
			expected: "10041.01\n",
		},
	],
};
