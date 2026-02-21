import type { Lesson } from "../../types";

export const nuclearTimescale: Lesson = {
	id: "nuclear-timescale",
	title: "Nuclear Timescale & Stellar Lifetimes",
	chapterId: "stellar-evolution",
	content: `## Nuclear Timescale & Stellar Lifetimes

Stars shine by **nuclear fusion** — converting hydrogen to helium in their cores. The energy released per unit mass is determined by Einstein's $E = mc^2$: hydrogen fusion converts 0.7% of the rest-mass energy into radiation.

### Nuclear Timescale

The total nuclear burning lifetime is:

$$t_{\\text{nuc}} = \\frac{\\varepsilon M c^2}{L}$$

where:
- $\\varepsilon = 0.007$ is the hydrogen fusion efficiency (0.7% of rest mass)
- $M$ is the stellar mass in kg
- $c = 2.998 \\times 10^8$ m/s
- $L$ is the luminosity in watts

For the Sun: $t_{\\text{nuc}} \\approx 104$ Gyr for complete H→He conversion, or ~10 Gyr for core hydrogen (roughly 10% of the total mass fuses before the star leaves the main sequence).

### Mass-Luminosity Relation

On the main sequence, luminosity scales steeply with mass:

$$L \\propto M^4$$

Combining this with the nuclear timescale:

$$t_{\\text{nuc}} \\propto \\frac{M}{L} \\propto \\frac{M}{M^4} = M^{-3}$$

**Massive stars burn out dramatically faster.** A 10 $M_\\odot$ star lives about 1000 times shorter than the Sun. A 0.1 $M_\\odot$ red dwarf could shine for trillions of years.

| Mass ($M_\\odot$) | Lifetime (Gyr) |
|-----------------|----------------|
| 10              | ~0.1           |
| 1 (Sun)         | ~104           |
| 0.1             | ~100,000       |

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`nuclear_timescale_s(M_kg, L_W)\` — returns $t_{\\text{nuc}}$ in seconds
- \`nuclear_timescale_Gyr(M_solar, L_solar)\` — returns $t_{\\text{nuc}}$ in gigayears
- \`main_sequence_lifetime_Gyr(M_solar)\` — uses $L \\propto M^4$ to return lifetime in Gyr

Use $c = 2.998 \\times 10^8$ m/s, $M_\\odot = 1.989 \\times 10^{30}$ kg, $L_\\odot = 3.828 \\times 10^{26}$ W, 1 Gyr $= 3.1558 \\times 10^{16}$ s.`,

	starterCode: `import math

def nuclear_timescale_s(M_kg, L_W):
    c = 2.998e8  # m/s
    # TODO: return 0.007 * M_kg * c^2 / L_W
    pass

def nuclear_timescale_Gyr(M_solar, L_solar):
    c = 2.998e8       # m/s
    M_sun = 1.989e30  # kg
    L_sun = 3.828e26  # W
    Gyr = 3.1558e16   # s per Gyr
    # TODO: convert to SI, compute timescale, convert to Gyr
    pass

def main_sequence_lifetime_Gyr(M_solar):
    c = 2.998e8       # m/s
    M_sun = 1.989e30  # kg
    L_sun = 3.828e26  # W
    Gyr = 3.1558e16   # s per Gyr
    # TODO: L_solar = M_solar^4, then compute nuclear_timescale_Gyr inline
    pass

print(round(nuclear_timescale_Gyr(1.0, 1.0), 2))
print(round(main_sequence_lifetime_Gyr(1.0), 2))
print(round(main_sequence_lifetime_Gyr(10.0), 4))
print(round(main_sequence_lifetime_Gyr(0.1), 0))
`,

	solution: `import math

def nuclear_timescale_s(M_kg, L_W):
    c = 2.998e8  # m/s
    return 0.007 * M_kg * c**2 / L_W

def nuclear_timescale_Gyr(M_solar, L_solar):
    c = 2.998e8       # m/s
    M_sun = 1.989e30  # kg
    L_sun = 3.828e26  # W
    Gyr = 3.1558e16   # s per Gyr
    return 0.007 * (M_solar * M_sun) * c**2 / (L_solar * L_sun) / Gyr

def main_sequence_lifetime_Gyr(M_solar):
    c = 2.998e8       # m/s
    M_sun = 1.989e30  # kg
    L_sun = 3.828e26  # W
    Gyr = 3.1558e16   # s per Gyr
    L_solar = M_solar**4
    return 0.007 * (M_solar * M_sun) * c**2 / (L_solar * L_sun) / Gyr

print(round(nuclear_timescale_Gyr(1.0, 1.0), 2))
print(round(main_sequence_lifetime_Gyr(1.0), 2))
print(round(main_sequence_lifetime_Gyr(10.0), 4))
print(round(main_sequence_lifetime_Gyr(0.1), 0))
`,

	tests: [
		{
			name: "nuclear_timescale_Gyr(1.0, 1.0) ≈ 103.59 Gyr (Sun burning all H)",
			code: `{{FUNC}}
print(round(nuclear_timescale_Gyr(1.0, 1.0), 2))`,
			expected: "103.59\n",
		},
		{
			name: "main_sequence_lifetime_Gyr(1.0) ≈ 103.59 Gyr (L = M^4 = 1 for 1 M_sun)",
			code: `{{FUNC}}
print(round(main_sequence_lifetime_Gyr(1.0), 2))`,
			expected: "103.59\n",
		},
		{
			name: "main_sequence_lifetime_Gyr(10.0) ≈ 0.1036 Gyr (10 M_sun burns 1000x faster)",
			code: `{{FUNC}}
print(round(main_sequence_lifetime_Gyr(10.0), 4))`,
			expected: "0.1036\n",
		},
		{
			name: "main_sequence_lifetime_Gyr(0.1) ≈ 103589 Gyr (red dwarf burns 1000x slower)",
			code: `{{FUNC}}
print(round(main_sequence_lifetime_Gyr(0.1), 0))`,
			expected: "103589.0\n",
		},
	],
};
