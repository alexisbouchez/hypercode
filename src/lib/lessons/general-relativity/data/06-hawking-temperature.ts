import type { Lesson } from "../../types";

export const hawkingTemperature: Lesson = {
	id: "hawking-temperature",
	title: "Hawking Temperature",
	chapterId: "black-holes",
	content: `## Hawking Temperature

In 1974, Stephen Hawking showed that black holes are not entirely black — they emit thermal radiation due to quantum effects near the event horizon. This **Hawking radiation** has a characteristic temperature:

$$T_H = \\frac{\\hbar c^3}{8\\pi G M k_B}$$

where:
- $\\hbar = 1.0546 \\times 10^{-34}$ J·s (reduced Planck constant)
- $c = 2.998 \\times 10^8$ m/s (speed of light)
- $G = 6.674 \\times 10^{-11}$ N·m²/kg² (gravitational constant)
- $M$ = black hole mass in kg
- $k_B = 1.381 \\times 10^{-23}$ J/K (Boltzmann constant)

For a solar-mass black hole, $T_H \\approx 6 \\times 10^{-8}$ K — far colder than the cosmic microwave background (2.7 K). Tiny black holes, however, are extremely hot and evaporate rapidly.

### Peak Wavelength of Hawking Radiation

The peak wavelength of the emitted radiation (Wien's law) simplifies to:

$$\\lambda_{\\text{peak}} = \\frac{hc}{k_B T_H} = \\frac{2\\pi\\hbar c}{k_B T_H}$$

Substituting $T_H$ and simplifying:

$$\\lambda_{\\text{peak}} = \\frac{16\\pi^2 G M}{c^2}$$

Notice this equals $8\\pi$ times the Schwarzschild radius — the radiation wavelength is comparable to the black hole size.

### Hawking Luminosity

Treating the black hole as a black body radiating from area $A = 4\\pi r_s^2$ (where $r_s = 2GM/c^2$), the Stefan–Boltzmann law gives:

$$L = \\sigma A T_H^4$$

with $\\sigma = 5.67 \\times 10^{-8}$ W/m²/K⁴. Smaller black holes are hotter and far more luminous.

### Your Task

Implement three functions. All physical constants must be defined **inside** each function body.

- \`hawking_temperature(M)\` — returns $T_H$ in Kelvin
- \`hawking_wavelength(M)\` — returns peak wavelength using the simplified formula $16\\pi^2 GM/c^2$
- \`hawking_luminosity(M)\` — returns luminosity $L$ using the Stefan–Boltzmann law`,

	starterCode: `import math

def hawking_temperature(M):
    hbar = 1.0546e-34
    c = 299792458.0
    G = 6.674e-11
    k_B = 1.381e-23
    # T_H = hbar * c^3 / (8 * pi * G * M * k_B)
    pass

def hawking_wavelength(M):
    G = 6.674e-11
    c = 299792458.0
    # Simplified: 16 * pi^2 * G * M / c^2
    pass

def hawking_luminosity(M):
    hbar = 1.0546e-34
    c = 299792458.0
    G = 6.674e-11
    k_B = 1.381e-23
    sigma = 5.67e-8
    # r_s = 2*G*M/c^2, A = 4*pi*r_s^2, T = hawking_temperature
    # L = sigma * A * T^4
    pass

print(round(hawking_temperature(1.989e30), 15))
print(round(hawking_wavelength(1.989e30), 2))
`,

	solution: `import math

def hawking_temperature(M):
    hbar = 1.0546e-34
    c = 299792458.0
    G = 6.674e-11
    k_B = 1.381e-23
    return hbar * c**3 / (8 * math.pi * G * M * k_B)

def hawking_wavelength(M):
    G = 6.674e-11
    c = 299792458.0
    return 16 * math.pi**2 * G * M / c**2

def hawking_luminosity(M):
    hbar = 1.0546e-34
    c = 299792458.0
    G = 6.674e-11
    k_B = 1.381e-23
    sigma = 5.67e-8
    r_s = 2 * G * M / c**2
    A = 4 * math.pi * r_s**2
    T = hbar * c**3 / (8 * math.pi * G * M * k_B)
    return sigma * A * T**4

print(round(hawking_temperature(1.989e30), 15))
print(round(hawking_wavelength(1.989e30), 2))
`,

	tests: [
		{
			name: "hawking_temperature(solar mass) ≈ 6.17e-8 K",
			code: `{{FUNC}}
print(round(hawking_temperature(1.989e30), 15))`,
			expected: "6.1673039e-08\n",
		},
		{
			name: "hawking_temperature(1e12 kg) ≈ 1.23e11 K (hot micro black hole)",
			code: `{{FUNC}}
print(round(hawking_temperature(1e12), 2))`,
			expected: "122667675369.65\n",
		},
		{
			name: "hawking_wavelength(solar mass) ≈ 233238 m",
			code: `{{FUNC}}
print(round(hawking_wavelength(1.989e30), 2))`,
			expected: "233238.0\n",
		},
		{
			name: "hawking_luminosity(1e12 kg) ≈ 3.56e8 W",
			code: `{{FUNC}}
print(round(hawking_luminosity(1e12), 2))`,
			expected: "355846786.25\n",
		},
	],
};
