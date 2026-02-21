import type { Lesson } from "../../types";

export const planckUnits: Lesson = {
	id: "planck-units",
	title: "Planck Units",
	chapterId: "dark-sector",
	content: `## Planck Units

The **Planck scale** marks where quantum mechanics and general relativity must both be important simultaneously. Built from the fundamental constants $G$, $\\hbar$, $c$, and $k_B$, Planck units set the natural scale of quantum gravity.

### The Planck Scales

$$l_P = \\sqrt{\\frac{\\hbar G}{c^3}} \\approx 1.616 \\times 10^{-35} \\text{ m}$$

$$t_P = \\sqrt{\\frac{\\hbar G}{c^5}} \\approx 5.391 \\times 10^{-44} \\text{ s}$$

$$m_P = \\sqrt{\\frac{\\hbar c}{G}} \\approx 2.177 \\times 10^{-8} \\text{ kg}$$

$$E_P = m_P c^2 = \\sqrt{\\frac{\\hbar c^5}{G}} \\approx 1.22 \\times 10^{19} \\text{ GeV}$$

$$T_P = \\frac{E_P}{k_B} \\approx 1.417 \\times 10^{32} \\text{ K}$$

### Cosmological Significance

Inflation is thought to have occurred at energies near the Planck scale. The Big Bang singularity is a Planck-scale phenomenon where our classical description of spacetime breaks down.

The Planck time is the earliest moment after the Big Bang that our current physics can describe. Before $t < t_P \\approx 5.4 \\times 10^{-44}$ s, a theory of quantum gravity is required.

### Constants

Use:
- $\\hbar = 1.055 \\times 10^{-34}$ J·s
- $G = 6.674 \\times 10^{-11}$ m³/(kg·s²)
- $c = 2.998 \\times 10^8$ m/s
- $1$ GeV $= 1.602 \\times 10^{-10}$ J

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`planck_length_m()\` — returns $l_P = \\sqrt{\\hbar G / c^3}$ in metres
- \`planck_time_s()\` — returns $t_P = \\sqrt{\\hbar G / c^5}$ in seconds
- \`planck_energy_GeV()\` — returns $E_P = \\sqrt{\\hbar c^5 / G}$ in GeV (divide by $1.602 \\times 10^{-10}$ J/GeV)`,

	starterCode: `import math

def planck_length_m():
    hbar = 1.055e-34
    G = 6.674e-11
    c = 2.998e8
    # TODO: return sqrt(hbar * G / c^3)
    pass

def planck_time_s():
    hbar = 1.055e-34
    G = 6.674e-11
    c = 2.998e8
    # TODO: return sqrt(hbar * G / c^5)
    pass

def planck_energy_GeV():
    hbar = 1.055e-34
    G = 6.674e-11
    c = 2.998e8
    # TODO: return sqrt(hbar * c^5 / G) / 1.602e-10
    pass

print(f"{planck_length_m():.3e}")
print(f"{planck_time_s():.3e}")
print(round(planck_energy_GeV() / 1e19, 4))
`,

	solution: `import math

def planck_length_m():
    hbar = 1.055e-34
    G = 6.674e-11
    c = 2.998e8
    return math.sqrt(hbar * G / c**3)

def planck_time_s():
    hbar = 1.055e-34
    G = 6.674e-11
    c = 2.998e8
    return math.sqrt(hbar * G / c**5)

def planck_energy_GeV():
    hbar = 1.055e-34
    G = 6.674e-11
    c = 2.998e8
    return math.sqrt(hbar * c**5 / G) / 1.602e-10

print(f"{planck_length_m():.3e}")
print(f"{planck_time_s():.3e}")
print(round(planck_energy_GeV() / 1e19, 4))
`,

	tests: [
		{
			name: 'planck_length_m() ≈ 1.616e-35 m',
			code: `{{FUNC}}
print(f"{planck_length_m():.3e}")`,
			expected: "1.616e-35\n",
		},
		{
			name: 'planck_time_s() ≈ 5.392e-44 s',
			code: `{{FUNC}}
print(f"{planck_time_s():.3e}")`,
			expected: "5.392e-44\n",
		},
		{
			name: "planck_energy_GeV() ≈ 1.2214e19 GeV",
			code: `{{FUNC}}
print(round(planck_energy_GeV() / 1e19, 4))`,
			expected: "1.2214\n",
		},
		{
			name: 'planck_length_m() to 4 sig figs ≈ 1.6165e-35 m',
			code: `{{FUNC}}
print(f"{planck_length_m():.4e}")`,
			expected: "1.6165e-35\n",
		},
	],
};
