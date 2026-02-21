import type { Lesson } from "../../types";

export const jeansInstability: Lesson = {
	id: "jeans-instability",
	title: "Jeans Instability",
	chapterId: "structure",
	content: `## Jeans Instability

A gas cloud collapses under its own gravity if it is large enough that gravitational potential energy overcomes the thermal kinetic energy of the gas. This threshold is set by the **Jeans length** λ_J.

### The Jeans Wavenumber

The Jeans wavenumber separates stable (oscillating) modes from unstable (collapsing) modes:

$$k_J = \\sqrt{\\frac{4\\pi G \\rho}{c_s^2}}$$

where $G = 6.674 \\times 10^{-11}$ m³/(kg·s²), $\\rho$ is the gas density in kg/m³, and $c_s$ is the sound speed in m/s.

### The Jeans Length

The Jeans length is the critical wavelength:

$$\\lambda_J = \\frac{2\\pi}{k_J} = c_s \\sqrt{\\frac{\\pi}{G \\rho}}$$

Perturbations with wavelength $\\lambda > \\lambda_J$ are gravitationally unstable and collapse. Those with $\\lambda < \\lambda_J$ oscillate as sound waves.

### The Jeans Mass

The mass enclosed within a sphere of radius $\\lambda_J / 2$:

$$M_J = \\frac{4\\pi}{3} \\rho \\left(\\frac{\\lambda_J}{2}\\right)^3$$

### Typical Molecular Cloud

For a cold molecular cloud with $c_s \\approx 200$ m/s (T ≈ 10 K) and $\\rho \\approx 10^{-18}$ kg/m³:
- $\\lambda_J \\approx 4.3 \\times 10^{16}$ m ≈ 1.4 parsecs
- $M_J \\approx 4.3 \\times 10^{31}$ kg ≈ 21 solar masses

These are the seeds of star formation!

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`jeans_wavenumber(c_s_m_s, rho_kg_m3)\` — returns $k_J$ in m⁻¹
- \`jeans_wavelength_m(c_s_m_s, rho_kg_m3)\` — returns $\\lambda_J = c_s \\sqrt{\\pi / (G \\rho)}$ in m
- \`jeans_mass_kg(c_s_m_s, rho_kg_m3)\` — returns $M_J$ in kg (compute $\\lambda_J$ inline)

Use $G = 6.674 \\times 10^{-11}$ m³/(kg·s²).`,

	starterCode: `import math

def jeans_wavenumber(c_s_m_s, rho_kg_m3):
    G = 6.674e-11
    # TODO: return sqrt(4 * pi * G * rho / c_s^2)
    pass

def jeans_wavelength_m(c_s_m_s, rho_kg_m3):
    G = 6.674e-11
    # TODO: return c_s * sqrt(pi / (G * rho))
    pass

def jeans_mass_kg(c_s_m_s, rho_kg_m3):
    G = 6.674e-11
    # TODO: compute lambda_J inline, then return (4*pi/3) * rho * (lambda_J/2)^3
    pass

print(f"{jeans_wavenumber(200, 1e-18):.4e}")
print(f"{jeans_wavelength_m(200, 1e-18):.4e}")
print(round(jeans_mass_kg(200, 1e-18) / 1e30, 4))
`,

	solution: `import math

def jeans_wavenumber(c_s_m_s, rho_kg_m3):
    G = 6.674e-11
    return math.sqrt(4 * math.pi * G * rho_kg_m3 / c_s_m_s**2)

def jeans_wavelength_m(c_s_m_s, rho_kg_m3):
    G = 6.674e-11
    return c_s_m_s * math.sqrt(math.pi / (G * rho_kg_m3))

def jeans_mass_kg(c_s_m_s, rho_kg_m3):
    G = 6.674e-11
    lambda_J = c_s_m_s * math.sqrt(math.pi / (G * rho_kg_m3))
    return (4 * math.pi / 3) * rho_kg_m3 * (lambda_J / 2)**3

print(f"{jeans_wavenumber(200, 1e-18):.4e}")
print(f"{jeans_wavelength_m(200, 1e-18):.4e}")
print(round(jeans_mass_kg(200, 1e-18) / 1e30, 4))
`,

	tests: [
		{
			name: "jeans_wavenumber(200, 1e-18) ≈ 1.4480e-16 m⁻¹",
			code: `{{FUNC}}
print(f"{jeans_wavenumber(200, 1e-18):.4e}")`,
			expected: "1.4480e-16\n",
		},
		{
			name: "jeans_wavelength_m(200, 1e-18) ≈ 4.3392e+16 m",
			code: `{{FUNC}}
print(f"{jeans_wavelength_m(200, 1e-18):.4e}")`,
			expected: "4.3392e+16\n",
		},
		{
			name: "jeans_mass_kg(200, 1e-18) ≈ 42.7793 × 10³⁰ kg",
			code: `{{FUNC}}
print(round(jeans_mass_kg(200, 1e-18) / 1e30, 4))`,
			expected: "42.7793\n",
		},
		{
			name: "jeans_wavelength_m(200, 1e-18) / 3.086e16 ≈ 1.4061 parsecs",
			code: `{{FUNC}}
print(round(jeans_wavelength_m(200, 1e-18) / 3.086e16, 4))`,
			expected: "1.4061\n",
		},
	],
};
