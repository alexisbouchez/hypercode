import type { Lesson } from "../../types";

export const darkEnergy: Lesson = {
	id: "dark-energy",
	title: "Dark Energy",
	chapterId: "dark-sector",
	content: `## Dark Energy

In 1998, observations of distant Type Ia supernovae revealed that the expansion of the universe is **accelerating** — a discovery that earned the 2011 Nobel Prize. The cause is called **dark energy**, making up about 68% of the total energy budget of the universe.

### The Cosmological Constant

The simplest model of dark energy is Einstein's cosmological constant Λ, with equation of state:

$$w = \\frac{p}{\\rho c^2} = -1$$

This means dark energy has constant energy density regardless of how much the universe expands:

$$\\rho_{\\rm DE}(a) = \\rho_{\\rm DE,0} \\cdot a^{-3(1+w)} = \\rho_{\\rm DE,0} \\quad (w = -1)$$

### General Dark Energy

A more general dark energy model allows $w \\neq -1$. The density evolves as:

$$\\frac{\\rho_{\\rm DE}(a)}{\\rho_{\\rm DE,0}} = a^{-3(1+w)}$$

### The Critical Density

The present-day dark energy density is:

$$\\rho_{\\rm DE,0} = \\Omega_\\Lambda \\cdot \\rho_c = \\Omega_\\Lambda \\cdot \\frac{3 H_0^2}{8\\pi G}$$

where $H_0$ is the Hubble constant in SI units: $H_0 = H_{0,\\rm km/s/Mpc} \\times 1000 / (3.0857 \\times 10^{22}$ m).

For $\\Omega_\\Lambda = 0.7$ and $H_0 = 70$ km/s/Mpc: $\\rho_{\\rm DE,0} \\approx 6.4 \\times 10^{-27}$ kg/m³.

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`dark_energy_density_ratio(a, w)\` — returns $\\rho_{\\rm DE}(a) / \\rho_{\\rm DE,0} = a^{-3(1+w)}$
- \`dark_energy_density_today_kg_m3(Omega_Lambda, H0_km_s_Mpc)\` — returns $\\rho_{\\rm DE,0}$ in kg/m³; use $G = 6.674 \\times 10^{-11}$, $H_0^{\\rm SI} = H_0 \\times 1000 / 3.0857 \\times 10^{22}$
- \`equation_of_state_pressure(w, rho_DE_kg_m3)\` — returns $p = w \\rho c^2$ in Pa; use $c = 2.998 \\times 10^8$ m/s`,

	starterCode: `import math

def dark_energy_density_ratio(a, w):
    # TODO: return a ** (-3 * (1 + w))
    pass

def dark_energy_density_today_kg_m3(Omega_Lambda, H0_km_s_Mpc):
    G = 6.674e-11
    # TODO: convert H0 to SI, then return Omega_Lambda * 3 * H0_SI**2 / (8 * pi * G)
    pass

def equation_of_state_pressure(w, rho_DE_kg_m3):
    c = 2.998e8
    # TODO: return w * rho * c^2
    pass

print(dark_energy_density_ratio(1.0, -1))
print(round(dark_energy_density_ratio(0.5, -1), 4))
print(round(dark_energy_density_today_kg_m3(0.7, 70) / 1e-27, 4))
print(round(equation_of_state_pressure(-1, 6e-27) / 1e-10, 4))
`,

	solution: `import math

def dark_energy_density_ratio(a, w):
    return a ** (-3 * (1 + w))

def dark_energy_density_today_kg_m3(Omega_Lambda, H0_km_s_Mpc):
    G = 6.674e-11
    H0_SI = H0_km_s_Mpc * 1000 / 3.0857e22
    return Omega_Lambda * 3 * H0_SI**2 / (8 * math.pi * G)

def equation_of_state_pressure(w, rho_DE_kg_m3):
    c = 2.998e8
    return w * rho_DE_kg_m3 * c**2

print(dark_energy_density_ratio(1.0, -1))
print(round(dark_energy_density_ratio(0.5, -1), 4))
print(round(dark_energy_density_today_kg_m3(0.7, 70) / 1e-27, 4))
print(round(equation_of_state_pressure(-1, 6e-27) / 1e-10, 4))
`,

	tests: [
		{
			name: "dark_energy_density_ratio(1.0, -1) = 1.0 (Λ doesn't dilute)",
			code: `{{FUNC}}
print(dark_energy_density_ratio(1.0, -1))`,
			expected: "1.0\n",
		},
		{
			name: "dark_energy_density_ratio(0.5, -1) = 1.0 (cosmological constant at a=0.5)",
			code: `{{FUNC}}
print(round(dark_energy_density_ratio(0.5, -1), 4))`,
			expected: "1.0\n",
		},
		{
			name: "dark_energy_density_today_kg_m3(0.7, 70) ≈ 6.4429e-27 kg/m³",
			code: `{{FUNC}}
print(round(dark_energy_density_today_kg_m3(0.7, 70) / 1e-27, 4))`,
			expected: "6.4429\n",
		},
		{
			name: "equation_of_state_pressure(-1, 6e-27) ≈ -5.3928e-10 Pa (p = -ρc²)",
			code: `{{FUNC}}
print(round(equation_of_state_pressure(-1, 6e-27) / 1e-10, 4))`,
			expected: "-5.3928\n",
		},
	],
};
