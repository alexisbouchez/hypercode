import type { Lesson } from "../../types";

export const criticalDensity: Lesson = {
	id: "critical-density",
	title: "Critical Density",
	chapterId: "thermal-history",
	content: `## Critical Density

The **critical density** ρ_c is the exact matter density required for a spatially flat universe — the boundary between a universe that expands forever and one that eventually recollapses.

### The Critical Density Formula

From the Friedmann equation with zero spatial curvature (k = 0):

$$\\rho_c = \\frac{3H^2}{8\\pi G}$$

With H₀ = 70 km/s/Mpc and G = 6.674 × 10⁻¹¹ m³/(kg·s²):

$$\\rho_c \\approx 9.20 \\times 10^{-27} \\text{ kg/m}^3$$

This is incredibly sparse — roughly 5–6 hydrogen atoms per cubic metre.

### Converting H₀ to SI

The Hubble constant in SI units:

$$H_0 = 70 \\frac{\\text{km/s}}{\\text{Mpc}} = \\frac{70 \\times 10^3}{3.0857 \\times 10^{22}} \\text{ s}^{-1} \\approx 2.27 \\times 10^{-18} \\text{ s}^{-1}$$

### The Density Parameter Ω

The density parameter normalises matter density to the critical density:

$$\\Omega = \\frac{\\rho}{\\rho_c}$$

A flat universe has Ω_total = 1. Observations give Ω_m ≈ 0.3 (matter) and Ω_Λ ≈ 0.7 (dark energy).

### Your Task

Implement three functions. All constants must be defined **inside** each function body.

- \`critical_density_kg_m3(H0_km_s_Mpc)\` — returns ρ_c in kg/m³
- \`omega_matter(rho_m_kg_m3, H0_km_s_Mpc)\` — returns Ω_m = ρ_m / ρ_c
- \`rho_from_omega(Omega, H0_km_s_Mpc)\` — returns ρ = Ω × ρ_c in kg/m³`,

	starterCode: `import math

def critical_density_kg_m3(H0_km_s_Mpc):
    G = 6.674e-11  # m^3 / (kg * s^2)
    # H0_SI = H0_km_s_Mpc * 1000 / 3.0857e22
    # TODO: return 3 * H0_SI^2 / (8 * pi * G)
    pass

def omega_matter(rho_m_kg_m3, H0_km_s_Mpc):
    G = 6.674e-11
    # TODO: compute rho_c, return rho_m / rho_c
    pass

def rho_from_omega(Omega, H0_km_s_Mpc):
    G = 6.674e-11
    # TODO: compute rho_c, return Omega * rho_c
    pass

print(round(critical_density_kg_m3(70) / 1e-27, 4))
print(round(omega_matter(2.8e-27, 70), 4))
print(round(rho_from_omega(0.3, 70) / 1e-27, 4))
print(round(critical_density_kg_m3(67.4) / 1e-27, 4))
`,

	solution: `import math

def critical_density_kg_m3(H0_km_s_Mpc):
    G = 6.674e-11  # m^3 / (kg * s^2)
    H0_SI = H0_km_s_Mpc * 1000 / 3.0857e22
    return 3 * H0_SI**2 / (8 * math.pi * G)

def omega_matter(rho_m_kg_m3, H0_km_s_Mpc):
    G = 6.674e-11
    H0_SI = H0_km_s_Mpc * 1000 / 3.0857e22
    rho_c = 3 * H0_SI**2 / (8 * math.pi * G)
    return rho_m_kg_m3 / rho_c

def rho_from_omega(Omega, H0_km_s_Mpc):
    G = 6.674e-11
    H0_SI = H0_km_s_Mpc * 1000 / 3.0857e22
    rho_c = 3 * H0_SI**2 / (8 * math.pi * G)
    return Omega * rho_c

print(round(critical_density_kg_m3(70) / 1e-27, 4))
print(round(omega_matter(2.8e-27, 70), 4))
print(round(rho_from_omega(0.3, 70) / 1e-27, 4))
print(round(critical_density_kg_m3(67.4) / 1e-27, 4))
`,

	tests: [
		{
			name: "critical_density_kg_m3(70) ≈ 9.2042e-27 kg/m³",
			code: `{{FUNC}}
print(round(critical_density_kg_m3(70) / 1e-27, 4))`,
			expected: "9.2042\n",
		},
		{
			name: "omega_matter(2.8e-27, 70) ≈ 0.3042",
			code: `{{FUNC}}
print(round(omega_matter(2.8e-27, 70), 4))`,
			expected: "0.3042\n",
		},
		{
			name: "rho_from_omega(0.3, 70) ≈ 2.7612e-27 kg/m³",
			code: `{{FUNC}}
print(round(rho_from_omega(0.3, 70) / 1e-27, 4))`,
			expected: "2.7612\n",
		},
		{
			name: "critical_density_kg_m3(67.4) ≈ 8.5331e-27 kg/m³ (Planck H₀)",
			code: `{{FUNC}}
print(round(critical_density_kg_m3(67.4) / 1e-27, 4))`,
			expected: "8.5331\n",
		},
	],
};
