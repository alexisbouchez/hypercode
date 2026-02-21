import type { Lesson } from "../../types";

export const hydrostaticEquilibrium: Lesson = {
	id: "hydrostatic-equilibrium",
	title: "Hydrostatic Equilibrium",
	chapterId: "stellar-evolution",
	content: `## Hydrostatic Equilibrium

A star is not collapsing — it is in a delicate balance between gravity pulling inward and pressure pushing outward. This balance is called **hydrostatic equilibrium**.

### The Equation

At every point inside a star, the pressure gradient must exactly counteract gravity:

$$\\frac{dP}{dr} = -\\rho g = -\\frac{G M(r) \\rho}{r^2}$$

where $M(r)$ is the mass enclosed within radius $r$.

### Central Pressure Estimate

Using the virial theorem approximation, the central pressure of a star scales as:

$$P_c \\approx \\frac{G M^2}{4\\pi R^4}$$

For the Sun, this gives $P_c \\approx 9 \\times 10^{13}$ Pa — about a billion atmospheres.

### Free-Fall (Dynamical) Timescale

If pressure support were suddenly removed, a star would collapse under its own gravity on the **free-fall timescale**:

$$t_{\\rm ff} = \\sqrt{\\frac{3\\pi}{32 G \\rho}}$$

where $\\rho$ is the mean density. For the Sun, $t_{\\rm ff} \\approx 1769$ s — about 30 minutes.

### Mean Density

$$\\bar{\\rho} = \\frac{M}{\\frac{4}{3}\\pi R^3}$$

The Sun's mean density is about 1410 kg/m³ — slightly denser than water.

### Your Task

Implement three functions. Use $G = 6.674 \\times 10^{-11}$ m³ kg⁻¹ s⁻², defined **inside** each function.

- \`central_pressure_Pa(M_kg, R_m)\` — virial estimate of central pressure in Pa
- \`freefall_timescale_s(rho_kg_m3)\` — free-fall collapse timescale in seconds
- \`mean_density_kg_m3(M_kg, R_m)\` — mean density of a spherical body in kg/m³`,

	starterCode: `import math

def central_pressure_Pa(M_kg, R_m):
    G = 6.674e-11
    # TODO: return G * M_kg**2 / (4 * pi * R_m**4)
    pass

def freefall_timescale_s(rho_kg_m3):
    G = 6.674e-11
    # TODO: return sqrt(3 * pi / (32 * G * rho))
    pass

def mean_density_kg_m3(M_kg, R_m):
    # TODO: return M_kg / ((4/3) * pi * R_m**3)
    pass

M_sun = 1.989e30  # kg
R_sun = 6.957e8   # m
rho_sun = mean_density_kg_m3(M_sun, R_sun)
print(round(central_pressure_Pa(M_sun, R_sun) / 1e13, 4))
print(round(rho_sun, 2))
print(round(freefall_timescale_s(rho_sun), 1))
`,

	solution: `import math

def central_pressure_Pa(M_kg, R_m):
    G = 6.674e-11
    return G * M_kg**2 / (4 * math.pi * R_m**4)

def freefall_timescale_s(rho_kg_m3):
    G = 6.674e-11
    return math.sqrt(3 * math.pi / (32 * G * rho_kg_m3))

def mean_density_kg_m3(M_kg, R_m):
    return M_kg / (4/3 * math.pi * R_m**3)

M_sun = 1.989e30  # kg
R_sun = 6.957e8   # m
rho_sun = mean_density_kg_m3(M_sun, R_sun)
print(round(central_pressure_Pa(M_sun, R_sun) / 1e13, 4))
print(round(rho_sun, 2))
print(round(freefall_timescale_s(rho_sun), 1))
`,

	tests: [
		{
			name: "central_pressure_Pa for the Sun ≈ 8.9693 × 10¹³ Pa",
			code: `{{FUNC}}
M_sun = 1.989e30
R_sun = 6.957e8
print(round(central_pressure_Pa(M_sun, R_sun) / 1e13, 4))`,
			expected: "8.9693\n",
		},
		{
			name: "mean_density_kg_m3 for the Sun ≈ 1410.2 kg/m³",
			code: `{{FUNC}}
M_sun = 1.989e30
R_sun = 6.957e8
print(round(mean_density_kg_m3(M_sun, R_sun), 2))`,
			expected: "1410.2\n",
		},
		{
			name: "freefall_timescale_s for solar mean density ≈ 1769.0 s",
			code: `{{FUNC}}
M_sun = 1.989e30
R_sun = 6.957e8
rho_sun = mean_density_kg_m3(M_sun, R_sun)
print(round(freefall_timescale_s(rho_sun), 1))`,
			expected: "1769.0\n",
		},
		{
			name: "mean_density_kg_m3 for Earth ≈ 5513.26 kg/m³",
			code: `{{FUNC}}
M_earth = 5.972e24
R_earth = 6.371e6
print(round(mean_density_kg_m3(M_earth, R_earth), 2))`,
			expected: "5513.26\n",
		},
	],
};
