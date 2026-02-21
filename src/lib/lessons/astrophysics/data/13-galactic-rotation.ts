import type { Lesson } from "../../types";

export const galacticRotation: Lesson = {
	id: "galactic-rotation",
	title: "Galactic Rotation Curves",
	chapterId: "galactic",
	content: `## Galactic Rotation Curves

Stars and gas orbit the center of a galaxy under gravity. The circular velocity $v_c(r)$ at radius $r$ depends on the total mass $M(r)$ enclosed within that radius:

$$v_c(r) = \\sqrt{\\frac{G\\,M(r)}{r}}$$

### Three Regimes

| Mass distribution | $M(r)$ | $v_c(r)$ |
|-------------------|---------|----------|
| Uniform sphere ($\\rho = $ const) | $\\propto r^3$ | $\\propto r$ (solid body) |
| Point mass / Keplerian | const | $\\propto r^{-1/2}$ |
| Flat rotation curve | $\\propto r$ | const |

### The Dark Matter Problem

Observations of spiral galaxies show that $v_c(r) \\approx $ const far beyond the visible disk — a **flat rotation curve**. This cannot be explained by visible matter alone. For a flat curve, the enclosed mass must grow as:

$$M(r) = \\frac{v_c^2\\,r}{G}$$

This implies vast amounts of unseen **dark matter** in an extended halo.

### The Milky Way

Our Galaxy has a roughly flat rotation curve with $v_c \\approx 220$ km/s at the Sun's position $r \\approx 8.5$ kpc. Using $1\\text{ kpc} = 3.086 \\times 10^{19}$ m, the enclosed mass within 8.5 kpc is roughly $10^{11}\\,M_\\odot$.

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`circular_velocity_m_s(M_enclosed_kg, r_m)\` — returns $v_c = \\sqrt{G M / r}$ in m/s
- \`enclosed_mass_flat_rotation(v_c_m_s, r_m)\` — returns $M = v_c^2 r / G$ in kg (mass implied by a flat rotation curve)
- \`kpc_to_m(kpc)\` — returns distance in metres ($1\\text{ kpc} = 3.086 \\times 10^{19}$ m)

Use $G = 6.674 \\times 10^{-11}$ N m² kg⁻².`,

	starterCode: `import math

def circular_velocity_m_s(M_enclosed_kg, r_m):
    G = 6.674e-11
    # TODO: return sqrt(G * M_enclosed_kg / r_m)
    pass

def enclosed_mass_flat_rotation(v_c_m_s, r_m):
    G = 6.674e-11
    # TODO: return v_c_m_s**2 * r_m / G
    pass

def kpc_to_m(kpc):
    # TODO: return kpc * 3.086e19
    pass

r_mw = kpc_to_m(8.5)
M_enc = enclosed_mass_flat_rotation(220e3, r_mw)
print(round(M_enc / 1.989e30 / 1e11, 4))
`,

	solution: `import math

def circular_velocity_m_s(M_enclosed_kg, r_m):
    G = 6.674e-11
    return math.sqrt(G * M_enclosed_kg / r_m)

def enclosed_mass_flat_rotation(v_c_m_s, r_m):
    G = 6.674e-11
    return v_c_m_s**2 * r_m / G

def kpc_to_m(kpc):
    return kpc * 3.086e19

r_mw = kpc_to_m(8.5)
M_enc = enclosed_mass_flat_rotation(220e3, r_mw)
print(round(M_enc / 1.989e30 / 1e11, 4))
`,

	tests: [
		{
			name: "kpc_to_m(1) = 3.086e19 m",
			code: `{{FUNC}}
print(kpc_to_m(1) == 3.086e19)`,
			expected: "True\n",
		},
		{
			name: "Milky Way enclosed mass ≈ 0.9564 × 10^11 Msun within 8.5 kpc",
			code: `{{FUNC}}
M_enc = enclosed_mass_flat_rotation(220e3, kpc_to_m(8.5))
print(round(M_enc / 1.989e30 / 1e11, 4))`,
			expected: "0.9564\n",
		},
		{
			name: "circular_velocity_m_s(1e11 Msun, 8.5 kpc) ≈ 224.96 km/s",
			code: `{{FUNC}}
v = circular_velocity_m_s(1e11 * 1.989e30, kpc_to_m(8.5))
print(round(v / 1e3, 2))`,
			expected: "224.96\n",
		},
		{
			name: "enclosed_mass_flat_rotation roundtrip with circular_velocity_m_s",
			code: `{{FUNC}}
r = kpc_to_m(8.5)
M = enclosed_mass_flat_rotation(220e3, r)
v = circular_velocity_m_s(M, r)
print(round(v / 1e3, 4))`,
			expected: "220.0\n",
		},
	],
};
