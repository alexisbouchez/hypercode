import type { Lesson } from "../../types";

export const gwStrain: Lesson = {
	id: "gw-strain",
	title: "Gravitational Wave Strain",
	chapterId: "gravitational-waves",
	content: `## Gravitational Wave Strain

The gravitational wave **strain** $h$ is a dimensionless number that measures the fractional stretching of space. If a detector of length $L$ is stretched by $\\delta L$, then $h = 2\\delta L/L$. LIGO measures strains of order $10^{-21}$ — one part in a billion billion billion.

For a compact binary at luminosity distance $d$, with chirp mass $\\mathcal{M}_c$ and instantaneous GW frequency $f$, the strain amplitude from the leading-order **quadrupole formula** is:

$$h = \\frac{4}{d} \\cdot \\frac{G\\mathcal{M}_c}{c^2} \\cdot \\left(\\frac{\\pi G \\mathcal{M}_c f}{c^3}\\right)^{2/3}$$

This grows during the inspiral as $f$ increases, reaching its peak at the ISCO frequency.

### GW150914 Numbers

| Parameter | Value |
|-----------|-------|
| $m_1 \\approx m_2$ | $\\sim 30\\,M_\\odot$ |
| $d$ | $\\sim 410$ Mpc |
| $f_{\\rm ISCO}$ | $\\sim 73$ Hz |
| Peak strain | $\\sim 10^{-21}$ |

### Distance Conversion

One megaparsec (Mpc): $1\\text{ Mpc} = 3.0857 \\times 10^{22}$ m.

### Constants (define inside each function)
- $G = 6.674 \\times 10^{-11}$ m³ kg⁻¹ s⁻²
- $c = 299792458$ m/s

### Your Task

Implement the three functions below. The chirp mass and ISCO frequency formulae from previous lessons may be computed inline inside \`gw_strain_at_isco\`.`,

	starterCode: `import math

def gw_strain(M_c, f, d):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return (4/d) * (G*M_c/c^2) * (pi*G*M_c*f/c^3)^(2/3)
    pass

def gw_strain_at_isco(m1, m2, d):
    G = 6.674e-11
    c = 299792458.0
    # TODO: compute M_c and f_isco inline, then return gw_strain formula
    pass

def luminosity_distance_mpc_to_m(d_mpc):
    # TODO: return d_mpc * 3.0857e22
    pass

M_sun = 1.989e30
d410 = luminosity_distance_mpc_to_m(410)
print(f"{luminosity_distance_mpc_to_m(1):.4e}")
print(f"{gw_strain_at_isco(30*M_sun, 30*M_sun, d410):.4e}")
`,

	solution: `import math

def gw_strain(M_c, f, d):
    G = 6.674e-11
    c = 299792458.0
    return (4/d) * (G*M_c/c**2) * (math.pi*G*M_c*f/c**3)**(2/3)

def gw_strain_at_isco(m1, m2, d):
    G = 6.674e-11
    c = 299792458.0
    M_c = (m1*m2)**(3/5) / (m1+m2)**(1/5)
    f = c**3 / (6**(3/2) * math.pi * G * (m1+m2))
    return (4/d) * (G*M_c/c**2) * (math.pi*G*M_c*f/c**3)**(2/3)

def luminosity_distance_mpc_to_m(d_mpc):
    return d_mpc * 3.0857e22

M_sun = 1.989e30
d410 = luminosity_distance_mpc_to_m(410)
print(f"{luminosity_distance_mpc_to_m(1):.4e}")
print(f"{gw_strain_at_isco(30*M_sun, 30*M_sun, d410):.4e}")
`,

	tests: [
		{
			name: "luminosity_distance_mpc_to_m(1 Mpc) = 3.0857e+22 m",
			code: `{{FUNC}}
print(f"{luminosity_distance_mpc_to_m(1):.4e}")`,
			expected: "3.0857e+22\n",
		},
		{
			name: "gw_strain_at_isco for GW150914-like binary at 410 Mpc ≈ 1.17e-21",
			code: `{{FUNC}}
M_sun = 1.989e30
d410 = luminosity_distance_mpc_to_m(410)
print(f"{gw_strain_at_isco(30*M_sun, 30*M_sun, d410):.4e}")`,
			expected: "1.1675e-21\n",
		},
		{
			name: "gw_strain at 10 Hz early inspiral (GW150914-like, 410 Mpc)",
			code: `{{FUNC}}
import math
M_sun = 1.989e30
M_c = (30*M_sun*30*M_sun)**(3/5) / (60*M_sun)**(1/5)
d410 = luminosity_distance_mpc_to_m(410)
print(f"{gw_strain(M_c, 10, d410):.4e}")`,
			expected: "3.0948e-22\n",
		},
		{
			name: "gw_strain_at_isco at closer distance 100 Mpc is larger",
			code: `{{FUNC}}
M_sun = 1.989e30
d100 = luminosity_distance_mpc_to_m(100)
print(f"{gw_strain_at_isco(30*M_sun, 30*M_sun, d100):.4e}")`,
			expected: "4.7866e-21\n",
		},
	],
};
