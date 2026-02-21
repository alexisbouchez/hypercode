import type { Lesson } from "../../types";

export const gravitationalLensing: Lesson = {
	id: "gravitational-lensing",
	title: "Gravitational Lensing",
	chapterId: "galactic",
	content: `## Gravitational Lensing

Einstein's general relativity predicts that mass bends the path of light. A massive object — a star, galaxy, or cluster — acts as a gravitational lens, distorting and magnifying the images of background sources.

### The Einstein Radius

When a source, lens, and observer are perfectly aligned, the source appears as a ring (an **Einstein ring**) with angular radius $\\theta_E$:

$$\\theta_E = \\sqrt{\\frac{4GM}{c^2} \\cdot \\frac{D_{LS}}{D_L D_S}}$$

where:
- $D_L$ = angular diameter distance to the lens
- $D_S$ = angular diameter distance to the source  
- $D_{LS}$ = angular diameter distance from lens to source

### Point-Source Magnification

For a point source at angular position $\\beta$ from the lens axis, the total magnification is:

$$\\mu = \\frac{u^2 + 2}{u \\sqrt{u^2 + 4}}, \\quad u = \\frac{\\beta}{\\theta_E}$$

At perfect alignment ($\\beta = 0$, $u = 0$), $\\mu \\to \\infty$. At $u = 1$, $\\mu \\approx 1.34$.

### Scales

| System | $\\theta_E$ |
|--------|------------|
| Galaxy (1e11 $M_\\odot$, 500 Mpc) | $\\sim 1^{\\prime\\prime}$ |
| Microlensing ($1\\,M_\\odot$, 1 kpc) | $\\sim 1$ mas |

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`einstein_radius_rad(M_kg, D_L_m, D_S_m, D_LS_m)\` — returns $\\theta_E$ in radians
- \`einstein_radius_arcsec(M_kg, D_L_m, D_S_m, D_LS_m)\` — returns $\\theta_E$ in arcseconds
- \`lensing_magnification(beta_rad, theta_E_rad)\` — returns $\\mu$

Use $G = 6.674 \\times 10^{-11}$ N m² kg⁻², $c = 2.998 \\times 10^8$ m/s. For arcseconds: multiply radians by $180/\\pi \\times 3600$.`,

	starterCode: `import math

def einstein_radius_rad(M_kg, D_L_m, D_S_m, D_LS_m):
    G = 6.674e-11
    c = 2.998e8
    # TODO: return sqrt(4*G*M_kg/c^2 * D_LS_m/(D_L_m*D_S_m))
    pass

def einstein_radius_arcsec(M_kg, D_L_m, D_S_m, D_LS_m):
    G = 6.674e-11
    c = 2.998e8
    # TODO: convert einstein_radius_rad to arcseconds (* 180/pi * 3600)
    pass

def lensing_magnification(beta_rad, theta_E_rad):
    # TODO: u = beta_rad/theta_E_rad; return (u^2+2)/(u*sqrt(u^2+4))
    pass

Mpc = 3.086e22
M_gal = 1e11 * 1.989e30
print(round(einstein_radius_arcsec(M_gal, 500*Mpc, 1000*Mpc, 500*Mpc), 4))
`,

	solution: `import math

def einstein_radius_rad(M_kg, D_L_m, D_S_m, D_LS_m):
    G = 6.674e-11
    c = 2.998e8
    return math.sqrt(4 * G * M_kg / c**2 * D_LS_m / (D_L_m * D_S_m))

def einstein_radius_arcsec(M_kg, D_L_m, D_S_m, D_LS_m):
    G = 6.674e-11
    c = 2.998e8
    rad = math.sqrt(4 * G * M_kg / c**2 * D_LS_m / (D_L_m * D_S_m))
    return rad * 180 / math.pi * 3600

def lensing_magnification(beta_rad, theta_E_rad):
    u = beta_rad / theta_E_rad
    return (u**2 + 2) / (u * math.sqrt(u**2 + 4))

Mpc = 3.086e22
M_gal = 1e11 * 1.989e30
print(round(einstein_radius_arcsec(M_gal, 500*Mpc, 1000*Mpc, 500*Mpc), 4))
`,

	tests: [
		{
			name: "Galaxy Einstein radius ≈ 0.9025 arcsec (1e11 Msun at 500 Mpc)",
			code: `{{FUNC}}
Mpc = 3.086e22
M_gal = 1e11 * 1.989e30
print(round(einstein_radius_arcsec(M_gal, 500*Mpc, 1000*Mpc, 500*Mpc), 4))`,
			expected: "0.9025\n",
		},
		{
			name: "Microlensing Einstein radius ≈ 0.002018 arcsec (1 Msun at 1 kpc)",
			code: `{{FUNC}}
kpc = 3.086e19
M_star = 1.989e30
print(round(einstein_radius_arcsec(M_star, 1*kpc, 2*kpc, 1*kpc), 6))`,
			expected: "0.002018\n",
		},
		{
			name: "Magnification at u=1 ≈ 1.3416",
			code: `{{FUNC}}
print(round(lensing_magnification(1.0, 1.0), 4))`,
			expected: "1.3416\n",
		},
		{
			name: "Magnification at u=0.5 ≈ 2.1828 (closer alignment = higher magnification)",
			code: `{{FUNC}}
print(round(lensing_magnification(0.5, 1.0), 4))`,
			expected: "2.1828\n",
		},
	],
};
