import type { Lesson } from "../../types";

export const gravitationalLensing: Lesson = {
	id: "gravitational-lensing",
	title: "Gravitational Lensing",
	chapterId: "gr-predictions",
	content: `## Gravitational Lensing

General relativity predicts that mass curves spacetime, bending the path of light. When light from a distant source passes a massive object at impact parameter $b$, it is deflected by:

$$\\alpha = \\frac{4GM}{c^2 b} = \\frac{2 r_s}{b} \\quad \\text{(radians)}$$

This is **twice** the Newtonian prediction. The factor of 2 arises because GR accounts for both the spatial and temporal curvature of spacetime.

### The 1919 Eclipse Measurement

In 1919, Arthur Eddington's expedition measured starlight deflection during a solar eclipse. For light grazing the Sun's surface ($b = R_\\odot = 6.96 \\times 10^8$ m):

$$\\alpha = \\frac{4GM_\\odot}{c^2 R_\\odot} \\approx 1.75 \\text{ arcseconds}$$

This matched GR's prediction and made Einstein world-famous. Newton's theory predicted only 0.875 arcseconds.

### Einstein Ring

When source, lens, and observer are perfectly aligned, light bends symmetrically to form a ring. The angular radius of the Einstein ring is:

$$\\theta_E = \\sqrt{\\frac{4GM}{c^2} \\cdot \\frac{D_{LS}}{D_L D_S}}$$

For collinear geometry with $D_{LS} = D_S - D_L$, the physical ring radius projected at the lens plane is:

$$r_E = D_L \\, \\theta_E = \\sqrt{\\frac{4GM \\, D_L (D_S - D_L)}{c^2 D_S}}$$

### Your Task

Implement three functions. All physical constants must be defined **inside** each function body.

- \`deflection_angle(M, b)\` — returns deflection in radians
- \`deflection_angle_arcsec(M, b)\` — converts radians to arcseconds (multiply by $180/\\pi \\times 3600$)
- \`einstein_ring_radius(M, D_L, D_S)\` — returns physical Einstein ring radius in meters`,

	starterCode: `import math

def deflection_angle(M, b):
    G = 6.674e-11
    c = 299792458.0
    # alpha = 4*G*M / (c^2 * b)
    pass

def deflection_angle_arcsec(M, b):
    G = 6.674e-11
    c = 299792458.0
    # convert radians to arcseconds: * 180/pi * 3600
    pass

def einstein_ring_radius(M, D_L, D_S):
    G = 6.674e-11
    c = 299792458.0
    # r_E = sqrt(4*G*M * D_L * (D_S - D_L) / (c^2 * D_S))
    pass

M_sun = 1.989e30
R_sun = 6.96e8
print(round(deflection_angle_arcsec(M_sun, R_sun), 4))
`,

	solution: `import math

def deflection_angle(M, b):
    G = 6.674e-11
    c = 299792458.0
    return 4 * G * M / (c**2 * b)

def deflection_angle_arcsec(M, b):
    G = 6.674e-11
    c = 299792458.0
    alpha = 4 * G * M / (c**2 * b)
    return alpha * 180 / math.pi * 3600

def einstein_ring_radius(M, D_L, D_S):
    G = 6.674e-11
    c = 299792458.0
    D_LS = D_S - D_L
    return math.sqrt(4 * G * M * D_L * D_LS / (c**2 * D_S))

M_sun = 1.989e30
R_sun = 6.96e8
print(round(deflection_angle_arcsec(M_sun, R_sun), 4))
`,

	tests: [
		{
			name: "deflection_angle(solar mass, solar radius) ≈ 8.49e-6 radians",
			code: `{{FUNC}}
print(round(deflection_angle(1.989e30, 6.96e8), 10))`,
			expected: "8.4885e-06\n",
		},
		{
			name: "deflection_angle_arcsec(solar mass, solar radius) ≈ 1.75 arcsec (1919 result)",
			code: `{{FUNC}}
print(round(deflection_angle_arcsec(1.989e30, 6.96e8), 4))`,
			expected: "1.7509\n",
		},
		{
			name: "deflection_angle_arcsec for Jupiter grazing ≈ 0.0163 arcsec",
			code: `{{FUNC}}
print(round(deflection_angle_arcsec(1.898e27, 7.15e7), 4))`,
			expected: "0.0163\n",
		},
		{
			name: "einstein_ring_radius for galaxy at 1 Gpc, source at 2 Gpc",
			code: `{{FUNC}}
print(f'{einstein_ring_radius(1e41, 1e25, 2e25):.4e}')`,
			expected: "3.8538e+19\n",
		},
	],
};
