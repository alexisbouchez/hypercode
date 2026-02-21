import type { Lesson } from "../../types";

export const perihelionPrecession: Lesson = {
	id: "perihelion-precession",
	title: "Perihelion Precession",
	chapterId: "gr-predictions",
	content: `## Perihelion Precession

A planet orbiting the Sun does not follow a perfectly closed ellipse — its orbit slowly rotates. Most of this **perihelion advance** is explained by the gravitational pull of other planets, but a small residual remained unexplained by Newtonian gravity for decades. For Mercury, 43 arcseconds per century were unaccounted for.

General relativity provides the explanation. The extra precession per orbit is:

$$\\Delta\\phi = \\frac{6\\pi G M}{c^2 a (1 - e^2)} \\quad \\text{(radians per orbit)}$$

where:
- $M$ = mass of the central body (Sun)
- $a$ = semi-major axis of the orbit
- $e$ = orbital eccentricity

### Mercury's Parameters

| Parameter | Value |
|-----------|-------|
| Semi-major axis $a$ | $5.79 \\times 10^{10}$ m |
| Eccentricity $e$ | $0.2056$ |
| Orbital period | $87.97$ days |

Plugging these values into the GR formula gives:

$$\\Delta\\phi \\approx 5.02 \\times 10^{-7} \\text{ rad/orbit} \\approx 0.1036 \\text{ arcsec/orbit}$$

Mercury completes about $414.9$ orbits per century, giving:

$$\\Delta\\phi_{\\text{century}} \\approx 43.0 \\text{ arcsec/century}$$

This was one of the earliest triumphs of GR, confirmed with the observed anomalous precession known since 1859.

### Your Task

Implement three functions. All physical constants must be defined **inside** each function body.

- \`precession_per_orbit(M, a, e)\` — precession in radians per orbit
- \`precession_arcsec_per_orbit(M, a, e)\` — convert to arcseconds: multiply by $(180/\\pi) \\times 3600$
- \`precession_arcsec_per_century(M, a, e, period_days)\` — arcseconds per century: multiply by $(100 \\times 365.25 / \\text{period\\_days})$`,

	starterCode: `import math

def precession_per_orbit(M, a, e):
    G = 6.674e-11
    c = 299792458.0
    # 6 * pi * G * M / (c^2 * a * (1 - e^2))
    pass

def precession_arcsec_per_orbit(M, a, e):
    G = 6.674e-11
    c = 299792458.0
    # convert radians to arcseconds: * 180/pi * 3600
    pass

def precession_arcsec_per_century(M, a, e, period_days):
    G = 6.674e-11
    c = 299792458.0
    # arcsec/orbit * (100 * 365.25 / period_days)
    pass

M_sun = 1.989e30
# Mercury: a=5.79e10 m, e=0.2056, T=87.97 days
print(round(precession_arcsec_per_century(M_sun, 5.79e10, 0.2056, 87.97), 2))
`,

	solution: `import math

def precession_per_orbit(M, a, e):
    G = 6.674e-11
    c = 299792458.0
    return 6 * math.pi * G * M / (c**2 * a * (1 - e**2))

def precession_arcsec_per_orbit(M, a, e):
    G = 6.674e-11
    c = 299792458.0
    rad = 6 * math.pi * G * M / (c**2 * a * (1 - e**2))
    return rad * 180 / math.pi * 3600

def precession_arcsec_per_century(M, a, e, period_days):
    G = 6.674e-11
    c = 299792458.0
    rad = 6 * math.pi * G * M / (c**2 * a * (1 - e**2))
    arcsec = rad * 180 / math.pi * 3600
    return arcsec * (100 * 365.25 / period_days)

M_sun = 1.989e30
# Mercury: a=5.79e10 m, e=0.2056, T=87.97 days
print(round(precession_arcsec_per_century(M_sun, 5.79e10, 0.2056, 87.97), 2))
`,

	tests: [
		{
			name: "precession_per_orbit for Mercury ≈ 5.02e-7 rad",
			code: `{{FUNC}}
print(round(precession_per_orbit(1.989e30, 5.79e10, 0.2056), 12))`,
			expected: "5.02065e-07\n",
		},
		{
			name: "precession_arcsec_per_orbit for Mercury ≈ 0.103558 arcsec",
			code: `{{FUNC}}
print(round(precession_arcsec_per_orbit(1.989e30, 5.79e10, 0.2056), 6))`,
			expected: "0.103558\n",
		},
		{
			name: "precession_arcsec_per_century for Mercury ≈ 43 arcsec/century",
			code: `{{FUNC}}
print(round(precession_arcsec_per_century(1.989e30, 5.79e10, 0.2056, 87.97), 2))`,
			expected: "43.0\n",
		},
		{
			name: "precession_arcsec_per_century for Earth ≈ 3.84 arcsec/century",
			code: `{{FUNC}}
print(round(precession_arcsec_per_century(1.989e30, 1.496e11, 0.0167, 365.25), 4))`,
			expected: "3.8397\n",
		},
	],
};
