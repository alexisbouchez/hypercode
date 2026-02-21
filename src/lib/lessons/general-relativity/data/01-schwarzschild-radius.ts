import type { Lesson } from "../../types";

export const schwarzschildRadius: Lesson = {
	id: "schwarzschild-radius",
	title: "Schwarzschild Radius",
	chapterId: "schwarzschild",
	content: `## Schwarzschild Radius

In 1916, Karl Schwarzschild found the first exact solution to Einstein's field equations — the geometry of spacetime surrounding a spherically symmetric, non-rotating mass $M$. The most famous prediction of this solution is the **Schwarzschild radius** $r_s$, the radius at which the escape velocity equals the speed of light:

$$r_s = \\frac{2GM}{c^2}$$

where $G = 6.674 \\times 10^{-11}$ m³ kg⁻¹ s⁻² is Newton's gravitational constant and $c = 299{,}792{,}458$ m/s.

If a mass $M$ is compressed inside a sphere of radius $r_s$, spacetime curves so severely that nothing — not even light — can escape. That boundary is the **event horizon** of a black hole.

### Typical Values

| Object | Mass (kg) | $r_s$ |
|--------|-----------|-------|
| Earth | $5.97 \\times 10^{24}$ | $\\approx 8.9$ mm |
| Sun | $1.99 \\times 10^{30}$ | $\\approx 2.95$ km |
| 1 kg | $1$ | $\\approx 1.5 \\times 10^{-27}$ m |

The Sun would need to be squeezed to a ball less than 3 km across to become a black hole. Earth's Schwarzschild radius is smaller than a penny.

### Compactness

A useful dimensionless measure is the **compactness** — the ratio of the Schwarzschild radius to the actual radius:

$$\\mathcal{C} = \\frac{r_s}{R} = \\frac{2GM}{c^2 R}$$

For the Sun, $\\mathcal{C} \\approx 4.2 \\times 10^{-6}$ (very Newtonian). For a neutron star, $\\mathcal{C} \\sim 0.4$.

### Your Task

Implement three functions. All physical constants must be defined **inside** each function body.

- \`schwarzschild_radius(M)\` — returns $r_s = 2GM/c^2$ in metres
- \`schwarzschild_mass(r_s)\` — inverse: given $r_s$, returns $M = r_s c^2 / (2G)$
- \`compactness(M, R)\` — returns $r_s / R = 2GM / (c^2 R)$`,

	starterCode: `import math

def schwarzschild_radius(M):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return 2 * G * M / c**2
    pass

def schwarzschild_mass(r_s):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return r_s * c**2 / (2 * G)
    pass

def compactness(M, R):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return (2 * G * M / c**2) / R
    pass

print(round(schwarzschild_radius(5.972e24), 4))
print(round(schwarzschild_radius(1.989e30), 0))
print('{:.4e}'.format(schwarzschild_mass(1.0)))
print(round(compactness(1.989e30, 6.96e8), 10))
`,

	solution: `import math

def schwarzschild_radius(M):
    G = 6.674e-11
    c = 299792458.0
    return 2 * G * M / c**2

def schwarzschild_mass(r_s):
    G = 6.674e-11
    c = 299792458.0
    return r_s * c**2 / (2 * G)

def compactness(M, R):
    G = 6.674e-11
    c = 299792458.0
    return (2 * G * M / c**2) / R

print(round(schwarzschild_radius(5.972e24), 4))
print(round(schwarzschild_radius(1.989e30), 0))
print('{:.4e}'.format(schwarzschild_mass(1.0)))
print(round(compactness(1.989e30, 6.96e8), 10))
`,

	tests: [
		{
			name: "schwarzschild_radius of Earth ≈ 0.0089 m",
			code: `{{FUNC}}
print(round(schwarzschild_radius(5.972e24), 4))`,
			expected: "0.0089\n",
		},
		{
			name: "schwarzschild_radius of Sun ≈ 2954.0 m",
			code: `{{FUNC}}
print(round(schwarzschild_radius(1.989e30), 0))`,
			expected: "2954.0\n",
		},
		{
			name: "schwarzschild_mass for r_s = 1.0 m ≈ 6.7333e+26 kg",
			code: `{{FUNC}}
print('{:.4e}'.format(schwarzschild_mass(1.0)))`,
			expected: "6.7333e+26\n",
		},
		{
			name: "compactness of Sun at R = 6.96e8 m ≈ 4.2442e-06",
			code: `{{FUNC}}
print(round(compactness(1.989e30, 6.96e8), 10))`,
			expected: "4.2442e-06\n",
		},
	],
};
