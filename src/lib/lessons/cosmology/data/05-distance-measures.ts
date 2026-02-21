import type { Lesson } from "../../types";

export const distanceMeasures: Lesson = {
	id: "distance-measures",
	title: "Distance Measures in Cosmology",
	chapterId: "expansion",
	content: `## Distance Measures in Cosmology

The comoving distance $d_C$ is the fundamental coordinate distance, but astronomers use several derived distance measures depending on what is being observed. Each has a clear physical meaning and practical application.

### Luminosity Distance

The **luminosity distance** $d_L$ relates the intrinsic luminosity $L$ of an object to its observed flux $F$:

$$F = \\frac{L}{4\\pi d_L^2}$$

In terms of the comoving distance:

$$d_L = d_C \\cdot (1+z)$$

The extra factor of $(1+z)$ accounts for two effects: photon energies are redshifted by $(1+z)$, and photon arrival rate is also reduced by $(1+z)$. Luminosity distance is used with **standard candles** like Type Ia supernovae.

### Angular Diameter Distance

The **angular diameter distance** $d_A$ relates the physical size $\\ell$ of an object to its observed angular size $\\theta$:

$$\\theta = \\frac{\\ell}{d_A}$$

$$d_A = \\frac{d_C}{1+z}$$

This is used with **standard rulers** like the Baryon Acoustic Oscillation (BAO) scale. Note that $d_A$ actually *decreases* beyond $z \\approx 1.6$ in $\\Lambda$CDM — very distant objects appear larger!

### The Distance Modulus

For observational astronomy, the **distance modulus** $\\mu$ converts luminosity distance to magnitudes:

$$\\mu = 5 \\log_{10}\\left(\\frac{d_L}{10 \\text{ pc}}\\right) = 5 \\log_{10}(d_L^{\\text{Mpc}}) + 25$$

since $d_L^{\\text{Mpc}} \\times 10^6 / 10 = d_L^{\\text{Mpc}} \\times 10^5$.

| Distance measure | Formula | Use case |
|-----------------|---------|----------|
| Comoving $d_C$ | $\\int_0^z c\\,dz'/H(z')$ | Large-scale structure |
| Luminosity $d_L$ | $d_C(1+z)$ | Standard candles (SNe Ia) |
| Angular diameter $d_A$ | $d_C/(1+z)$ | Standard rulers (BAO) |
| Distance modulus $\\mu$ | $5\\log_{10}(d_L/10\\text{ pc})$ | Magnitude system |

### Your Task

Implement the following functions. The constant $\\pi$ is available via \`import math\`. All other constants must be defined **inside** each function body.

- \`luminosity_distance_Mpc(d_comoving_Mpc, z)\` — returns $d_L = d_C(1+z)$ in Mpc
- \`angular_diameter_distance_Mpc(d_comoving_Mpc, z)\` — returns $d_A = d_C/(1+z)$ in Mpc
- \`distance_modulus(d_L_Mpc)\` — returns $\\mu = 5\\log_{10}(d_L^{\\text{Mpc}} \\times 10^6 / 10)$`,

	starterCode: `import math

def luminosity_distance_Mpc(d_comoving_Mpc, z):
    # d_L = d_C * (1 + z)
    pass

def angular_diameter_distance_Mpc(d_comoving_Mpc, z):
    # d_A = d_C / (1 + z)
    pass

def distance_modulus(d_L_Mpc):
    # mu = 5 * log10(d_L_Mpc * 1e6 / 10)
    pass

print(luminosity_distance_Mpc(3303.8, 1.0))
print(angular_diameter_distance_Mpc(3303.8, 1.0))
print(round(distance_modulus(3303.8), 4))
`,

	solution: `import math

def luminosity_distance_Mpc(d_comoving_Mpc, z):
    return d_comoving_Mpc * (1 + z)

def angular_diameter_distance_Mpc(d_comoving_Mpc, z):
    return d_comoving_Mpc / (1 + z)

def distance_modulus(d_L_Mpc):
    return 5 * math.log10(d_L_Mpc * 1e6 / 10)

print(luminosity_distance_Mpc(3303.8, 1.0))
print(angular_diameter_distance_Mpc(3303.8, 1.0))
print(round(distance_modulus(3303.8), 4))
`,

	tests: [
		{
			name: "luminosity_distance_Mpc(3303.8, 1.0) = 6607.6 Mpc",
			code: `{{FUNC}}
print(luminosity_distance_Mpc(3303.8, 1.0))`,
			expected: "6607.6\n",
		},
		{
			name: "angular_diameter_distance_Mpc(3303.8, 1.0) = 1651.9 Mpc",
			code: `{{FUNC}}
print(angular_diameter_distance_Mpc(3303.8, 1.0))`,
			expected: "1651.9\n",
		},
		{
			name: "distance_modulus(6607.6) ≈ 44.1002 mag",
			code: `{{FUNC}}
print(round(distance_modulus(6607.6), 4))`,
			expected: "44.1002\n",
		},
		{
			name: "distance_modulus(10) = 30.0 mag (10 Mpc standard reference)",
			code: `{{FUNC}}
print(round(distance_modulus(10), 4))`,
			expected: "30.0\n",
		},
	],
};
