import type { Lesson } from "../../types";

export const comovingDistance: Lesson = {
	id: "comoving-distance",
	title: "Comoving Distance",
	chapterId: "expansion",
	content: `## Comoving Distance

When we say a galaxy is "3 billion light-years away", we need to be careful — in an expanding universe, distance is not a single number. The **comoving distance** is the coordinate distance that factors out the expansion of the universe, remaining constant as the universe grows.

### Definition

The comoving distance to a source at redshift $z$ is:

$$\\chi(z) = \\frac{c}{H_0} \\int_0^z \\frac{dz'}{E(z')}$$

where the **dimensionless Hubble parameter** is:

$$E(z) = \\frac{H(z)}{H_0} = \\sqrt{\\Omega_m(1+z)^3 + \\Omega_\\Lambda}$$

for a flat universe with negligible radiation, and $D_H = c/H_0$ is the Hubble distance.

### Why Comoving Distance?

Comoving coordinates expand with the universe. Two galaxies at fixed comoving positions are moving apart only because space itself is expanding — not because they have any peculiar velocity. This makes comoving distance the natural coordinate for large-scale structure.

### Numerical Integration

The integral has no closed-form solution for general $\\Omega_m$ and $\\Omega_\\Lambda$. We use the **trapezoidal rule** with $N = 1000$ steps:

$$\\chi(z) \\approx D_H \\cdot \\Delta z \\sum_{i} \\frac{1}{E(z_i)}$$

For standard flat $\\Lambda$CDM ($H_0 = 70$, $\\Omega_m = 0.3$, $\\Omega_\\Lambda = 0.7$):
- At $z = 0.1$: $\\chi \\approx 418.5$ Mpc
- At $z = 1.0$: $\\chi \\approx 3303.8$ Mpc

### Your Task

Implement the following functions. All constants must be defined **inside** each function body.

- \`E_z(z, Omega_m, Omega_Lambda)\` — returns $E(z) = \\sqrt{\\Omega_m(1+z)^3 + \\Omega_\\Lambda}$
- \`comoving_distance_Mpc(z, H0_km_s_Mpc, Omega_m, Omega_Lambda)\` — returns $\\chi(z)$ in Mpc using trapezoidal integration with $N = 1000$ steps and $c = 299792.458$ km/s`,

	starterCode: `import math

def E_z(z, Omega_m, Omega_Lambda):
    # sqrt(Omega_m*(1+z)^3 + Omega_Lambda)
    pass

def comoving_distance_Mpc(z, H0_km_s_Mpc, Omega_m, Omega_Lambda):
    c = 299792.458  # km/s
    N = 1000
    # DH = c / H0_km_s_Mpc
    # dz = z / N
    # Trapezoidal integration of 1/E_z from 0 to z
    pass

print(round(comoving_distance_Mpc(0.1, 70, 0.3, 0.7), 1))
print(round(comoving_distance_Mpc(1.0, 70, 0.3, 0.7), 1))
`,

	solution: `import math

def E_z(z, Omega_m, Omega_Lambda):
    return math.sqrt(Omega_m*(1+z)**3 + Omega_Lambda)

def comoving_distance_Mpc(z, H0_km_s_Mpc, Omega_m, Omega_Lambda):
    c = 299792.458  # km/s
    N = 1000
    DH = c / H0_km_s_Mpc
    dz = z / N
    total = 0.5 * (1/E_z(0, Omega_m, Omega_Lambda) + 1/E_z(z, Omega_m, Omega_Lambda))
    for i in range(1, N):
        zi = i * dz
        total += 1/E_z(zi, Omega_m, Omega_Lambda)
    total *= dz
    return DH * total

print(round(comoving_distance_Mpc(0.1, 70, 0.3, 0.7), 1))
print(round(comoving_distance_Mpc(1.0, 70, 0.3, 0.7), 1))
`,

	tests: [
		{
			name: "E_z(0, 0.3, 0.7) = 1.0 (E=1 today in flat ΛCDM)",
			code: `{{FUNC}}
print(round(E_z(0, 0.3, 0.7), 4))`,
			expected: "1.0\n",
		},
		{
			name: "comoving_distance_Mpc(0.1, 70, 0.3, 0.7) ≈ 418.5 Mpc",
			code: `{{FUNC}}
print(round(comoving_distance_Mpc(0.1, 70, 0.3, 0.7), 1))`,
			expected: "418.5\n",
		},
		{
			name: "comoving_distance_Mpc(1.0, 70, 0.3, 0.7) ≈ 3303.8 Mpc",
			code: `{{FUNC}}
print(round(comoving_distance_Mpc(1.0, 70, 0.3, 0.7), 1))`,
			expected: "3303.8\n",
		},
		{
			name: "E_z(1, 0.3, 0.7) ≈ 1.7607 (Hubble parameter ratio at z=1)",
			code: `{{FUNC}}
print(round(E_z(1, 0.3, 0.7), 4))`,
			expected: "1.7607\n",
		},
	],
};
