import type { Lesson } from "../../types";

export const friedmannEquation: Lesson = {
	id: "friedmann-equation",
	title: "The Friedmann Equation",
	chapterId: "expansion",
	content: `## The Friedmann Equation

The **Friedmann equation** is the master equation governing how the universe expands. Derived from Einstein's field equations applied to a homogeneous, isotropic universe, it relates the Hubble parameter $H(a)$ to the energy content of the cosmos.

### The Full Equation

$$H^2(a) = H_0^2 \\left[ \\frac{\\Omega_m}{a^3} + \\frac{\\Omega_r}{a^4} + \\Omega_\\Lambda + \\frac{\\Omega_k}{a^2} \\right]$$

where $a$ is the **scale factor** (normalised so $a_0 = 1$ today), and:

- $\\Omega_m$ — matter density parameter (dark + baryonic)
- $\\Omega_r$ — radiation density parameter
- $\\Omega_\\Lambda$ — dark energy (cosmological constant)
- $\\Omega_k = 1 - \\Omega_m - \\Omega_r - \\Omega_\\Lambda$ — curvature term

For the observed flat universe ($\\Omega_k = 0$) with negligible radiation today, the standard $\\Lambda$CDM values are $\\Omega_m \\approx 0.3$, $\\Omega_\\Lambda \\approx 0.7$.

### Dimensionless Hubble Parameter

It is convenient to define $E(a) = H(a)/H_0$:

$$E(a) = \\sqrt{\\frac{\\Omega_m}{a^3} + \\frac{\\Omega_r}{a^4} + \\Omega_\\Lambda}$$

At $a = 1$ (today), $E(1) = 1$ when $\\Omega_m + \\Omega_\\Lambda = 1$ (flat).

### Deceleration Parameter

The **deceleration parameter** $q_0$ measures whether the expansion is speeding up or slowing down today:

$$q_0 = \\frac{\\Omega_m}{2} + \\Omega_r - \\Omega_\\Lambda$$

For $\\Omega_m = 0.3$, $\\Omega_\\Lambda = 0.7$: $q_0 = -0.55 < 0$ — the universe is **accelerating**.

### Your Task

Implement the following functions. All constants must be defined **inside** each function body.

- \`H_over_H0(Omega_m, Omega_r, Omega_Lambda, a)\` — returns $E(a) = H(a)/H_0$ for flat universe
- \`H_at_z(H0_km_s_Mpc, Omega_m, Omega_r, Omega_Lambda, z)\` — returns $H(z)$ in km/s/Mpc
- \`deceleration_parameter(Omega_m, Omega_r, Omega_Lambda)\` — returns $q_0$`,

	starterCode: `import math

def H_over_H0(Omega_m, Omega_r, Omega_Lambda, a):
    # sqrt(Omega_m/a^3 + Omega_r/a^4 + Omega_Lambda)
    pass

def H_at_z(H0_km_s_Mpc, Omega_m, Omega_r, Omega_Lambda, z):
    # a = 1/(1+z), then H0 * H_over_H0(...)
    pass

def deceleration_parameter(Omega_m, Omega_r, Omega_Lambda):
    # q0 = Omega_m/2 + Omega_r - Omega_Lambda
    pass

print(round(H_over_H0(0.3, 0.0, 0.7, 1.0), 4))
print(round(H_at_z(70, 0.3, 0.0, 0.7, 1.0), 4))
print(round(deceleration_parameter(0.3, 0.0, 0.7), 4))
`,

	solution: `import math

def H_over_H0(Omega_m, Omega_r, Omega_Lambda, a):
    return math.sqrt(Omega_m/a**3 + Omega_r/a**4 + Omega_Lambda)

def H_at_z(H0_km_s_Mpc, Omega_m, Omega_r, Omega_Lambda, z):
    a = 1 / (1 + z)
    return H0_km_s_Mpc * H_over_H0(Omega_m, Omega_r, Omega_Lambda, a)

def deceleration_parameter(Omega_m, Omega_r, Omega_Lambda):
    return Omega_m/2 + Omega_r - Omega_Lambda

print(round(H_over_H0(0.3, 0.0, 0.7, 1.0), 4))
print(round(H_at_z(70, 0.3, 0.0, 0.7, 1.0), 4))
print(round(deceleration_parameter(0.3, 0.0, 0.7), 4))
`,

	tests: [
		{
			name: "H_over_H0(0.3, 0.0, 0.7, 1.0) = 1.0 (flat ΛCDM at a=1)",
			code: `{{FUNC}}
print(round(H_over_H0(0.3, 0.0, 0.7, 1.0), 4))`,
			expected: "1.0\n",
		},
		{
			name: "H_at_z(70, 0.3, 0.0, 0.7, 1.0) ≈ 123.2477 km/s/Mpc at z=1",
			code: `{{FUNC}}
print(round(H_at_z(70, 0.3, 0.0, 0.7, 1.0), 4))`,
			expected: "123.2477\n",
		},
		{
			name: "deceleration_parameter(0.3, 0.0, 0.7) = -0.55 (accelerating universe)",
			code: `{{FUNC}}
print(round(deceleration_parameter(0.3, 0.0, 0.7), 4))`,
			expected: "-0.55\n",
		},
		{
			name: "H_over_H0(1.0, 0.0, 0.0, 0.5) ≈ 2.8284 (matter-only at a=0.5)",
			code: `{{FUNC}}
print(round(H_over_H0(1.0, 0.0, 0.0, 0.5), 4))`,
			expected: "2.8284\n",
		},
	],
};
