import type { Lesson } from "../../types";

export const gravitationalRedshift: Lesson = {
	id: "gravitational-redshift",
	title: "Gravitational Redshift",
	chapterId: "schwarzschild",
	content: `## Gravitational Redshift

When light climbs out of a gravitational well, it loses energy. Since a photon's energy is $E = hf$, a lower energy means a lower frequency — the light is **redshifted**. This is gravitational redshift, first confirmed by Pound and Rebka in 1959 using the Mössbauer effect over just 22 metres in a Harvard building.

### The Formula

Light emitted at radius $r$ from mass $M$ and received far away (at infinity) has a **redshift parameter** $z$:

$$1 + z = \\frac{\\lambda_{\\text{obs}}}{\\lambda_{\\text{emit}}} = \\frac{1}{\\sqrt{1 - r_s/r}} = \\frac{1}{\\sqrt{1 - 2GM/(c^2 r)}}$$

So:

$$z = \\frac{1}{\\sqrt{1 - 2GM/(c^2 r)}} - 1$$

The observed wavelength and frequency are:

$$\\lambda_{\\text{obs}} = \\frac{\\lambda_{\\text{emit}}}{\\sqrt{1 - r_s/r}}, \\qquad f_{\\text{obs}} = f_{\\text{emit}} \\cdot \\sqrt{1 - r_s/r}$$

### Physical Examples

| Source | $z$ |
|--------|-----|
| Sun (surface) | $\\approx 2.1 \\times 10^{-6}$ |
| White dwarf | $\\sim 10^{-4}$ |
| Neutron star | $\\sim 0.2 – 0.5$ |
| Near event horizon | $\\to \\infty$ |

### Your Task

Implement these functions with all constants defined **inside** each function:

- \`redshift_factor(M, r)\` — returns $z = 1/\\sqrt{1 - 2GM/(c^2 r)} - 1$
- \`wavelength_observed(lambda_emit, M, r)\` — returns the redshifted wavelength
- \`frequency_observed(f_emit, M, r)\` — returns the blueshifted (lowered) frequency`,

	starterCode: `import math

def redshift_factor(M, r):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return 1.0 / math.sqrt(1 - 2*G*M / (c**2 * r)) - 1
    pass

def wavelength_observed(lambda_emit, M, r):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return lambda_emit / math.sqrt(1 - 2*G*M / (c**2 * r))
    pass

def frequency_observed(f_emit, M, r):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return f_emit * math.sqrt(1 - 2*G*M / (c**2 * r))
    pass

M_sun = 1.989e30
r_sun = 6.96e8
print(round(redshift_factor(M_sun, r_sun), 8))
print(round(wavelength_observed(656.3e-9, M_sun, r_sun) * 1e9, 6))
f_emit = 299792458.0 / 656.3e-9
print(round(frequency_observed(f_emit, M_sun, r_sun) / f_emit, 8))
M_ns = 2 * 1.989e30
print(round(redshift_factor(M_ns, 10e3), 6))
`,

	solution: `import math

def redshift_factor(M, r):
    G = 6.674e-11
    c = 299792458.0
    return 1.0 / math.sqrt(1 - 2 * G * M / (c**2 * r)) - 1

def wavelength_observed(lambda_emit, M, r):
    G = 6.674e-11
    c = 299792458.0
    return lambda_emit / math.sqrt(1 - 2 * G * M / (c**2 * r))

def frequency_observed(f_emit, M, r):
    G = 6.674e-11
    c = 299792458.0
    return f_emit * math.sqrt(1 - 2 * G * M / (c**2 * r))

M_sun = 1.989e30
r_sun = 6.96e8
print(round(redshift_factor(M_sun, r_sun), 8))
print(round(wavelength_observed(656.3e-9, M_sun, r_sun) * 1e9, 6))
f_emit = 299792458.0 / 656.3e-9
print(round(frequency_observed(f_emit, M_sun, r_sun) / f_emit, 8))
M_ns = 2 * 1.989e30
print(round(redshift_factor(M_ns, 10e3), 6))
`,

	tests: [
		{
			name: "redshift_factor at solar surface ≈ 2.12e-06",
			code: `{{FUNC}}
print(round(redshift_factor(1.989e30, 6.96e8), 8))`,
			expected: "2.12e-06\n",
		},
		{
			name: "wavelength_observed for H-alpha (656.3 nm) at solar surface ≈ 656.301393 nm",
			code: `{{FUNC}}
print(round(wavelength_observed(656.3e-9, 1.989e30, 6.96e8) * 1e9, 6))`,
			expected: "656.301393\n",
		},
		{
			name: "frequency_observed / f_emit at solar surface ≈ 0.99999788",
			code: `{{FUNC}}
c = 299792458.0
f_emit = c / 656.3e-9
print(round(frequency_observed(f_emit, 1.989e30, 6.96e8) / f_emit, 8))`,
			expected: "0.99999788\n",
		},
		{
			name: "redshift_factor at neutron star surface (2 solar masses, r=10 km) ≈ 0.563261",
			code: `{{FUNC}}
print(round(redshift_factor(2 * 1.989e30, 10e3), 6))`,
			expected: "0.563261\n",
		},
	],
};
