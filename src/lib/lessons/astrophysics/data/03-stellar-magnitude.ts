import type { Lesson } from "../../types";

export const stellarMagnitude: Lesson = {
	id: "stellar-magnitude",
	title: "Stellar Magnitude & Distance",
	chapterId: "stellar-physics",
	content: `## Stellar Magnitude & Distance

Astronomers measure stellar brightness on a **logarithmic magnitude scale** dating back to ancient Greece. The key rule: a difference of **5 magnitudes = a factor of 100 in flux**.

### Flux Ratio

For two stars with apparent magnitudes $m_1$ and $m_2$:

$$\\frac{F_1}{F_2} = 10^{(m_2 - m_1)/2.5}$$

Note: *smaller* magnitude = *brighter* star (Sirius: $m = -1.46$, faintest naked-eye stars: $m \\approx 6$).

### Distance Modulus

The **absolute magnitude** $M$ is the apparent magnitude a star would have at exactly 10 parsecs. The **distance modulus** $\\mu$ connects apparent and absolute magnitude to distance $d$ in parsecs:

$$\\mu = m - M = 5 \\log_{10}\\left(\\frac{d}{10\\text{ pc}}\\right)$$

At $d = 10$ pc: $\\mu = 0$. At $d = 100$ pc: $\\mu = 5$. At $d = 1000$ pc: $\\mu = 10$.

### Parallax

Nearby stars show an annual **parallax** — a tiny apparent shift as Earth orbits the Sun. The distance in parsecs is simply:

$$d_{\\text{pc}} = \\frac{1}{p_{\\text{arcsec}}}$$

The Hipparcos and Gaia missions measured parallaxes for over a billion stars.

| Star | $m$ | $M$ | $d$ (pc) |
|------|-----|-----|----------|
| Sun  | −26.74 | +4.83 | 0.0000049 |
| Sirius | −1.46 | +1.43 | 2.64 |
| Polaris | +1.98 | −3.64 | 133 |

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`flux_ratio(m1, m2)\` — returns $F_1/F_2 = 10^{(m_2-m_1)/2.5}$
- \`distance_modulus(d_pc)\` — returns $\\mu = 5 \\log_{10}(d/10)$
- \`distance_from_modulus(mu)\` — returns $d = 10 \\times 10^{\\mu/5}$ in parsecs`,

	starterCode: `import math

def flux_ratio(m1, m2):
    # TODO: return 10^((m2 - m1) / 2.5)
    pass

def distance_modulus(d_pc):
    # TODO: return 5 * log10(d_pc / 10)
    pass

def distance_from_modulus(mu):
    # TODO: return 10 * 10^(mu / 5)
    pass

print(round(flux_ratio(0, 5), 4))
print(round(distance_modulus(10), 4))
print(round(distance_modulus(100), 4))
print(round(distance_from_modulus(5.0), 4))
`,

	solution: `import math

def flux_ratio(m1, m2):
    return 10**((m2 - m1) / 2.5)

def distance_modulus(d_pc):
    return 5 * math.log10(d_pc / 10)

def distance_from_modulus(mu):
    return 10 * 10**(mu / 5)

print(round(flux_ratio(0, 5), 4))
print(round(distance_modulus(10), 4))
print(round(distance_modulus(100), 4))
print(round(distance_from_modulus(5.0), 4))
`,

	tests: [
		{
			name: "flux_ratio(0, 5) = 100.0 (5 mag difference = factor of 100 in flux)",
			code: `{{FUNC}}
print(round(flux_ratio(0, 5), 4))`,
			expected: "100.0\n",
		},
		{
			name: "distance_modulus(10) = 0.0 (absolute magnitude defined at 10 pc)",
			code: `{{FUNC}}
print(round(distance_modulus(10), 4))`,
			expected: "0.0\n",
		},
		{
			name: "distance_modulus(100) = 5.0 (100 pc is 5 mag distance modulus)",
			code: `{{FUNC}}
print(round(distance_modulus(100), 4))`,
			expected: "5.0\n",
		},
		{
			name: "distance_from_modulus(5.0) = 100.0 pc (inverse of distance modulus)",
			code: `{{FUNC}}
print(round(distance_from_modulus(5.0), 4))`,
			expected: "100.0\n",
		},
	],
};
