import type { Lesson } from "../../types";

export const gwFrequency: Lesson = {
	id: "gw-frequency",
	title: "Gravitational Wave Frequency",
	chapterId: "gravitational-waves",
	content: `## Gravitational Wave Frequency

Gravitational waves from a compact binary are emitted at **twice the orbital frequency**:

$$f_{\\rm GW} = 2 f_{\\rm orb}$$

The orbital frequency follows from **Kepler's third law** for a circular orbit of separation $r$:

$$f_{\\rm orb} = \\frac{1}{2\\pi} \\sqrt{\\frac{G M_{\\rm tot}}{r^3}}$$

Combining these:

$$f_{\\rm GW} = \\frac{1}{\\pi} \\sqrt{\\frac{G(m_1+m_2)}{r^3}}$$

### ISCO: Peak Frequency at Merger

The inspiral ends at the **Innermost Stable Circular Orbit (ISCO)**, located at $r_{\\rm ISCO} = 6GM_{\\rm tot}/c^2$ for a Schwarzschild (non-spinning) black hole. The GW frequency at ISCO is the highest frequency emitted before merger:

$$f_{\\rm ISCO} = \\frac{c^3}{6^{3/2}\\,\\pi\\,G\\,M_{\\rm tot}}$$

For two 30 $M_\\odot$ black holes (like GW150914), $f_{\\rm ISCO} \\approx 73$ Hz — squarely in LIGO's sensitive band.

### Inverting for Orbital Separation

Given $f_{\\rm GW}$, the corresponding orbital separation is:

$$r = \\left(\\frac{G(m_1+m_2)}{(\\pi f_{\\rm GW})^2}\\right)^{1/3}$$

### Constants (define inside each function)
- $G = 6.674 \\times 10^{-11}$ m³ kg⁻¹ s⁻²
- $c = 299792458$ m/s

### Your Task

Implement the three functions below. Use $M_\\odot = 1.989 \\times 10^{30}$ kg for tests.`,

	starterCode: `import math

def gw_frequency(m1, m2, r):
    G = 6.674e-11
    # TODO: return (1/pi) * sqrt(G*(m1+m2)/r^3)
    pass

def isco_frequency(m1, m2):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return c^3 / (6^(3/2) * pi * G * (m1+m2))
    pass

def orbital_separation(m1, m2, f_gw):
    G = 6.674e-11
    # TODO: return (G*(m1+m2) / (pi*f_gw)^2)^(1/3)
    pass

M_sun = 1.989e30
G = 6.674e-11
c = 299792458.0
r_isco = 6*G*(30*M_sun+30*M_sun)/c**2
print(round(isco_frequency(30*M_sun, 30*M_sun), 4))
print(round(gw_frequency(30*M_sun, 30*M_sun, r_isco), 4))
`,

	solution: `import math

def gw_frequency(m1, m2, r):
    G = 6.674e-11
    return (1/math.pi) * math.sqrt(G*(m1+m2)/r**3)

def isco_frequency(m1, m2):
    G = 6.674e-11
    c = 299792458.0
    return c**3 / (6**(3/2) * math.pi * G * (m1+m2))

def orbital_separation(m1, m2, f_gw):
    G = 6.674e-11
    return (G*(m1+m2) / (math.pi*f_gw)**2)**(1/3)

M_sun = 1.989e30
G = 6.674e-11
c = 299792458.0
r_isco = 6*G*(30*M_sun+30*M_sun)/c**2
print(round(isco_frequency(30*M_sun, 30*M_sun), 4))
print(round(gw_frequency(30*M_sun, 30*M_sun, r_isco), 4))
`,

	tests: [
		{
			name: "isco_frequency for two 30 solar-mass BHs ≈ 73.27 Hz",
			code: `{{FUNC}}
M_sun = 1.989e30
print(round(isco_frequency(30*M_sun, 30*M_sun), 4))`,
			expected: "73.2678\n",
		},
		{
			name: "gw_frequency at ISCO separation equals isco_frequency",
			code: `{{FUNC}}
M_sun = 1.989e30
G = 6.674e-11
c = 299792458.0
r_isco = 6*G*(30*M_sun+30*M_sun)/c**2
print(round(gw_frequency(30*M_sun, 30*M_sun, r_isco), 4))`,
			expected: "73.2678\n",
		},
		{
			name: "orbital_separation at 100 Hz for 30+30 solar-mass binary",
			code: `{{FUNC}}
M_sun = 1.989e30
print(f"{orbital_separation(30*M_sun, 30*M_sun, 100):.4e}")`,
			expected: "4.3214e+05\n",
		},
		{
			name: "isco_frequency for two 1.4 solar-mass neutron stars ≈ 1570 Hz",
			code: `{{FUNC}}
M_sun = 1.989e30
print(round(isco_frequency(1.4*M_sun, 1.4*M_sun), 2))`,
			expected: "1570.02\n",
		},
	],
};
