import type { Lesson } from "../../types";

export const gwPower: Lesson = {
	id: "gw-power",
	title: "Gravitational Wave Power",
	chapterId: "gravitational-waves",
	content: `## Gravitational Wave Power

Einstein's quadrupole formula gives the power (luminosity) radiated as gravitational waves by a compact binary in a circular orbit of separation $r$:

$$P_{\\rm GW} = \\frac{32}{5} \\frac{G^4}{c^5} \\frac{(m_1 m_2)^2 (m_1+m_2)}{r^5}$$

The steep $r^{-5}$ dependence means power increases enormously as the binary shrinks. At merger, a stellar-mass binary briefly outshines the entire visible universe in gravitational-wave luminosity.

### Orbital Decay Rate

This radiated energy must come from the orbital energy $E_{\\rm orb} = -Gm_1 m_2/(2r)$. Differentiating with respect to $r$ and setting $P_{\\rm GW} = -dE_{\\rm orb}/dt$:

$$\\left|\\frac{dr}{dt}\\right| = \\frac{64}{5} \\frac{G^3}{c^5} \\frac{m_1 m_2 (m_1+m_2)}{r^3}$$

The orbit shrinks slowly at large $r$ and catastrophically fast near merger.

### Earth–Sun as a Sanity Check

For the Earth–Sun system ($m_1 = 5.972 \\times 10^{24}$ kg, $m_2 = 1.989 \\times 10^{30}$ kg, $r = 1.496 \\times 10^{11}$ m), the GW power is only about 200 W — negligible compared to the Sun's $3.8 \\times 10^{26}$ W electromagnetic luminosity. The orbit decays by ~$10^{-20}$ m/s, imperceptible on human timescales.

### Constants (define inside each function)
- $G = 6.674 \\times 10^{-11}$ m³ kg⁻¹ s⁻²
- $c = 299792458$ m/s

### Your Task

Implement the three functions below. Return the **positive magnitude** for power and decay rate (energy is being lost, separation is decreasing). Return the **negative value** for orbital energy (bound orbits have $E < 0$).`,

	starterCode: `import math

def gw_power(m1, m2, r):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return (32/5) * G^4/c^5 * (m1*m2)^2*(m1+m2) / r^5
    pass

def orbital_decay_rate(m1, m2, r):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return (64/5) * G^3/c^5 * m1*m2*(m1+m2) / r^3
    pass

def orbital_energy(m1, m2, r):
    G = 6.674e-11
    # TODO: return -G*m1*m2/(2*r)
    pass

m_earth = 5.972e24
m_sun = 1.989e30
r_earth = 1.496e11
print(f"{gw_power(m_earth, m_sun, r_earth):.4e}")
print(f"{orbital_decay_rate(m_earth, m_sun, r_earth):.4e}")
print(f"{orbital_energy(m_earth, m_sun, r_earth):.4e}")
`,

	solution: `import math

def gw_power(m1, m2, r):
    G = 6.674e-11
    c = 299792458.0
    return (32/5) * G**4/c**5 * (m1*m2)**2*(m1+m2) / r**5

def orbital_decay_rate(m1, m2, r):
    G = 6.674e-11
    c = 299792458.0
    return (64/5) * G**3/c**5 * m1*m2*(m1+m2) / r**3

def orbital_energy(m1, m2, r):
    G = 6.674e-11
    return -G*m1*m2/(2*r)

m_earth = 5.972e24
m_sun = 1.989e30
r_earth = 1.496e11
print(f"{gw_power(m_earth, m_sun, r_earth):.4e}")
print(f"{orbital_decay_rate(m_earth, m_sun, r_earth):.4e}")
print(f"{orbital_energy(m_earth, m_sun, r_earth):.4e}")
`,

	tests: [
		{
			name: "gw_power emitted by Earth-Sun system ≈ 196 W",
			code: `{{FUNC}}
m_earth = 5.972e24
m_sun = 1.989e30
r_earth = 1.496e11
print(f"{gw_power(m_earth, m_sun, r_earth):.4e}")`,
			expected: "1.9639e+02\n",
		},
		{
			name: "orbital_decay_rate of Earth-Sun ≈ 1.1e-20 m/s (negligible)",
			code: `{{FUNC}}
m_earth = 5.972e24
m_sun = 1.989e30
r_earth = 1.496e11
print(f"{orbital_decay_rate(m_earth, m_sun, r_earth):.4e}")`,
			expected: "1.1088e-20\n",
		},
		{
			name: "orbital_energy of Earth-Sun orbit (negative = bound)",
			code: `{{FUNC}}
m_earth = 5.972e24
m_sun = 1.989e30
r_earth = 1.496e11
print(f"{orbital_energy(m_earth, m_sun, r_earth):.4e}")`,
			expected: "-2.6496e+33\n",
		},
		{
			name: "gw_power for neutron star binary at 300,000 km separation",
			code: `{{FUNC}}
M_sun = 1.989e30
m_ns = 1.4*M_sun
print(f"{gw_power(m_ns, m_ns, 3e8):.4e}")`,
			expected: "7.2254e+27\n",
		},
	],
};
