import type { Lesson } from "../../types";

export const orbitalMechanics: Lesson = {
	id: "orbital-mechanics",
	title: "Orbital Mechanics",
	chapterId: "compact-objects",
	content: `## Orbital Mechanics

Kepler and Newton gave us the laws that govern how objects orbit one another — from moons around planets to stars around black holes.

### Kepler's Third Law

The orbital period $T$ of a body on a circular (or elliptical) orbit with semi-major axis $a$ around a central mass $M$ is:

$$T^2 = \\frac{4\\pi^2 a^3}{GM}$$

Solving for $T$:

$$T = 2\\pi \\sqrt{\\frac{a^3}{GM}}$$

For Earth orbiting the Sun ($a = 1.496 \\times 10^{11}$ m), this gives $T \\approx 3.156 \\times 10^7$ s — one year.

### Circular Orbital Velocity

For a circular orbit, gravity provides exactly the centripetal force needed:

$$v_c = \\sqrt{\\frac{GM}{r}}$$

Earth's orbital velocity is approximately 29.8 km/s.

### Orbital Energy

The total mechanical energy of a bound orbit (kinetic + potential) is:

$$E = -\\frac{GMm}{2a}$$

Negative energy means the orbit is bound. The more negative (smaller $a$), the more tightly bound the orbit.

### Your Task

Implement three functions. Use $G = 6.674 \\times 10^{-11}$ m³ kg⁻¹ s⁻², defined **inside** each function.

- \`orbital_period_s(a_m, M_kg)\` — orbital period in seconds (Kepler's third law)
- \`circular_velocity_m_s(M_kg, r_m)\` — circular orbital speed in m/s
- \`orbital_energy_J(M_kg, m_kg, a_m)\` — total orbital energy in joules`,

	starterCode: `import math

def orbital_period_s(a_m, M_kg):
    G = 6.674e-11
    # TODO: return 2 * pi * sqrt(a^3 / (G * M))
    pass

def circular_velocity_m_s(M_kg, r_m):
    G = 6.674e-11
    # TODO: return sqrt(G * M / r)
    pass

def orbital_energy_J(M_kg, m_kg, a_m):
    G = 6.674e-11
    # TODO: return -G * M * m / (2 * a)
    pass

M_sun = 1.989e30
M_earth = 5.972e24
a_earth = 1.496e11
print(round(orbital_period_s(a_earth, M_sun) / 1e7, 4))
print(round(circular_velocity_m_s(M_sun, a_earth) / 1e3, 2))
`,

	solution: `import math

def orbital_period_s(a_m, M_kg):
    G = 6.674e-11
    return 2 * math.pi * math.sqrt(a_m**3 / (G * M_kg))

def circular_velocity_m_s(M_kg, r_m):
    G = 6.674e-11
    return math.sqrt(G * M_kg / r_m)

def orbital_energy_J(M_kg, m_kg, a_m):
    G = 6.674e-11
    return -G * M_kg * m_kg / (2 * a_m)

M_sun = 1.989e30
M_earth = 5.972e24
a_earth = 1.496e11
print(round(orbital_period_s(a_earth, M_sun) / 1e7, 4))
print(round(circular_velocity_m_s(M_sun, a_earth) / 1e3, 2))
`,

	tests: [
		{
			name: "Earth's orbital period ≈ 3.1555 × 10⁷ s (≈ 1 year)",
			code: `{{FUNC}}
M_sun = 1.989e30
a_earth = 1.496e11
print(round(orbital_period_s(a_earth, M_sun) / 1e7, 4))`,
			expected: "3.1555\n",
		},
		{
			name: "Earth's orbital velocity ≈ 29.79 km/s",
			code: `{{FUNC}}
M_sun = 1.989e30
a_earth = 1.496e11
print(round(circular_velocity_m_s(M_sun, a_earth) / 1e3, 2))`,
			expected: "29.79\n",
		},
		{
			name: "Moon's orbital period ≈ 27.45 days",
			code: `{{FUNC}}
M_earth = 5.972e24
a_moon = 3.844e8
print(round(orbital_period_s(a_moon, M_earth) / (24 * 3600), 2))`,
			expected: "27.45\n",
		},
		{
			name: "Earth's orbital energy ≈ -2.6496 × 10³³ J",
			code: `{{FUNC}}
M_sun = 1.989e30
M_earth = 5.972e24
a_earth = 1.496e11
print(round(orbital_energy_J(M_sun, M_earth, a_earth) / 1e33, 4))`,
			expected: "-2.6496\n",
		},
	],
};
