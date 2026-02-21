import type { Lesson } from "../../types";

export const escapeVelocity: Lesson = {
	id: "escape-velocity",
	title: "Escape Velocity and Black Holes",
	chapterId: "compact-objects",
	content: `## Escape Velocity and Black Holes

The **escape velocity** is the minimum speed an object needs to escape a gravitational field entirely — travelling to infinity with zero remaining kinetic energy.

### Escape Velocity Formula

Setting kinetic energy equal to gravitational potential energy:

$$v_{\\rm esc} = \\sqrt{\\frac{2GM}{R}}$$

| Object | $v_{\\rm esc}$ |
|--------|---------------|
| Earth | 11.2 km/s (0.004% of $c$) |
| Sun | 618 km/s (0.2% of $c$) |
| Neutron star ($M = 1.4\\ M_\\odot$, $R = 10$ km) | ≈ 0.64$c$ |
| Black hole at $r_s$ | exactly $c$ |

### The Schwarzschild Radius

A black hole forms when an object is compressed below its **Schwarzschild radius**, where even light cannot escape:

$$r_s = \\frac{2GM}{c^2}$$

For the Sun, $r_s \\approx 2954$ m — about 3 km. For Earth, $r_s \\approx 9$ mm.

### Surface Gravity

The gravitational acceleration at a body's surface:

$$g = \\frac{GM}{R^2}$$

Earth's surface gravity is $g \\approx 9.82$ m/s².

### Your Task

Implement three functions. Use $G = 6.674 \\times 10^{-11}$ m³ kg⁻¹ s⁻² and $c = 2.998 \\times 10^8$ m/s, defined **inside** each function.

- \`escape_velocity_m_s(M_kg, R_m)\` — escape velocity in m/s
- \`schwarzschild_radius_m(M_kg)\` — Schwarzschild radius in metres
- \`surface_gravity_m_s2(M_kg, R_m)\` — surface gravitational acceleration in m/s²`,

	starterCode: `import math

def escape_velocity_m_s(M_kg, R_m):
    G = 6.674e-11
    # TODO: return sqrt(2 * G * M / R)
    pass

def schwarzschild_radius_m(M_kg):
    G = 6.674e-11
    c = 2.998e8  # m/s
    # TODO: return 2 * G * M / c^2
    pass

def surface_gravity_m_s2(M_kg, R_m):
    G = 6.674e-11
    # TODO: return G * M / R^2
    pass

M_earth = 5.972e24
R_earth = 6.371e6
M_sun = 1.989e30
R_sun = 6.957e8
print(round(escape_velocity_m_s(M_earth, R_earth), 2))
print(round(schwarzschild_radius_m(M_sun), 1))
print(round(surface_gravity_m_s2(M_earth, R_earth), 2))
`,

	solution: `import math

def escape_velocity_m_s(M_kg, R_m):
    G = 6.674e-11
    return math.sqrt(2 * G * M_kg / R_m)

def schwarzschild_radius_m(M_kg):
    G = 6.674e-11
    c = 2.998e8  # m/s
    return 2 * G * M_kg / c**2

def surface_gravity_m_s2(M_kg, R_m):
    G = 6.674e-11
    return G * M_kg / R_m**2

M_earth = 5.972e24
R_earth = 6.371e6
M_sun = 1.989e30
R_sun = 6.957e8
print(round(escape_velocity_m_s(M_earth, R_earth), 2))
print(round(schwarzschild_radius_m(M_sun), 1))
print(round(surface_gravity_m_s2(M_earth, R_earth), 2))
`,

	tests: [
		{
			name: "escape_velocity_m_s(Earth) ≈ 11185.73 m/s",
			code: `{{FUNC}}
M_earth = 5.972e24
R_earth = 6.371e6
print(round(escape_velocity_m_s(M_earth, R_earth), 2))`,
			expected: "11185.73\n",
		},
		{
			name: "escape_velocity_m_s(Sun) ≈ 617.75 km/s",
			code: `{{FUNC}}
M_sun = 1.989e30
R_sun = 6.957e8
print(round(escape_velocity_m_s(M_sun, R_sun) / 1e3, 2))`,
			expected: "617.75\n",
		},
		{
			name: "schwarzschild_radius_m(Sun) ≈ 2953.8 m",
			code: `{{FUNC}}
M_sun = 1.989e30
print(round(schwarzschild_radius_m(M_sun), 1))`,
			expected: "2953.8\n",
		},
		{
			name: "surface_gravity_m_s2(Earth) ≈ 9.82 m/s²",
			code: `{{FUNC}}
M_earth = 5.972e24
R_earth = 6.371e6
print(round(surface_gravity_m_s2(M_earth, R_earth), 2))`,
			expected: "9.82\n",
		},
	],
};
