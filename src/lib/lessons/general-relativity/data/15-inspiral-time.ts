import type { Lesson } from "../../types";

export const inspiralTime: Lesson = {
	id: "inspiral-time",
	title: "Binary Inspiral Time",
	chapterId: "gravitational-waves",
	content: `## Binary Inspiral Time

How long does it take two compact objects to spiral together from an initial separation $r_0$? Integrating the orbital decay rate $|dr/dt| = (64/5) G^3 m_1 m_2 (m_1+m_2)/(c^5 r^3)$ from $r_0$ to $0$ gives the **Peters formula** for a circular orbit:

$$T_{\\rm merge} = \\frac{5\\,c^5}{256\\,G^3} \\cdot \\frac{r_0^4}{m_1 m_2 (m_1+m_2)}$$

This $r_0^4$ dependence is dramatic: halving the initial separation cuts the merger time by 16.

### Separation as a Function of Time

Running the integral in reverse, the separation remaining at time $t$ before merger is:

$$r(t) = \\left(r_0^4 - \\frac{256}{5} \\frac{G^3 m_1 m_2 (m_1+m_2)}{c^5}\\,t\\right)^{1/4}$$

At $t=0$ this gives $r_0$; at $t = T_{\\rm merge}$ the bracket vanishes and $r = 0$.

### Physical Timescales

| System | $r_0$ | $T_{\\rm merge}$ |
|--------|--------|-----------------|
| Earth–Sun | $1.496 \\times 10^{11}$ m | $\\sim 10^{23}$ years |
| NS binary at 0.01 AU | $1.496 \\times 10^{9}$ m | $\\sim 5.8 \\times 10^{8}$ years |
| 30+30 $M_\\odot$ BH at $10^8$ m | $10^8$ m | $\\sim 1.2$ years |

### Constants (define inside each function)
- $G = 6.674 \\times 10^{-11}$ m³ kg⁻¹ s⁻²
- $c = 299792458$ m/s

### Your Task

Implement the three functions below.`,

	starterCode: `import math

def inspiral_time(m1, m2, r0):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return 5*c^5/(256*G^3) * r0^4 / (m1*m2*(m1+m2))
    pass

def inspiral_time_years(m1, m2, r0):
    G = 6.674e-11
    c = 299792458.0
    # TODO: compute inspiral_time inline, then divide by seconds per year
    # 1 year = 365.25 * 24 * 3600 seconds
    pass

def separation_at_time(m1, m2, r0, t):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return (r0^4 - (256/5)*G^3*m1*m2*(m1+m2)/c^5 * t)^(1/4)
    pass

M_sun = 1.989e30
m_earth = 5.972e24
m_sun = 1.989e30
print(f"{inspiral_time_years(m_earth, m_sun, 1.496e11):.4e}")
`,

	solution: `import math

def inspiral_time(m1, m2, r0):
    G = 6.674e-11
    c = 299792458.0
    return 5*c**5 * r0**4 / (256 * G**3 * m1*m2*(m1+m2))

def inspiral_time_years(m1, m2, r0):
    G = 6.674e-11
    c = 299792458.0
    t = 5*c**5 * r0**4 / (256 * G**3 * m1*m2*(m1+m2))
    return t / (365.25 * 24 * 3600)

def separation_at_time(m1, m2, r0, t):
    G = 6.674e-11
    c = 299792458.0
    return (r0**4 - (256/5) * G**3*m1*m2*(m1+m2)/c**5 * t)**(1/4)

M_sun = 1.989e30
m_earth = 5.972e24
m_sun = 1.989e30
print(f"{inspiral_time_years(m_earth, m_sun, 1.496e11):.4e}")
`,

	tests: [
		{
			name: "inspiral_time_years for Earth-Sun: astronomically long (~1e23 years)",
			code: `{{FUNC}}
m_earth = 5.972e24
m_sun = 1.989e30
print(f"{inspiral_time_years(m_earth, m_sun, 1.496e11):.4e}")`,
			expected: "1.0688e+23\n",
		},
		{
			name: "inspiral_time in seconds for NS binary at 0.01 AU",
			code: `{{FUNC}}
M_sun = 1.989e30
m_ns = 1.4*M_sun
r0 = 0.01 * 1.496e11
print(f"{inspiral_time(m_ns, m_ns, r0):.4e}")`,
			expected: "1.8454e+16\n",
		},
		{
			name: "inspiral_time_years for 30+30 solar-mass BH binary at 1e8 m",
			code: `{{FUNC}}
M_sun = 1.989e30
m_bh = 30*M_sun
print(f"{inspiral_time_years(m_bh, m_bh, 1e8):.4e}")`,
			expected: "1.1865e+00\n",
		},
		{
			name: "separation_at_time at T/2 equals r0 / 2^(1/4)",
			code: `{{FUNC}}
M_sun = 1.989e30
m_bh = 30*M_sun
r0 = 1e8
T = inspiral_time(m_bh, m_bh, r0)
print(f"{separation_at_time(m_bh, m_bh, r0, T/2):.4e}")`,
			expected: "8.4090e+07\n",
		},
	],
};
