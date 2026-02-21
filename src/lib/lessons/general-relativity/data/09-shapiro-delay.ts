import type { Lesson } from "../../types";

export const shapiroDelay: Lesson = {
	id: "shapiro-delay",
	title: "Shapiro Delay",
	chapterId: "gr-predictions",
	content: `## Shapiro Delay

In 1964, Irwin Shapiro predicted a fourth test of GR: radar signals passing close to a massive body should arrive slightly later than flat-space predictions, because time runs slower in a gravitational field. This is the **Shapiro time delay** (gravitational time delay).

For a signal travelling from distance $r_1$ to $r_2$ past a mass $M$ at closest approach $b$ (where $r_1, r_2 \\gg b$), the excess travel time is approximately:

$$\\Delta t = \\frac{2GM}{c^3} \\ln\\!\\left(\\frac{4 r_1 r_2}{b^2}\\right)$$

### Earth–Mars Radar Test

Shapiro and collaborators verified this prediction in 1968–1971 by bouncing radar pulses off Mars and Venus as they passed behind the Sun. For a signal from Earth to Mars grazing the Sun:

| Parameter | Value |
|-----------|-------|
| $M$ (Sun) | $1.989 \\times 10^{30}$ kg |
| $r_1$ (Earth–Sun) | $1.5 \\times 10^{11}$ m |
| $r_2$ (Mars–Sun) | $2.3 \\times 10^{11}$ m |
| $b$ (solar radius) | $6.96 \\times 10^8$ m |

This gives a one-way delay of about **124 microseconds**, confirmed to better than 0.1% precision.

### Round-Trip and Microseconds

For a radar echo (round trip), the total delay is $2 \\Delta t$. It is convenient to express the delay in **microseconds** ($\\mu$s) since the delays are tiny fractions of a second.

### Your Task

Implement three functions. All physical constants must be defined **inside** each function body.

- \`shapiro_delay(M, r1, r2, b)\` — one-way delay in seconds: $(2GM/c^3) \\ln(4 r_1 r_2 / b^2)$
- \`shapiro_delay_round_trip(M, r1, r2, b)\` — round-trip delay in seconds: $2 \\times \\Delta t$
- \`shapiro_delay_us(M, r1, r2, b)\` — one-way delay in microseconds: $\\Delta t \\times 10^6$`,

	starterCode: `import math

def shapiro_delay(M, r1, r2, b):
    G = 6.674e-11
    c = 299792458.0
    # (2*G*M/c^3) * ln(4*r1*r2/b^2)
    pass

def shapiro_delay_round_trip(M, r1, r2, b):
    G = 6.674e-11
    c = 299792458.0
    # 2 * shapiro_delay
    pass

def shapiro_delay_us(M, r1, r2, b):
    G = 6.674e-11
    c = 299792458.0
    # delay in microseconds: * 1e6
    pass

M_sun = 1.989e30
print(round(shapiro_delay_us(M_sun, 1.5e11, 2.3e11, 6.96e8), 2))
`,

	solution: `import math

def shapiro_delay(M, r1, r2, b):
    G = 6.674e-11
    c = 299792458.0
    return (2 * G * M / c**3) * math.log(4 * r1 * r2 / b**2)

def shapiro_delay_round_trip(M, r1, r2, b):
    G = 6.674e-11
    c = 299792458.0
    return 2 * (2 * G * M / c**3) * math.log(4 * r1 * r2 / b**2)

def shapiro_delay_us(M, r1, r2, b):
    G = 6.674e-11
    c = 299792458.0
    return (2 * G * M / c**3) * math.log(4 * r1 * r2 / b**2) * 1e6

M_sun = 1.989e30
print(round(shapiro_delay_us(M_sun, 1.5e11, 2.3e11, 6.96e8), 2))
`,

	tests: [
		{
			name: "shapiro_delay (Earth to Mars, near Sun) ≈ 0.000124 s",
			code: `{{FUNC}}
print(round(shapiro_delay(1.989e30, 1.5e11, 2.3e11, 6.96e8), 6))`,
			expected: "0.000124\n",
		},
		{
			name: "shapiro_delay_round_trip (Earth-Mars radar echo) ≈ 0.000248 s",
			code: `{{FUNC}}
print(round(shapiro_delay_round_trip(1.989e30, 1.5e11, 2.3e11, 6.96e8), 6))`,
			expected: "0.000248\n",
		},
		{
			name: "shapiro_delay_us (Earth to Mars, near Sun) ≈ 123.76 μs",
			code: `{{FUNC}}
print(round(shapiro_delay_us(1.989e30, 1.5e11, 2.3e11, 6.96e8), 2))`,
			expected: "123.76\n",
		},
		{
			name: "shapiro_delay_us near Jupiter ≈ 0.187826 μs",
			code: `{{FUNC}}
print(round(shapiro_delay_us(1.898e27, 7.78e11, 7.78e11, 7.15e7), 6))`,
			expected: "0.187826\n",
		},
	],
};
