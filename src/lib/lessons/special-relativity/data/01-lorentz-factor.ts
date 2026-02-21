import type { Lesson } from "../../types";

export const lorentzFactor: Lesson = {
	id: "lorentz-factor",
	title: "The Lorentz Factor",
	chapterId: "kinematics",
	content: `## The Lorentz Factor

Special relativity rests on two postulates: the laws of physics are the same in all inertial frames, and the speed of light $c = 299{,}792{,}458$ m/s is the same for all observers. These innocent-sounding rules force us to abandon absolute time and space.

The quantity that appears in every relativistic formula is the **Lorentz factor** $\\gamma$. It is built from the **beta parameter** $\\beta$, the fraction of the speed of light:

$$\\beta = \\frac{v}{c}$$

$$\\gamma = \\frac{1}{\\sqrt{1 - \\beta^2}}$$

### Behaviour

| $v/c$ | $\\beta$ | $\\gamma$ |
|--------|---------|---------|
| $0$ | $0$ | $1$ |
| $0.6c$ | $0.6$ | $1.25$ |
| $0.8c$ | $0.8$ | $\\approx 1.667$ |
| $0.99c$ | $0.99$ | $\\approx 7.09$ |
| $\\to c$ | $\\to 1$ | $\\to \\infty$ |

At everyday speeds $\\gamma \\approx 1$ and all relativistic effects are negligible. As $v \\to c$, $\\gamma$ diverges, meaning it takes infinite energy to reach light speed. The factor $\\gamma$ will reappear in time dilation, length contraction, and relativistic momentum and energy.

### Your Task

Implement \`lorentz_factor(v)\` that returns $\\gamma$ for a velocity $v$ in m/s, and \`beta(v)\` that returns $\\beta = v/c$. Use $c = 299792458.0$ m/s. Both constants must be defined **inside** each function.`,

	starterCode: `import math

def lorentz_factor(v):
    c = 299792458.0
    # TODO: return 1 / sqrt(1 - (v/c)^2)
    pass

def beta(v):
    c = 299792458.0
    # TODO: return v / c
    pass

print(lorentz_factor(0))
print(round(lorentz_factor(0.6 * 299792458.0), 4))
print(round(lorentz_factor(0.8 * 299792458.0), 4))
print(round(beta(0.5 * 299792458.0), 4))
`,

	solution: `import math

def lorentz_factor(v):
    c = 299792458.0
    return 1.0 / math.sqrt(1 - (v / c) ** 2)

def beta(v):
    c = 299792458.0
    return v / c

print(lorentz_factor(0))
print(round(lorentz_factor(0.6 * 299792458.0), 4))
print(round(lorentz_factor(0.8 * 299792458.0), 4))
print(round(beta(0.5 * 299792458.0), 4))
`,

	tests: [
		{
			name: "lorentz_factor(0) = 1.0 (no time dilation at rest)",
			code: `{{FUNC}}
print(lorentz_factor(0))`,
			expected: "1.0\n",
		},
		{
			name: "lorentz_factor(0.6c) = 1.25",
			code: `{{FUNC}}
print(round(lorentz_factor(0.6 * 299792458.0), 4))`,
			expected: "1.25\n",
		},
		{
			name: "lorentz_factor(0.8c) ≈ 1.6667",
			code: `{{FUNC}}
print(round(lorentz_factor(0.8 * 299792458.0), 4))`,
			expected: "1.6667\n",
		},
		{
			name: "beta(0.5c) = 0.5",
			code: `{{FUNC}}
print(round(beta(0.5 * 299792458.0), 4))`,
			expected: "0.5\n",
		},
	],
};
