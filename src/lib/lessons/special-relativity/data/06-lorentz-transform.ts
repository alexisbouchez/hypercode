import type { Lesson } from "../../types";

export const lorentzTransform: Lesson = {
	id: "lorentz-transform",
	title: "The Lorentz Transformation",
	chapterId: "spacetime",
	content: `## The Lorentz Transformation

The Lorentz transformation is the relativistic replacement for the Galilean transformation. It tells you how to convert spacetime coordinates $(x, t)$ measured in frame $S$ into coordinates $(x', t')$ measured in frame $S'$, where $S'$ moves at velocity $v$ along the $x$-axis relative to $S$:

$$x' = \\gamma(x - vt), \\quad t' = \\gamma\\!\\left(t - \\frac{vx}{c^2}\\right)$$

The factor $\\gamma = 1/\\sqrt{1-v^2/c^2}$ appears in both equations — space and time are mixed together. This is the mathematical heart of special relativity: there is no absolute time, only spacetime.

### The Inverse Transformation

To go back from $S'$ to $S$, replace $v$ with $-v$ (the primed frame moves at $+v$ relative to $S$, so $S$ moves at $-v$ relative to $S'$):

$$x = \\gamma(x' + vt'), \\quad t = \\gamma\\!\\left(t' + \\frac{vx'}{c^2}\\right)$$

### Key Example (v = 0.6c, γ = 1.25)

Consider an event at $x = 3c$ m, $t = 5$ s. In the moving frame:

$$x' = 1.25 \\times (3c - 0.6c \\times 5) = 1.25 \\times 0 = 0$$
$$t' = 1.25 \\times \\left(5 - \\frac{0.6 \\times 3c}{c^2}\\right) = 1.25 \\times 3.2 = 4.0 \\text{ s}$$

The spatial separation vanishes in the moving frame — both events happen at the same location.

### Your Task

Implement \`lorentz_x(x, t, v)\` returning the transformed position $x'$, and \`lorentz_t(x, t, v)\` returning the transformed time $t'$. Use $c = 299792458.0$ m/s defined **inside** each function.`,

	starterCode: `import math

def lorentz_x(x, t, v):
    c = 299792458.0
    # gamma = 1 / sqrt(1 - (v/c)^2)
    # x' = gamma * (x - v*t)
    pass

def lorentz_t(x, t, v):
    c = 299792458.0
    # gamma = 1 / sqrt(1 - (v/c)^2)
    # t' = gamma * (t - v*x/c^2)
    pass

print(round(lorentz_x(1e9, 5.0, 0), 1))
print(round(lorentz_x(3 * 299792458.0, 5.0, 0.6 * 299792458.0), 4))
print(round(lorentz_t(3 * 299792458.0, 5.0, 0.6 * 299792458.0), 4))
print(round(lorentz_x(0.0, 2.0, 0.6 * 299792458.0) / 299792458.0, 4))
`,

	solution: `import math

def lorentz_x(x, t, v):
    c = 299792458.0
    gamma = 1.0 / math.sqrt(1 - (v / c) ** 2)
    return gamma * (x - v * t)

def lorentz_t(x, t, v):
    c = 299792458.0
    gamma = 1.0 / math.sqrt(1 - (v / c) ** 2)
    return gamma * (t - v * x / c ** 2)

print(round(lorentz_x(1e9, 5.0, 0), 1))
print(round(lorentz_x(3 * 299792458.0, 5.0, 0.6 * 299792458.0), 4))
print(round(lorentz_t(3 * 299792458.0, 5.0, 0.6 * 299792458.0), 4))
print(round(lorentz_x(0.0, 2.0, 0.6 * 299792458.0) / 299792458.0, 4))
`,

	tests: [
		{
			name: "Identity at v=0: lorentz_x(1e9, 5.0, 0) = 1000000000.0",
			code: `{{FUNC}}
print(round(lorentz_x(1e9, 5.0, 0), 1))`,
			expected: "1000000000.0\n",
		},
		{
			name: "v=0.6c, x=3c m, t=5 s → x'=0.0 (same location in moving frame)",
			code: `{{FUNC}}
print(round(lorentz_x(3 * 299792458.0, 5.0, 0.6 * 299792458.0), 4))`,
			expected: "0.0\n",
		},
		{
			name: "v=0.6c, x=3c m, t=5 s → t'=4.0 s",
			code: `{{FUNC}}
print(round(lorentz_t(3 * 299792458.0, 5.0, 0.6 * 299792458.0), 4))`,
			expected: "4.0\n",
		},
		{
			name: "v=0.6c, x=0, t=2 s → x'/c = -1.5 (γ × 0.6c × 2 s / c)",
			code: `{{FUNC}}
print(round(lorentz_x(0.0, 2.0, 0.6 * 299792458.0) / 299792458.0, 4))`,
			expected: "-1.5\n",
		},
	],
};
