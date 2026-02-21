import type { Lesson } from "../../types";

export const spacetimeInterval: Lesson = {
	id: "spacetime-interval",
	title: "The Spacetime Interval",
	chapterId: "spacetime",
	content: `## The Spacetime Interval

The Lorentz transformation mixes space and time — different observers disagree on $\\Delta x$ and $\\Delta t$ separately. But there is a quantity they all agree on: the **spacetime interval**:

$$s^2 = c^2 \\Delta t^2 - \\Delta x^2$$

(Using the $+---$ metric signature.) No matter which inertial frame you compute this in, you get the same number. It is the relativistic generalisation of the distance formula.

### Classification of Intervals

| $s^2$ | Type | Meaning |
|--------|------|---------|
| $s^2 > 0$ | **Timelike** | A signal slower than $c$ can connect the events; they are causally related |
| $s^2 = 0$ | **Lightlike** | Only a light ray connects them; they lie on the light cone |
| $s^2 < 0$ | **Spacelike** | No causal connection is possible; observers disagree on the order |

### Proper Time

For a timelike interval, the **proper time** $\\Delta\\tau$ is the time measured by a clock that travels directly between the two events:

$$\\Delta\\tau = \\frac{\\sqrt{s^2}}{c}$$

### Invariance Example

Event: $\\Delta t = 5$ s, $\\Delta x = 3c$ m. Then $s^2 = 25c^2 - 9c^2 = 16c^2$. After a Lorentz boost to $v = 0.6c$ ($\\gamma = 1.25$): $\\Delta t' = 4$ s, $\\Delta x' = 0$. So $s^2{}'= 16c^2 - 0 = 16c^2$. Invariant confirmed.

### Your Task

Implement \`spacetime_interval_sq(delta_t, delta_x)\` returning $s^2 = c^2 \\Delta t^2 - \\Delta x^2$, and \`interval_type(s_sq)\` returning \`"timelike"\`, \`"lightlike"\`, or \`"spacelike"\`. Use $c = 299792458.0$ m/s defined **inside** each function.`,

	starterCode: `def spacetime_interval_sq(delta_t, delta_x):
    c = 299792458.0
    # s^2 = c^2 * delta_t^2 - delta_x^2
    pass

def interval_type(s_sq):
    # return "timelike", "lightlike", or "spacelike"
    pass

print(spacetime_interval_sq(1.0, 299792458.0))
print(interval_type(spacetime_interval_sq(2.0, 299792458.0)))
print(interval_type(spacetime_interval_sq(1.0, 2 * 299792458.0)))
print(round(spacetime_interval_sq(5.0, 3 * 299792458.0) / 299792458.0 ** 2, 4))
`,

	solution: `def spacetime_interval_sq(delta_t, delta_x):
    c = 299792458.0
    return c ** 2 * delta_t ** 2 - delta_x ** 2

def interval_type(s_sq):
    if s_sq > 0:
        return "timelike"
    elif s_sq == 0:
        return "lightlike"
    else:
        return "spacelike"

print(spacetime_interval_sq(1.0, 299792458.0))
print(interval_type(spacetime_interval_sq(2.0, 299792458.0)))
print(interval_type(spacetime_interval_sq(1.0, 2 * 299792458.0)))
print(round(spacetime_interval_sq(5.0, 3 * 299792458.0) / 299792458.0 ** 2, 4))
`,

	tests: [
		{
			name: "Light signal (delta_x = c * delta_t) → s² = 0.0 (lightlike)",
			code: `{{FUNC}}
print(spacetime_interval_sq(1.0, 299792458.0))`,
			expected: "0.0\n",
		},
		{
			name: "delta_t=2 s, delta_x=c m → timelike (s² = 3c² > 0)",
			code: `{{FUNC}}
print(interval_type(spacetime_interval_sq(2.0, 299792458.0)))`,
			expected: "timelike\n",
		},
		{
			name: "delta_t=1 s, delta_x=2c m → spacelike (s² = -3c² < 0)",
			code: `{{FUNC}}
print(interval_type(spacetime_interval_sq(1.0, 2 * 299792458.0)))`,
			expected: "spacelike\n",
		},
		{
			name: "Invariance: delta_t=5 s, delta_x=3c m → s²/c² = 16.0",
			code: `{{FUNC}}
print(round(spacetime_interval_sq(5.0, 3 * 299792458.0) / 299792458.0 ** 2, 4))`,
			expected: "16.0\n",
		},
	],
};
