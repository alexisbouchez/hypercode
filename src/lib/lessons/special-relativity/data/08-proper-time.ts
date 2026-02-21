import type { Lesson } from "../../types";

export const properTime: Lesson = {
	id: "proper-time",
	title: "Proper Time and the Twin Paradox",
	chapterId: "spacetime",
	content: `## Proper Time

Every clock measures its own **proper time** $\\tau$ — the time elapsed along its own worldline. For a clock moving at constant velocity $v$ for coordinate time $\\Delta t$:

$$\\Delta\\tau = \\frac{\\Delta t}{\\gamma} = \\Delta t\\sqrt{1 - \\frac{v^2}{c^2}}$$

Because $\\gamma \\geq 1$, proper time is always less than or equal to coordinate time. Moving clocks run slow — **time dilation**.

### The Twin Paradox

Alice stays on Earth; Bob travels at $v = 0.8c$ for $T/2$ then returns. Earth's coordinate time for the round trip is $T$. Bob's proper time is:

$$\\tau_\\text{Bob} = \\frac{T}{\\gamma}$$

For $v = 0.8c$, $\\gamma = 5/3 \\approx 1.667$, so Bob ages only $0.6 T$ — he returns younger than Alice.

There is no paradox: Bob's worldline is not straight (he decelerates and turns around), so the situation is not symmetric. Alice follows the straight worldline between the two events — which is the worldline of maximum proper time (the spacetime geodesic).

| $v/c$ | $\\gamma$ | Bob ages (T=20 yr) |
|--------|---------|-----------------|
| 0.6 | 1.25 | 16 yr |
| 0.8 | 1.667 | 12 yr |
| 0.99 | ≈7.09 | ≈2.82 yr |

### Your Task

Implement \`proper_time(t, v)\` returning the proper time for coordinate time $t$ at constant speed $v$, and \`twin_age_difference(T, v)\` returning how much less the traveling twin ages compared to the stay-at-home twin. Use $c = 299792458.0$ m/s defined **inside** each function.`,

	starterCode: `import math

def proper_time(t, v):
    c = 299792458.0
    # tau = t / gamma = t * sqrt(1 - (v/c)^2)
    pass

def twin_age_difference(T, v):
    """T: coordinate time for the round trip.
    Returns how much less the traveling twin ages."""
    c = 299792458.0
    pass

print(round(proper_time(10.0, 0), 1))
print(round(proper_time(10.0, 0.6 * 299792458.0), 4))
print(round(proper_time(10.0, 0.8 * 299792458.0), 4))
print(round(twin_age_difference(20.0, 0.8 * 299792458.0), 4))
`,

	solution: `import math

def proper_time(t, v):
    c = 299792458.0
    gamma = 1.0 / math.sqrt(1 - (v / c) ** 2)
    return t / gamma

def twin_age_difference(T, v):
    """T: coordinate time for the round trip.
    Returns how much less the traveling twin ages."""
    c = 299792458.0
    gamma = 1.0 / math.sqrt(1 - (v / c) ** 2)
    stay_home_age = T
    traveler_age = T / gamma
    return stay_home_age - traveler_age

print(round(proper_time(10.0, 0), 1))
print(round(proper_time(10.0, 0.6 * 299792458.0), 4))
print(round(proper_time(10.0, 0.8 * 299792458.0), 4))
print(round(twin_age_difference(20.0, 0.8 * 299792458.0), 4))
`,

	tests: [
		{
			name: "v=0: proper_time(10.0, 0) = 10.0 (no dilation at rest)",
			code: `{{FUNC}}
print(round(proper_time(10.0, 0), 1))`,
			expected: "10.0\n",
		},
		{
			name: "v=0.6c: proper_time(10.0, 0.6c) = 8.0 s (γ=1.25)",
			code: `{{FUNC}}
print(round(proper_time(10.0, 0.6 * 299792458.0), 4))`,
			expected: "8.0\n",
		},
		{
			name: "v=0.8c: proper_time(10.0, 0.8c) = 6.0 s (γ=5/3)",
			code: `{{FUNC}}
print(round(proper_time(10.0, 0.8 * 299792458.0), 4))`,
			expected: "6.0\n",
		},
		{
			name: "Twin paradox: T=20 yr, v=0.8c → age difference = 8.0 yr",
			code: `{{FUNC}}
print(round(twin_age_difference(20.0, 0.8 * 299792458.0), 4))`,
			expected: "8.0\n",
		},
	],
};
