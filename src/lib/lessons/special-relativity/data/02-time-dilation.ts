import type { Lesson } from "../../types";

export const timeDilation: Lesson = {
	id: "time-dilation",
	title: "Time Dilation",
	chapterId: "kinematics",
	content: `## Time Dilation

One of the most astonishing predictions of special relativity is that **moving clocks run slow**. This is not a mechanical effect — it is a fundamental property of spacetime itself.

### Proper Time and Coordinate Time

The **proper time** $\\Delta\\tau$ is the time measured by a clock that travels with the moving object (its own rest frame). The **coordinate time** $\\Delta t$ is the time measured by a stationary observer watching the clock move past.

The relationship is:

$$\\Delta t = \\gamma\\,\\Delta\\tau$$

Because $\\gamma \\geq 1$, the coordinate time is always at least as large as the proper time. A moving clock ticks fewer times than a stationary clock between two events: it is running slow as seen from outside.

Inverting the relation gives the proper time from a measured coordinate time:

$$\\Delta\\tau = \\frac{\\Delta t}{\\gamma}$$

### The Muon Experiment

Cosmic rays create **muons** at 15 km altitude moving at $v \\approx 0.998c$ toward the ground. In their own rest frame, a muon lives only about $\\tau = 2.2\\,\\mu\\text{s}$ — classically it should decay after traveling just $2.2\\,\\mu\\text{s} \\times 0.998c \\approx 660\\text{ m}$. Yet muons reach sea level because, from our frame, their internal clock runs slow: $\\gamma \\approx 15$, so the coordinate lifetime is $\\approx 33\\,\\mu\\text{s}$ and they cover the full 15 km.

### Your Task

Implement:
- \`time_dilation(tau, v)\` — returns coordinate time $\\Delta t = \\gamma\\,\\Delta\\tau$
- \`proper_time(t, v)\` — returns proper time $\\Delta\\tau = \\Delta t / \\gamma$

Use $c = 299792458.0$ m/s, defined inside each function.`,

	starterCode: `import math

def time_dilation(tau, v):
    c = 299792458.0
    # TODO: compute gamma and return gamma * tau
    pass

def proper_time(t, v):
    c = 299792458.0
    # TODO: compute gamma and return t / gamma
    pass

print(round(time_dilation(1.0, 0.6 * 299792458.0), 4))
print(round(time_dilation(10.0, 0.8 * 299792458.0), 4))
print(round(proper_time(1.25, 0.6 * 299792458.0), 4))
print(time_dilation(0, 0.9 * 299792458.0))
`,

	solution: `import math

def time_dilation(tau, v):
    c = 299792458.0
    gamma = 1.0 / math.sqrt(1 - (v / c) ** 2)
    return gamma * tau

def proper_time(t, v):
    c = 299792458.0
    gamma = 1.0 / math.sqrt(1 - (v / c) ** 2)
    return t / gamma

print(round(time_dilation(1.0, 0.6 * 299792458.0), 4))
print(round(time_dilation(10.0, 0.8 * 299792458.0), 4))
print(round(proper_time(1.25, 0.6 * 299792458.0), 4))
print(time_dilation(0, 0.9 * 299792458.0))
`,

	tests: [
		{
			name: "time_dilation(1.0, 0.6c) = 1.25 s (γ = 1.25)",
			code: `{{FUNC}}
print(round(time_dilation(1.0, 0.6 * 299792458.0), 4))`,
			expected: "1.25\n",
		},
		{
			name: "time_dilation(10.0, 0.8c) ≈ 16.6667 s (γ = 5/3)",
			code: `{{FUNC}}
print(round(time_dilation(10.0, 0.8 * 299792458.0), 4))`,
			expected: "16.6667\n",
		},
		{
			name: "proper_time(1.25, 0.6c) = 1.0 s (inverse of dilation)",
			code: `{{FUNC}}
print(round(proper_time(1.25, 0.6 * 299792458.0), 4))`,
			expected: "1.0\n",
		},
		{
			name: "time_dilation(0, 0.9c) = 0.0 (zero proper time stays zero)",
			code: `{{FUNC}}
print(time_dilation(0, 0.9 * 299792458.0))`,
			expected: "0.0\n",
		},
	],
};
