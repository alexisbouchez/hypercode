import type { Lesson } from "../../types";

export const gravitationalTimeDilation: Lesson = {
	id: "gravitational-time-dilation",
	title: "Gravitational Time Dilation",
	chapterId: "schwarzschild",
	content: `## Gravitational Time Dilation

One of the most striking predictions of General Relativity is that **gravity slows time**. A clock deep in a gravitational well ticks more slowly than a clock far away. This is not a measurement artifact — it is a real physical effect confirmed to extraordinary precision by atomic clocks, GPS satellites, and the Pound–Rebka experiment.

### The Formula

For a clock at radius $r$ from a mass $M$ (outside the event horizon, $r > r_s$), compared to a clock at infinity, the **time dilation factor** is:

$$\\frac{d\\tau}{dt_\\infty} = \\sqrt{1 - \\frac{r_s}{r}} = \\sqrt{1 - \\frac{2GM}{c^2 r}}$$

Equivalently, one local second ($d\\tau = 1$) corresponds to a longer interval at infinity:

$$dt_\\infty = \\frac{d\\tau}{\\sqrt{1 - r_s/r}} = \\frac{d\\tau}{\\sqrt{1 - 2GM/(c^2 r)}}$$

The factor $1/\\sqrt{1 - r_s/r} \\geq 1$ tells you how much the remote observer's time is stretched relative to the local clock.

### GPS Satellites

GPS satellites orbit at $r \\approx 26{,}560$ km. Because they are higher in Earth's gravitational field, their clocks run **faster** than clocks on the surface by about $45\\,\\mu$s per day due to gravitational dilation (partially offset by the special-relativistic slowdown from orbital speed). Without GR corrections, GPS would drift by kilometres per day.

### Your Task

Implement these functions with all constants defined **inside** each function:

- \`time_dilation_factor(M, r)\` — returns $1/\\sqrt{1 - 2GM/(c^2 r)}$, the factor $> 1$ by which a remote observer sees the local clock run slow
- \`time_at_infinity(dt_local, M, r)\` — returns the elapsed time at infinity for a local interval $dt_{\\text{local}}$
- \`gravitational_time_shift(dt_local, M, r)\` — returns the extra time at infinity: $dt_\\infty - dt_{\\text{local}}$`,

	starterCode: `import math

def time_dilation_factor(M, r):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return 1.0 / math.sqrt(1 - 2*G*M / (c**2 * r))
    pass

def time_at_infinity(dt_local, M, r):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return dt_local / math.sqrt(1 - 2*G*M / (c**2 * r))
    pass

def gravitational_time_shift(dt_local, M, r):
    G = 6.674e-11
    c = 299792458.0
    # TODO: return dt_local * (1 / math.sqrt(1 - 2*G*M/(c**2 * r)) - 1)
    pass

M_earth = 5.972e24
r_gps = 26560e3
print(round(time_dilation_factor(M_earth, r_gps), 10))
print(round(time_at_infinity(1.0, M_earth, r_gps), 10))
print(round(gravitational_time_shift(1.0, M_earth, r_gps), 12))
print(round(time_dilation_factor(M_earth, 6.371e6), 10))
`,

	solution: `import math

def time_dilation_factor(M, r):
    G = 6.674e-11
    c = 299792458.0
    return 1.0 / math.sqrt(1 - 2 * G * M / (c**2 * r))

def time_at_infinity(dt_local, M, r):
    G = 6.674e-11
    c = 299792458.0
    return dt_local / math.sqrt(1 - 2 * G * M / (c**2 * r))

def gravitational_time_shift(dt_local, M, r):
    G = 6.674e-11
    c = 299792458.0
    return dt_local * (1.0 / math.sqrt(1 - 2 * G * M / (c**2 * r)) - 1)

M_earth = 5.972e24
r_gps = 26560e3
print(round(time_dilation_factor(M_earth, r_gps), 10))
print(round(time_at_infinity(1.0, M_earth, r_gps), 10))
print(round(gravitational_time_shift(1.0, M_earth, r_gps), 12))
print(round(time_dilation_factor(M_earth, 6.371e6), 10))
`,

	tests: [
		{
			name: "time_dilation_factor at GPS orbit ≈ 1.0000000002",
			code: `{{FUNC}}
print(round(time_dilation_factor(5.972e24, 26560e3), 10))`,
			expected: "1.0000000002\n",
		},
		{
			name: "time_at_infinity(1.0 s) at GPS orbit ≈ 1.0000000002 s",
			code: `{{FUNC}}
print(round(time_at_infinity(1.0, 5.972e24, 26560e3), 10))`,
			expected: "1.0000000002\n",
		},
		{
			name: "gravitational_time_shift(1.0 s) at GPS orbit ≈ 1.67e-10 s",
			code: `{{FUNC}}
print(round(gravitational_time_shift(1.0, 5.972e24, 26560e3), 12))`,
			expected: "1.67e-10\n",
		},
		{
			name: "time_dilation_factor at Earth surface ≈ 1.0000000007",
			code: `{{FUNC}}
print(round(time_dilation_factor(5.972e24, 6.371e6), 10))`,
			expected: "1.0000000007\n",
		},
	],
};
