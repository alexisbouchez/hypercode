import type { Lesson } from "../../types";

export const relativisticDoppler: Lesson = {
	id: "relativistic-doppler",
	title: "Relativistic Doppler Effect",
	chapterId: "kinematics",
	content: `## The Relativistic Doppler Effect

The classical Doppler effect shifts sound frequencies when source and observer move relative to each other. Light has a similar effect, but with a crucial difference: because of time dilation, there is a frequency shift even for **transverse** (sideways) motion.

### Longitudinal Doppler

For a source moving directly **away** from the observer at velocity $v$ (recession), the observed frequency is:

$$f_{\\text{obs}} = f_0\\sqrt{\\frac{1 - \\beta}{1 + \\beta}}$$

For a source moving directly **toward** the observer (approach):

$$f_{\\text{obs}} = f_0\\sqrt{\\frac{1 + \\beta}{1 - \\beta}}$$

where $\\beta = v/c$ and $f_0$ is the emitted frequency.

### Behaviour

| $\\beta$ | Receding factor | Approaching factor |
|---------|----------------|--------------------|
| $0$ | $1$ (no shift) | $1$ (no shift) |
| $0.6$ | $0.5$ (redshifted by half) | $2$ (blueshifted double) |
| $0.8$ | $1/3$ | $3$ |

Notice that at $\\beta = 0.6$: $\\sqrt{0.4/1.6} = \\sqrt{1/4} = 0.5$, so the observed frequency is halved.

### Symmetry Property

For any speed $v$, the product of the receding and approaching factors equals $1$:

$$f_{\\text{recede}} \\times f_{\\text{approach}} = f_0\\sqrt{\\frac{1-\\beta}{1+\\beta}} \\times f_0\\sqrt{\\frac{1+\\beta}{1-\\beta}} = f_0^2$$

This is a useful sanity check.

### Cosmological Redshift

Galaxies receding from us (due to cosmic expansion) have their light redshifted. The redshift parameter $z = (f_0 - f_{\\text{obs}})/f_{\\text{obs}}$ quantifies this. A galaxy at $z = 1$ has its light frequency halved.

### Your Task

Implement:
- \`doppler_receding(f0, v)\` — observed frequency when source moves away
- \`doppler_approaching(f0, v)\` — observed frequency when source moves toward observer

Use $c = 299792458.0$ m/s, defined inside each function.`,

	starterCode: `import math

def doppler_receding(f0, v):
    c = 299792458.0
    beta = v / c
    # TODO: return f0 * sqrt((1 - beta) / (1 + beta))
    pass

def doppler_approaching(f0, v):
    c = 299792458.0
    beta = v / c
    # TODO: return f0 * sqrt((1 + beta) / (1 - beta))
    pass

print(round(doppler_receding(1000.0, 0), 1))
print(round(doppler_receding(1000.0, 0.6 * 299792458.0), 1))
print(round(doppler_approaching(1000.0, 0.6 * 299792458.0), 1))
print(round(doppler_receding(1000.0, 0.8 * 299792458.0) * doppler_approaching(1000.0, 0.8 * 299792458.0), 1))
`,

	solution: `import math

def doppler_receding(f0, v):
    c = 299792458.0
    beta = v / c
    return f0 * math.sqrt((1 - beta) / (1 + beta))

def doppler_approaching(f0, v):
    c = 299792458.0
    beta = v / c
    return f0 * math.sqrt((1 + beta) / (1 - beta))

print(round(doppler_receding(1000.0, 0), 1))
print(round(doppler_receding(1000.0, 0.6 * 299792458.0), 1))
print(round(doppler_approaching(1000.0, 0.6 * 299792458.0), 1))
print(round(doppler_receding(1000.0, 0.8 * 299792458.0) * doppler_approaching(1000.0, 0.8 * 299792458.0), 1))
`,

	tests: [
		{
			name: "no shift at v=0: doppler_receding(1000.0, 0) = 1000.0 Hz",
			code: `{{FUNC}}
print(round(doppler_receding(1000.0, 0), 1))`,
			expected: "1000.0\n",
		},
		{
			name: "0.6c recession halves frequency: doppler_receding(1000.0, 0.6c) = 500.0 Hz",
			code: `{{FUNC}}
print(round(doppler_receding(1000.0, 0.6 * 299792458.0), 1))`,
			expected: "500.0\n",
		},
		{
			name: "0.6c approach doubles frequency: doppler_approaching(1000.0, 0.6c) = 2000.0 Hz",
			code: `{{FUNC}}
print(round(doppler_approaching(1000.0, 0.6 * 299792458.0), 1))`,
			expected: "2000.0\n",
		},
		{
			name: "symmetry: receding × approaching = f0² = 1000000.0 at 0.8c",
			code: `{{FUNC}}
print(round(doppler_receding(1000.0, 0.8 * 299792458.0) * doppler_approaching(1000.0, 0.8 * 299792458.0), 1))`,
			expected: "1000000.0\n",
		},
	],
};
