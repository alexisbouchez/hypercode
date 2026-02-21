import type { Lesson } from "../../types";

export const opticalPath: Lesson = {
  id: "optical-path",
  title: "Optical Path Length and Phase",
  chapterId: "modern",
  content: `# Optical Path Length and Phase

The **optical path length (OPL)** is the equivalent distance light would travel in vacuum to accumulate the same phase as it does traveling a physical distance $d$ through a medium of refractive index $n$:

$$\\text{OPL} = n \\cdot d$$

## Why OPL Matters

Inside a medium, light travels slower ($v = c/n$) and its wavelength shortens ($\\lambda_n = \\lambda_0/n$). But the phase accumulated per meter of wavelength is the same as in vacuum. The OPL captures the total phase accumulation in vacuum-equivalent meters.

OPL is the foundation of:
- **Interference calculations** — path difference in OPL determines constructive or destructive interference
- **Optical coherence** — coherence length in OPL units
- **Wavefront engineering** — spatial light modulators and adaptive optics manipulate OPL

## Phase Difference

As light travels through a medium, the accumulated phase is:

$$\\phi = \\frac{2\\pi}{\\lambda_0} \\cdot \\text{OPL} = \\frac{2\\pi n d}{\\lambda_0}$$

Where $\\lambda_0$ is the vacuum wavelength.

When two beams with OPL difference $\\Delta$ interfere:
- Constructive if $\\Delta = m\\lambda_0$ ($m = 0, 1, 2, \\ldots$)
- Destructive if $\\Delta = (m + \\frac{1}{2})\\lambda_0$

**Example:** Light ($\\lambda_0 = 550\\,\\text{nm}$) through glass ($n = 1.5$, $d = 1\\,\\text{cm} = 0.01\\,\\text{m}$):

$$\\text{OPL} = 1.5 \\times 0.01 = 0.015\\,\\text{m}$$

$$\\phi = \\frac{2\\pi \\times 0.015}{550 \\times 10^{-9}} \\approx 171{,}360\\,\\text{rad}$$

That is roughly 27,000 full oscillations — which is why the slightest path length difference matters in interferometry.

## Optical Path Difference

When comparing two paths, the **optical path difference (OPD)** determines the interference outcome:

$$\\text{OPD} = n_1 d_1 - n_2 d_2$$

## Your Task

Implement the OPL and phase difference calculations.
`,
  starterCode: `import math

def optical_path_length(n, d_m):
    # Return the optical path length in meters
    # n: refractive index, d_m: physical distance in meters
    pass

def phase_difference_rad(n, d_m, lam_nm):
    # Return the accumulated phase in radians
    # n: refractive index, d_m: distance in meters, lam_nm: vacuum wavelength in nm
    pass
`,
  solution: `import math

def optical_path_length(n, d_m):
    return n * d_m

def phase_difference_rad(n, d_m, lam_nm):
    lam_m = lam_nm * 1e-9
    return 2 * math.pi * n * d_m / lam_m
`,
  tests: [
    {
      name: "optical_path_length(1.5, 0.01) = 0.015 m",
      code: `{{FUNC}}
print(round(optical_path_length(1.5, 0.01), 4))`,
      expected: "0.015\n",
    },
    {
      name: "optical_path_length(1.0, 0.005) = 0.005 m",
      code: `{{FUNC}}
print(round(optical_path_length(1.0, 0.005), 4))`,
      expected: "0.005\n",
    },
    {
      name: "phase_difference_rad(1.5, 0.01, 550) = 171359.5993 rad",
      code: `{{FUNC}}
print(round(phase_difference_rad(1.5, 0.01, 550), 4))`,
      expected: "171359.5993\n",
    },
    {
      name: "phase_difference_rad(1.0, 0.005, 500) = 62831.8531 rad",
      code: `{{FUNC}}
print(round(phase_difference_rad(1.0, 0.005, 500), 4))`,
      expected: "62831.8531\n",
    },
  ],
};
