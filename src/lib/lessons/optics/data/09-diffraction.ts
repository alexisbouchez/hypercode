import type { Lesson } from "../../types";

export const diffraction: Lesson = {
  id: "diffraction",
  title: "Diffraction",
  chapterId: "wave",
  content: `# Diffraction

**Diffraction** is the bending and spreading of waves around obstacles or through openings. When the opening size is comparable to the wavelength, the effect becomes dramatic.

## Single-Slit Diffraction

For a slit of width $a$ illuminated by monochromatic light of wavelength $\\lambda$, **dark fringes** (minima) appear at angles satisfying:

$$a \\sin\\theta = m\\lambda \\quad m = \\pm 1, \\pm 2, \\ldots$$

The **first minimum** (smallest angle) occurs at:

$$\\sin\\theta = \\frac{\\lambda}{a}$$

$$\\theta = \\arcsin\\!\\left(\\frac{\\lambda}{a}\\right)$$

The central bright maximum has width $2\\theta$ — it is twice as wide as the other maxima.

**Example:** $\\lambda = 500\\,\\text{nm}$, $a = 100\\,\\mu\\text{m}$:

$$\\theta = \\arcsin\\!\\left(\\frac{500 \\times 10^{-9}}{100 \\times 10^{-6}}\\right) = \\arcsin(0.005) \\approx 0.287°$$

## Diffraction Grating

A diffraction grating has thousands of slits per millimeter. For grating spacing $d$ (distance between adjacent slits), bright maxima occur at:

$$d \\sin\\theta = m\\lambda \\quad m = 0, \\pm 1, \\pm 2, \\ldots$$

Solving for the angle of the $m$-th order maximum:

$$\\theta = \\arcsin\\!\\left(\\frac{m\\lambda}{d}\\right)$$

Diffraction gratings are used in spectrometers to separate light into its component wavelengths with high precision.

**Example:** $\\lambda = 550\\,\\text{nm}$, $d = 2\\,\\mu\\text{m}$ (500 lines/mm), $m = 1$:

$$\\theta = \\arcsin\\!\\left(\\frac{550 \\times 10^{-9}}{2 \\times 10^{-6}}\\right) = \\arcsin(0.275) \\approx 15.96°$$

## Your Task

Implement the two angle formulas. Use \`math.asin\` and \`math.degrees\`. Wavelengths are in nm; slit widths/spacings are in micrometers ($\\mu$m).
`,
  starterCode: `import math

def single_slit_first_min_deg(lam_nm, a_um):
    # Return the angle (degrees) of the first diffraction minimum for a single slit
    # lam_nm: wavelength in nm, a_um: slit width in micrometers
    pass

def grating_angle_deg(lam_nm, d_um, m):
    # Return the angle (degrees) of the m-th order maximum for a diffraction grating
    # lam_nm: wavelength in nm, d_um: grating spacing in micrometers, m: order
    pass
`,
  solution: `import math

def single_slit_first_min_deg(lam_nm, a_um):
    lam_m = lam_nm * 1e-9
    a_m = a_um * 1e-6
    return math.degrees(math.asin(lam_m / a_m))

def grating_angle_deg(lam_nm, d_um, m):
    lam_m = lam_nm * 1e-9
    d_m = d_um * 1e-6
    return math.degrees(math.asin(m * lam_m / d_m))
`,
  tests: [
    {
      name: "single_slit_first_min_deg(500 nm, 100 µm) = 0.2865°",
      code: `{{FUNC}}
print(round(single_slit_first_min_deg(500, 100), 4))`,
      expected: "0.2865\n",
    },
    {
      name: "single_slit_first_min_deg(550 nm, 200 µm) = 0.1576°",
      code: `{{FUNC}}
print(round(single_slit_first_min_deg(550, 200), 4))`,
      expected: "0.1576\n",
    },
    {
      name: "grating_angle_deg(550 nm, 2 µm, m=1) = 15.962°",
      code: `{{FUNC}}
print(round(grating_angle_deg(550, 2, 1), 4))`,
      expected: "15.962\n",
    },
    {
      name: "grating_angle_deg(600 nm, 2 µm, m=2) = 36.8699°",
      code: `{{FUNC}}
print(round(grating_angle_deg(600, 2, 2), 4))`,
      expected: "36.8699\n",
    },
  ],
};
