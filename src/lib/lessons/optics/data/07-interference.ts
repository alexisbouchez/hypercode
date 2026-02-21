import type { Lesson } from "../../types";

export const interference: Lesson = {
  id: "interference",
  title: "Young's Double-Slit Interference",
  chapterId: "wave",
  content: `# Young's Double-Slit Interference

In 1801, Thomas Young demonstrated that light is a wave by passing it through two narrow slits and observing alternating bright and dark bands — an **interference pattern** — on a distant screen.

## Setup

Two slits separated by distance $d$ are illuminated by monochromatic light of wavelength $\\lambda$. A screen is placed at distance $L$ from the slits.

## Path Difference

The two waves from the slits travel different distances to any point $P$ on the screen. For a point at height $y$ above the center, the path difference is approximately:

$$\\Delta = \\frac{y d}{L}$$

## Bright Fringes (Constructive Interference)

Constructive interference occurs when the path difference equals a whole number of wavelengths:

$$\\Delta = m \\lambda \\quad m = 0, \\pm 1, \\pm 2, \\ldots$$

This gives bright fringe positions:

$$y_m = \\frac{m \\lambda L}{d}$$

## Fringe Spacing

The distance between adjacent bright fringes is constant:

$$\\Delta y = \\frac{\\lambda L}{d}$$

**Example:** $\\lambda = 550\\,\\text{nm}$, $L = 1\\,\\text{m}$, $d = 1\\,\\text{mm}$:

$$\\Delta y = \\frac{550 \\times 10^{-9} \\times 1}{10^{-3}} = 0.00055\\,\\text{m} = 0.55\\,\\text{mm}$$

## Dark Fringes (Destructive Interference)

Destructive interference occurs at half-integer path differences:

$$\\Delta = \\left(m + \\frac{1}{2}\\right) \\lambda$$

## Finding the Fringe Order

Given a measured position $y$, the nearest bright fringe order is:

$$m = \\text{round}\\!\\left(\\frac{y d}{\\lambda L}\\right)$$

## Your Task

Implement the fringe spacing formula and the order-finding function. Wavelengths are in nm, distances in meters.
`,
  starterCode: `def fringe_spacing(lam_nm, L_m, d_m):
    # Return the fringe spacing in meters
    # lam_nm: wavelength in nm, L_m: screen distance in m, d_m: slit separation in m
    pass

def fringe_order(y_m, lam_nm, L_m, d_m):
    # Return the nearest bright fringe order (integer) for a point at y_m meters from center
    pass
`,
  solution: `def fringe_spacing(lam_nm, L_m, d_m):
    lam_m = lam_nm * 1e-9
    return lam_m * L_m / d_m

def fringe_order(y_m, lam_nm, L_m, d_m):
    lam_m = lam_nm * 1e-9
    return round(y_m * d_m / (lam_m * L_m))
`,
  tests: [
    {
      name: "fringe_spacing(550 nm, 1 m, 1 mm) = 0.00055 m",
      code: `{{FUNC}}
print(round(fringe_spacing(550, 1.0, 0.001), 6))`,
      expected: "0.00055\n",
    },
    {
      name: "fringe_spacing(633 nm, 2 m, 0.5 mm) = 0.002532 m",
      code: `{{FUNC}}
print(round(fringe_spacing(633, 2.0, 0.0005), 6))`,
      expected: "0.002532\n",
    },
    {
      name: "fringe_order at y=1.1 mm is order 2",
      code: `{{FUNC}}
print(fringe_order(0.0011, 550, 1.0, 0.001))`,
      expected: "2\n",
    },
    {
      name: "fringe_order at y=1.65 mm is order 3",
      code: `{{FUNC}}
print(fringe_order(0.00165, 550, 1.0, 0.001))`,
      expected: "3\n",
    },
  ],
};
