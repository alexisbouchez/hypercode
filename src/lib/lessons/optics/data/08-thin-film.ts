import type { Lesson } from "../../types";

export const thinFilm: Lesson = {
  id: "thin-film",
  title: "Thin Film Interference",
  chapterId: "wave",
  content: `# Thin Film Interference

Thin film interference explains the rainbow colors in soap bubbles, oil slicks, and anti-reflective coatings. It arises because light reflects from both the top and bottom surfaces of a thin transparent film.

## Phase Shifts on Reflection

A key subtlety: light undergoes a **phase shift of $\\pi$ (half a wavelength)** when it reflects off a medium with a higher refractive index. No phase shift occurs when reflecting off a lower-index medium.

| Reflection | Phase shift |
|-----------|-------------|
| Low-$n$ → High-$n$ | $\\pi$ (half wavelength) |
| High-$n$ → Low-$n$ | None |

## Condition for Constructive Interference

For a film of thickness $t$ and refractive index $n$ surrounded by air ($n_{\\text{air}} = 1$), light reflects from the top surface (with phase shift) and the bottom surface (no phase shift). The **one** net phase shift from reflection means the condition for constructive interference is:

$$2nt = \\left(m - \\frac{1}{2}\\right)\\lambda \\quad m = 1, 2, 3, \\ldots$$

This simplifies for $m = 1$ (minimum thickness):

$$t_{\\min} = \\frac{\\lambda}{4n}$$

## General Thickness for Constructive Interference

For order $m \\geq 1$:

$$t = \\frac{m \\lambda}{2n} - \\frac{\\lambda}{4n} = \\frac{(2m - 1)\\lambda}{4n}$$

However, a cleaner convention used in many textbooks counts the minimum as order $m = 1$ giving:

$$t_{\\min} = \\frac{\\lambda}{4n}$$

And subsequent maxima at:

$$t_m = \\frac{m \\lambda}{2n} \\quad m = 1, 2, 3, \\ldots$$

(where $m=1$ gives the second constructive maximum).

**Example:** For $\\lambda_0 = 550\\,\\text{nm}$ in glass ($n = 1.5$):

$$t_{\\min} = \\frac{550}{4 \\times 1.5} \\approx 91.7\\,\\text{nm}$$

## Your Task

Implement the two formulas. Wavelength inputs are in nm; return thickness in nm.
`,
  starterCode: `def thin_film_min_thickness(lam_nm, n):
    # Return the minimum film thickness (nm) for constructive interference
    # with one phase-inverting boundary (air-film interface)
    pass

def thin_film_thickness(lam_nm, n, m):
    # Return film thickness t = m*lambda/(2n) in nm for order m >= 1
    pass
`,
  solution: `def thin_film_min_thickness(lam_nm, n):
    return lam_nm / (4 * n)

def thin_film_thickness(lam_nm, n, m):
    return m * lam_nm / (2 * n)
`,
  tests: [
    {
      name: "thin_film_min_thickness(550, 1.5) = 91.6667 nm",
      code: `{{FUNC}}
print(round(thin_film_min_thickness(550, 1.5), 4))`,
      expected: "91.6667\n",
    },
    {
      name: "thin_film_min_thickness(600, 1.4) = 107.1429 nm",
      code: `{{FUNC}}
print(round(thin_film_min_thickness(600, 1.4), 4))`,
      expected: "107.1429\n",
    },
    {
      name: "thin_film_thickness(550, 1.5, 2) = 366.6667 nm",
      code: `{{FUNC}}
print(round(thin_film_thickness(550, 1.5, 2), 4))`,
      expected: "366.6667\n",
    },
    {
      name: "thin_film_thickness(600, 1.4, 1) = 214.2857 nm",
      code: `{{FUNC}}
print(round(thin_film_thickness(600, 1.4, 1), 4))`,
      expected: "214.2857\n",
    },
  ],
};
