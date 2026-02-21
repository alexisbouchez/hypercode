import type { Lesson } from "../../types";

export const lensmaker: Lesson = {
  id: "lensmaker",
  title: "The Lensmaker's Equation",
  chapterId: "geometric",
  content: `# The Lensmaker's Equation

The **lensmaker's equation** connects a lens's focal length to its physical properties: the refractive index of the glass and the radii of curvature of its two surfaces.

## The Equation

$$\\frac{1}{f} = (n - 1)\\left(\\frac{1}{R_1} - \\frac{1}{R_2}\\right)$$

Where:
- $n$ is the refractive index of the lens material
- $R_1$ is the radius of curvature of the first surface (positive if center of curvature is to the right)
- $R_2$ is the radius of curvature of the second surface (positive if center of curvature is to the right)
- $f$ is the focal length

## Sign Convention for Radii

- If a surface is convex toward incoming light: $R > 0$
- If a surface is concave toward incoming light: $R < 0$

For a standard **biconvex** lens: $R_1 > 0$ and $R_2 < 0$.

**Example:** A biconvex glass lens ($n = 1.5$) with $R_1 = 20\\,\\text{cm}$ and $R_2 = -20\\,\\text{cm}$:

$$\\frac{1}{f} = (1.5 - 1)\\left(\\frac{1}{20} - \\frac{1}{-20}\\right) = 0.5 \\times 0.1 = 0.05$$
$$f = 20\\,\\text{cm}$$

## Two-Lens System

When two thin lenses are separated by a distance $d$, the combined focal length is:

$$\\frac{1}{f} = \\frac{1}{f_1} + \\frac{1}{f_2} - \\frac{d}{f_1 f_2}$$

When the lenses are in contact ($d = 0$):

$$\\frac{1}{f} = \\frac{1}{f_1} + \\frac{1}{f_2}$$

## Your Task

Implement the lensmaker's equation and the two-lens combination formula.
`,
  starterCode: `def lensmaker_focal_length(n, R1, R2):
    # Return the focal length using the lensmaker's equation
    # n: refractive index, R1, R2: radii of curvature (same units as returned f)
    pass

def combined_focal_length(f1, f2, d):
    # Return the combined focal length of two lenses separated by distance d
    pass
`,
  solution: `def lensmaker_focal_length(n, R1, R2):
    return 1 / ((n - 1) * (1 / R1 - 1 / R2))

def combined_focal_length(f1, f2, d):
    return 1 / (1 / f1 + 1 / f2 - d / (f1 * f2))
`,
  tests: [
    {
      name: "lensmaker_focal_length(1.5, 20, -20) = 20.0 cm",
      code: `{{FUNC}}
print(round(lensmaker_focal_length(1.5, 20, -20), 4))`,
      expected: "20.0\n",
    },
    {
      name: "lensmaker_focal_length(1.5, 10, -10) = 10.0 cm",
      code: `{{FUNC}}
print(round(lensmaker_focal_length(1.5, 10, -10), 4))`,
      expected: "10.0\n",
    },
    {
      name: "combined_focal_length(20, 25, 10) = 14.2857 cm",
      code: `{{FUNC}}
print(round(combined_focal_length(20, 25, 10), 4))`,
      expected: "14.2857\n",
    },
    {
      name: "combined_focal_length(10, 15, 0) = 6.0 cm (contact lenses)",
      code: `{{FUNC}}
print(round(combined_focal_length(10, 15, 0), 4))`,
      expected: "6.0\n",
    },
  ],
};
