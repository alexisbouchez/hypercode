import type { Lesson } from "../../types";

export const thinLens: Lesson = {
  id: "thin-lens",
  title: "Thin Lens Equation",
  chapterId: "geometric",
  content: `# Thin Lens Equation

A **thin lens** is one whose thickness is negligible compared to the other distances involved. Lenses work by refracting light at two curved surfaces.

## Types of Lenses

- **Converging (convex) lens**: thicker at the center; $f > 0$. Focuses parallel rays to a real focal point.
- **Diverging (concave) lens**: thinner at the center; $f < 0$. Spreads rays as if from a virtual focal point.

## The Thin Lens Equation

The same form as the mirror equation:

$$\\frac{1}{f} = \\frac{1}{d_o} + \\frac{1}{d_i}$$

Solving for image distance:

$$d_i = \\frac{1}{\\dfrac{1}{f} - \\dfrac{1}{d_o}}$$

### Sign Convention

| Quantity | Positive | Negative |
|----------|----------|----------|
| $f$ | Converging lens | Diverging lens |
| $d_o$ | Object on incoming side | (rare) |
| $d_i$ | Real image (opposite side) | Virtual image (same side as object) |

## Magnification

$$m = -\\frac{d_i}{d_o}$$

## Lens Power

Optometrists measure lens strength in **diopters** (D), which is simply the reciprocal of the focal length in meters:

$$P = \\frac{1}{f_{\\text{meters}}}$$

A +2 D lens has $f = 0.5\\,\\text{m}$; a −4 D lens has $f = −0.25\\,\\text{m}$.

## Ray Diagrams

Three principal rays for a converging lens:
1. A ray parallel to the axis refracts through the far focal point.
2. A ray through the optical center passes straight through.
3. A ray through the near focal point emerges parallel to the axis.

## Your Task

Implement the three functions below.
`,
  starterCode: `def lens_image_distance(f, do):
    # Return the image distance di given focal length f and object distance do
    pass

def lens_magnification(di, do):
    # Return the lateral magnification
    pass

def lens_power(f_meters):
    # Return the lens power in diopters given focal length in meters
    pass
`,
  solution: `def lens_image_distance(f, do):
    return 1 / (1 / f - 1 / do)

def lens_magnification(di, do):
    return -di / do

def lens_power(f_meters):
    return 1 / f_meters
`,
  tests: [
    {
      name: "lens_image_distance(f=10, do=30) = 15.0 cm",
      code: `{{FUNC}}
print(round(lens_image_distance(10, 30), 4))`,
      expected: "15.0\n",
    },
    {
      name: "lens_magnification(15, 30) = -0.5",
      code: `{{FUNC}}
print(round(lens_magnification(15, 30), 4))`,
      expected: "-0.5\n",
    },
    {
      name: "lens_power(0.25 m) = 4.0 diopters",
      code: `{{FUNC}}
print(round(lens_power(0.25), 4))`,
      expected: "4.0\n",
    },
    {
      name: "lens_power(0.5 m) = 2.0 diopters",
      code: `{{FUNC}}
print(round(lens_power(0.5), 4))`,
      expected: "2.0\n",
    },
  ],
};
