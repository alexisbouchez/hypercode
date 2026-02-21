import type { Lesson } from "../../types";

export const reflection: Lesson = {
  id: "reflection",
  title: "Reflection and Mirrors",
  chapterId: "geometric",
  content: `# Reflection and Mirrors

The **law of reflection** states that when light bounces off a surface, the angle of reflection equals the angle of incidence, both measured from the normal to the surface:

$$\\theta_r = \\theta_i$$

## Spherical Mirrors

A spherical mirror has a focal point $F$ at half the radius of curvature $R$:

$$f = \\frac{R}{2}$$

- **Concave mirror** (converging): $f > 0$. Used in telescopes, headlights, and shaving mirrors.
- **Convex mirror** (diverging): $f < 0$. Used in security mirrors and car rear-view mirrors.

## The Mirror Equation

The relationship between object distance $d_o$, image distance $d_i$, and focal length $f$:

$$\\frac{1}{f} = \\frac{1}{d_o} + \\frac{1}{d_i}$$

Solving for the image distance:

$$d_i = \\frac{1}{\\dfrac{1}{f} - \\dfrac{1}{d_o}}$$

### Sign Conventions

| Quantity | Positive | Negative |
|----------|----------|----------|
| $d_o$ | Object in front of mirror | (always positive) |
| $d_i$ | Real image (in front) | Virtual image (behind) |
| $f$ | Concave mirror | Convex mirror |

## Magnification

The lateral magnification relates image size to object size:

$$m = -\\frac{d_i}{d_o}$$

- $|m| > 1$: image is magnified
- $|m| < 1$: image is diminished
- $m > 0$: image is upright (virtual)
- $m < 0$: image is inverted (real)

## Your Task

Implement the mirror equation and magnification formula below.
`,
  starterCode: `def mirror_image_distance(f, do):
    # Return the image distance di given focal length f and object distance do
    pass

def mirror_magnification(di, do):
    # Return the lateral magnification given image distance di and object distance do
    pass
`,
  solution: `def mirror_image_distance(f, do):
    return 1 / (1 / f - 1 / do)

def mirror_magnification(di, do):
    return -di / do
`,
  tests: [
    {
      name: "mirror_image_distance(f=20, do=60) = 30.0",
      code: `{{FUNC}}
print(round(mirror_image_distance(20, 60), 4))`,
      expected: "30.0\n",
    },
    {
      name: "mirror_image_distance(f=15, do=30) = 30.0",
      code: `{{FUNC}}
print(round(mirror_image_distance(15, 30), 4))`,
      expected: "30.0\n",
    },
    {
      name: "mirror_magnification(30, 60) = -0.5",
      code: `{{FUNC}}
print(round(mirror_magnification(30, 60), 4))`,
      expected: "-0.5\n",
    },
    {
      name: "mirror_magnification(-30, 60) = 0.5 (virtual image)",
      code: `{{FUNC}}
print(round(mirror_magnification(-30, 60), 4))`,
      expected: "0.5\n",
    },
  ],
};
