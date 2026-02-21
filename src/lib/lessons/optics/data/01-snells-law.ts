import type { Lesson } from "../../types";

export const snellsLaw: Lesson = {
  id: "snells-law",
  title: "Snell's Law",
  chapterId: "geometric",
  content: `# Snell's Law

When light travels from one transparent medium into another, it changes direction. This bending of light at an interface is called **refraction**, and the relationship between the angles is described by Snell's Law.

## The Law

$$n_1 \\sin\\theta_1 = n_2 \\sin\\theta_2$$

Where:
- $n_1$ is the refractive index of the first medium
- $n_2$ is the refractive index of the second medium
- $\\theta_1$ is the angle of incidence (measured from the normal)
- $\\theta_2$ is the angle of refraction (measured from the normal)

## Refractive Index

The refractive index $n$ of a medium is defined as the ratio of the speed of light in vacuum to the speed of light in that medium:

$$n = \\frac{c}{v}$$

| Medium | Refractive Index |
|--------|-----------------|
| Vacuum | 1.000 |
| Air | ~1.0003 |
| Water | 1.333 |
| Glass (typical) | 1.5 |
| Diamond | 2.42 |

## Solving for the Refracted Angle

Rearranging Snell's Law to find $\\theta_2$:

$$\\theta_2 = \\arcsin\\!\\left(\\frac{n_1 \\sin\\theta_1}{n_2}\\right)$$

## Critical Angle and Total Internal Reflection

When light moves from a denser medium ($n_1 > n_2$) to a less dense medium, at a certain angle the refracted ray runs along the boundary. This is the **critical angle** $\\theta_c$:

$$\\theta_c = \\arcsin\\!\\left(\\frac{n_2}{n_1}\\right) \\quad (n_1 > n_2)$$

For angles of incidence greater than $\\theta_c$, no light escapes — this is **total internal reflection**, the principle behind optical fibers and diamond sparkle.

## Your Task

Implement the two functions below using \`math.asin\`, \`math.sin\`, \`math.degrees\`, and \`math.radians\`.
`,
  starterCode: `import math

def snell_refraction(n1, t1_deg, n2):
    # Return the refracted angle in degrees given n1, angle of incidence t1_deg, and n2
    pass

def critical_angle(n1, n2):
    # Return the critical angle in degrees if n1 > n2, otherwise return None
    pass
`,
  solution: `import math

def snell_refraction(n1, t1_deg, n2):
    t1_rad = math.radians(t1_deg)
    t2_rad = math.asin(n1 * math.sin(t1_rad) / n2)
    return math.degrees(t2_rad)

def critical_angle(n1, n2):
    if n1 <= n2:
        return None
    return math.degrees(math.asin(n2 / n1))
`,
  tests: [
    {
      name: "snell_refraction(1.0, 30, 1.5) = 19.4712°",
      code: `{{FUNC}}
print(round(snell_refraction(1.0, 30, 1.5), 4))`,
      expected: "19.4712\n",
    },
    {
      name: "snell_refraction(1.0, 45, 1.5) = 28.1255°",
      code: `{{FUNC}}
print(round(snell_refraction(1.0, 45, 1.5), 4))`,
      expected: "28.1255\n",
    },
    {
      name: "critical_angle(1.5, 1.0) = 41.8103°",
      code: `{{FUNC}}
print(round(critical_angle(1.5, 1.0), 4))`,
      expected: "41.8103\n",
    },
    {
      name: "critical_angle(1.0, 1.5) = None",
      code: `{{FUNC}}
print(critical_angle(1.0, 1.5))`,
      expected: "None\n",
    },
  ],
};
