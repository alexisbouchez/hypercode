import type { Lesson } from "../../types";

export const polarization: Lesson = {
  id: "polarization",
  title: "Polarization and Malus's Law",
  chapterId: "wave",
  content: `# Polarization and Malus's Law

Light is a transverse electromagnetic wave — the electric field oscillates perpendicular to the direction of propagation. **Polarization** describes the orientation of this oscillation.

## Types of Polarization

- **Unpolarized light**: electric field oscillates in all directions perpendicular to propagation (e.g., sunlight)
- **Linearly polarized light**: electric field oscillates in one fixed direction
- **Circular/elliptical polarization**: the electric field vector rotates as the wave propagates

## Polarizers

A linear polarizer transmits only the component of light parallel to its transmission axis. When unpolarized light passes through a polarizer, the transmitted intensity is halved:

$$I = \\frac{I_0}{2}$$

## Malus's Law

When **already polarized** light of intensity $I_0$ passes through a polarizer whose transmission axis makes an angle $\\theta$ with the polarization direction:

$$I = I_0 \\cos^2\\theta$$

This is **Malus's Law**. At $\\theta = 0°$, full transmission. At $\\theta = 90°$, complete extinction.

**Example:** $I_0 = 100\\,\\text{W/m}^2$, $\\theta = 45°$:

$$I = 100 \\cos^2 45° = 100 \\times 0.5 = 50\\,\\text{W/m}^2$$

## Brewster's Angle

When light hits a surface at a special angle called **Brewster's angle** $\\theta_B$, the reflected light is completely polarized parallel to the surface. The condition is:

$$\\tan\\theta_B = \\frac{n_2}{n_1}$$

$$\\theta_B = \\arctan\\!\\left(\\frac{n_2}{n_1}\\right)$$

At Brewster's angle, the reflected and refracted rays are perpendicular. Polarized sunglasses exploit this to block glare from horizontal surfaces (road, water).

**Example:** Air ($n_1 = 1$) to glass ($n_2 = 1.5$):

$$\\theta_B = \\arctan(1.5) \\approx 56.3°$$

## Your Task

Implement Malus's Law and Brewster's angle using \`math.cos\`, \`math.radians\`, and \`math.atan\`.
`,
  starterCode: `import math

def malus_intensity(I0, theta_deg):
    # Return the transmitted intensity using Malus's law
    # I0: incident intensity, theta_deg: angle between polarization and polarizer axis
    pass

def brewster_angle_deg(n1, n2):
    # Return Brewster's angle in degrees for light going from medium n1 to n2
    pass
`,
  solution: `import math

def malus_intensity(I0, theta_deg):
    return I0 * math.cos(math.radians(theta_deg)) ** 2

def brewster_angle_deg(n1, n2):
    return math.degrees(math.atan(n2 / n1))
`,
  tests: [
    {
      name: "malus_intensity(100, 45°) = 50.0 W/m²",
      code: `{{FUNC}}
print(round(malus_intensity(100, 45), 4))`,
      expected: "50.0\n",
    },
    {
      name: "malus_intensity(100, 60°) = 25.0 W/m²",
      code: `{{FUNC}}
print(round(malus_intensity(100, 60), 4))`,
      expected: "25.0\n",
    },
    {
      name: "brewster_angle_deg(1.0, 1.5) = 56.3099°",
      code: `{{FUNC}}
print(round(brewster_angle_deg(1.0, 1.5), 4))`,
      expected: "56.3099\n",
    },
    {
      name: "brewster_angle_deg(1.0, 1.33) = 53.0612°",
      code: `{{FUNC}}
print(round(brewster_angle_deg(1.0, 1.33), 4))`,
      expected: "53.0612\n",
    },
  ],
};
