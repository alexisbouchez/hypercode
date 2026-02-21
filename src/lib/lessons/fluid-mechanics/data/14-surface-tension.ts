import type { Lesson } from "../../types";

export const surfaceTension: Lesson = {
  id: "surface-tension",
  title: "Surface Tension and Capillarity",
  chapterId: "viscous",
  content: `# Surface Tension and Capillarity

**Surface tension** $\\gamma$ (N/m) arises because molecules at a fluid surface have fewer neighbours than those in the bulk, creating a net inward force. The surface behaves like an elastic membrane that resists stretching.

| Fluid | $T$ (°C) | $\\gamma$ (N/m) |
|-------|----------|----------------|
| Water | 20 | 0.0728 |
| Water | 80 | 0.0627 |
| Mercury | 20 | 0.487 |
| Ethanol | 20 | 0.0223 |

## Capillary Rise

A liquid in a narrow tube rises (or falls) due to the balance between surface tension and gravity. The equilibrium height is:

$$h = \\frac{2\\gamma\\cos\\theta}{\\rho g r}$$

where:
- $\\gamma$ — surface tension (N/m)
- $\\theta$ — contact angle between the liquid and the tube wall
- $\\rho$ — liquid density (kg/m³)
- $g = 9.81$ m/s²
- $r$ — tube radius (m)

Water on glass has $\\theta \\approx 0°$ ($\\cos\\theta = 1$), so it rises. Mercury on glass has $\\theta \\approx 140°$ ($\\cos\\theta < 0$), so it is depressed.

## Pressure Inside Curved Surfaces

Surface tension creates a **pressure jump** across a curved interface (Young-Laplace equation). For a **spherical bubble** in a liquid (two surfaces):

$$\\Delta P = \\frac{4\\gamma}{r}$$

For a **spherical drop** in air (one surface):

$$\\Delta P = \\frac{2\\gamma}{r}$$

## Your Task

Implement:

- \`capillary_rise(gamma, theta_deg, rho, r)\` — capillary rise height $h$ (m). Accept $\\theta$ in degrees. Use $g = 9.81$ m/s² inside the function.
- \`bubble_pressure(gamma, r)\` — excess pressure inside a bubble $\\Delta P$ (Pa)
- \`drop_pressure(gamma, r)\` — excess pressure inside a drop $\\Delta P$ (Pa)
`,

  starterCode: `import math

def capillary_rise(gamma, theta_deg, rho, r):
    # g = 9.81 m/s²
    # h = 2 * gamma * cos(theta) / (rho * g * r)
    pass

def bubble_pressure(gamma, r):
    # delta_P = 4 * gamma / r
    pass

def drop_pressure(gamma, r):
    # delta_P = 2 * gamma / r
    pass
`,

  solution: `import math

def capillary_rise(gamma, theta_deg, rho, r):
    g = 9.81
    theta = math.radians(theta_deg)
    return 2 * gamma * math.cos(theta) / (rho * g * r)

def bubble_pressure(gamma, r):
    return 4 * gamma / r

def drop_pressure(gamma, r):
    return 2 * gamma / r
`,

  tests: [
    {
      name: "capillary_rise(0.0728, 0, 1000.0, 0.001) ≈ 0.014842 m (water in 1 mm glass tube)",
      code: `{{FUNC}}
print(round(capillary_rise(0.0728, 0, 1000.0, 0.001), 6))`,
      expected: "0.014842\n",
    },
    {
      name: "bubble_pressure(0.0728, 0.001) = 291.2 Pa (1 mm radius soap bubble)",
      code: `{{FUNC}}
print(bubble_pressure(0.0728, 0.001))`,
      expected: "291.2\n",
    },
    {
      name: "drop_pressure(0.0728, 0.001) = 145.6 Pa (1 mm radius water drop)",
      code: `{{FUNC}}
print(drop_pressure(0.0728, 0.001))`,
      expected: "145.6\n",
    },
    {
      name: "capillary_rise(0.0728, 60, 1000.0, 0.001) ≈ 0.007421 m (60° contact angle)",
      code: `{{FUNC}}
print(round(capillary_rise(0.0728, 60, 1000.0, 0.001), 6))`,
      expected: "0.007421\n",
    },
  ],
};
