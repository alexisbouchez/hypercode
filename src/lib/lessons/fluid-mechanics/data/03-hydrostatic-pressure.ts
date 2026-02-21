import type { Lesson } from "../../types";

export const hydrostaticPressure: Lesson = {
  id: "hydrostatic-pressure",
  title: "Hydrostatic Pressure",
  chapterId: "statics",
  content: `# Hydrostatic Pressure

A fluid at rest exerts pressure that increases with depth. This **hydrostatic pressure** arises from the weight of fluid above a given point:

$$P = \\rho g h$$

where:
- $\\rho$ is fluid density (kg/m³)
- $g = 9.81\\,\\text{m/s}^2$ is gravitational acceleration
- $h$ is depth below the free surface (m)

## Total Absolute Pressure at Depth

Adding atmospheric pressure gives the total (absolute) pressure at depth $h$:

$$P_{\\text{total}} = P_{\\text{atm}} + \\rho g h$$

where $P_{\\text{atm}} = 101\\,325\\,\\text{Pa}$.

## Hydrostatic Force on a Horizontal Surface

The hydrostatic force on a horizontal flat surface of area $A$ at depth $h$ is simply:

$$F = P \\cdot A = \\rho g h A$$

### Examples

| Fluid | Depth (m) | Gauge Pressure (Pa) |
|-------|-----------|-------------------|
| Water (1000 kg/m³) | 10 | 98 100 |
| Water (1000 kg/m³) | 100 | 981 000 |
| Seawater (1025 kg/m³) | 10 | 100 552.5 |

## Your Task

Implement:
- \`hydrostatic_pressure(rho, h)\` — gauge pressure at depth $h$ (Pa)
- \`total_pressure(rho, h)\` — absolute pressure at depth $h$ (Pa)
- \`hydrostatic_force(rho, h, A)\` — force on a horizontal surface (N)
`,

  starterCode: `def hydrostatic_pressure(rho, h):
    # g = 9.81 m/s²; return rho * g * h
    pass

def total_pressure(rho, h):
    # P_atm = 101325.0 Pa; return P_atm + rho * g * h
    pass

def hydrostatic_force(rho, h, A):
    # return rho * g * h * A
    pass
`,

  solution: `def hydrostatic_pressure(rho, h):
    g = 9.81
    return rho * g * h

def total_pressure(rho, h):
    g = 9.81
    P_atm = 101325.0
    return P_atm + rho * g * h

def hydrostatic_force(rho, h, A):
    g = 9.81
    return rho * g * h * A
`,

  tests: [
    {
      name: "hydrostatic_pressure(1000.0, 10.0) = 98100.0 Pa (10 m water column)",
      code: `{{FUNC}}
print(hydrostatic_pressure(1000.0, 10.0))`,
      expected: "98100.0\n",
    },
    {
      name: "total_pressure(1000.0, 10.0) = 199425.0 Pa",
      code: `{{FUNC}}
print(round(total_pressure(1000.0, 10.0), 1))`,
      expected: "199425.0\n",
    },
    {
      name: "hydrostatic_pressure(1000.0, 100.0) = 981000.0 Pa (100 m depth)",
      code: `{{FUNC}}
print(hydrostatic_pressure(1000.0, 100.0))`,
      expected: "981000.0\n",
    },
    {
      name: "hydrostatic_force(1000.0, 5.0, 2.0) = 98100.0 N",
      code: `{{FUNC}}
print(hydrostatic_force(1000.0, 5.0, 2.0))`,
      expected: "98100.0\n",
    },
  ],
};
