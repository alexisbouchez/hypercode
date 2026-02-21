import type { Lesson } from "../../types";

export const manometer: Lesson = {
  id: "manometer",
  title: "Manometers",
  chapterId: "statics",
  content: `# Manometers

A **manometer** is a device that uses a liquid column to measure pressure differences. The simplest form is the **U-tube manometer**, where a dense indicating fluid (often mercury, $\\rho = 13\\,546\\,\\text{kg/m}^3$) fills the bottom of a U-shaped tube.

## Simple U-Tube Manometer

For a U-tube connecting a pressure source on one side to atmosphere on the other, the gauge pressure at the source equals:

$$\\Delta P = \\rho_m \\, g \\, \\Delta h$$

where $\\rho_m$ is the manometer fluid density and $\\Delta h$ is the height difference between the two fluid columns.

## Differential Manometer

When connecting two pipes at different elevations, the pressure difference accounting for both the manometer fluid and the pipe fluid is:

$$P_1 - P_2 = \\rho_m \\, g \\, h_m - \\rho_f \\, g \\, (z_2 - z_1)$$

where:
- $\\rho_m$ — manometer fluid density (kg/m³)
- $h_m$ — manometer reading (height difference, m)
- $\\rho_f$ — pipe fluid density (kg/m³)
- $z_1, z_2$ — elevations of the two pipe centrelines (m)

When the pipes are at the same elevation ($z_1 = z_2$), this reduces to the simple formula $\\Delta P = \\rho_m g h_m$.

### Example: Mercury Manometer

A mercury manometer ($\\rho_m = 13\\,546\\,\\text{kg/m}^3$) reads $\\Delta h = 0.1\\,\\text{m}$:

$$\\Delta P = 13\\,546 \\times 9.81 \\times 0.1 = 13\\,288.6\\,\\text{Pa}$$

## Your Task

Implement:
- \`manometer_pressure(rho_m, delta_h)\` — pressure difference for a simple U-tube (Pa)
- \`gauge_pressure_manometer(rho_m, h_m, rho_fluid, z1, z2)\` — differential manometer reading (Pa)
`,

  starterCode: `def manometer_pressure(rho_m, delta_h):
    # g = 9.81; return rho_m * g * delta_h
    pass

def gauge_pressure_manometer(rho_m, h_m, rho_fluid, z1, z2):
    # g = 9.81; return rho_m*g*h_m - rho_fluid*g*(z2 - z1)
    pass
`,

  solution: `def manometer_pressure(rho_m, delta_h):
    g = 9.81
    return rho_m * g * delta_h

def gauge_pressure_manometer(rho_m, h_m, rho_fluid, z1, z2):
    g = 9.81
    return rho_m * g * h_m - rho_fluid * g * (z2 - z1)
`,

  tests: [
    {
      name: "manometer_pressure(13546.0, 0.1) = 13288.626 Pa (10 cm mercury column)",
      code: `{{FUNC}}
print(round(manometer_pressure(13546.0, 0.1), 3))`,
      expected: "13288.626\n",
    },
    {
      name: "manometer_pressure(1000.0, 0.5) = 4905.0 Pa (50 cm water column)",
      code: `{{FUNC}}
print(manometer_pressure(1000.0, 0.5))`,
      expected: "4905.0\n",
    },
    {
      name: "gauge_pressure_manometer same elevation equals manometer_pressure",
      code: `{{FUNC}}
print(round(gauge_pressure_manometer(13546.0, 0.1, 1000.0, 0.0, 0.0), 3))`,
      expected: "13288.626\n",
    },
    {
      name: "gauge_pressure_manometer(13546.0, 0.2, 1000.0, 0.0, 0.1) = 25596.252 Pa",
      code: `{{FUNC}}
print(round(gauge_pressure_manometer(13546.0, 0.2, 1000.0, 0.0, 0.1), 3))`,
      expected: "25596.252\n",
    },
  ],
};
