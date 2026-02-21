import type { Lesson } from "../../types";

export const density: Lesson = {
  id: "density",
  title: "Density and Specific Gravity",
  chapterId: "fundamentals",
  content: `# Density and Specific Gravity

**Density** is the mass per unit volume of a substance:

$$\\rho = \\frac{m}{V}$$

where $m$ is mass in kilograms and $V$ is volume in cubic metres. The SI unit of density is kg/m³.

## Common Densities

| Substance | Density (kg/m³) |
|-----------|----------------|
| Air (sea level) | 1.225 |
| Ethanol | 789 |
| Water (4 °C) | 1000 |
| Seawater | ~1025 |
| Mercury | 13 546 |

## Specific Gravity

**Specific gravity** (SG) is the dimensionless ratio of a substance's density to that of pure water at standard conditions ($\\rho_{\\text{water}} = 1000\\,\\text{kg/m}^3$):

$$SG = \\frac{\\rho}{\\rho_{\\text{water}}}$$

A specific gravity greater than 1 means the substance is denser than water and will sink; less than 1 means it will float.

### Examples

| Substance | $\\rho$ (kg/m³) | SG |
|-----------|----------------|-----|
| Ethanol | 789 | 0.789 |
| Water | 1000 | 1.000 |
| Mercury | 13 546 | 13.546 |

## Your Task

Implement \`density(mass, volume)\` that returns density in kg/m³, and \`specific_gravity(rho)\` that returns the dimensionless specific gravity relative to water.
`,

  starterCode: `def density(mass, volume):
    # Return mass / volume in kg/m³
    pass

def specific_gravity(rho):
    # Return rho / rho_water (rho_water = 1000.0 kg/m³)
    pass
`,

  solution: `def density(mass, volume):
    return mass / volume

def specific_gravity(rho):
    rho_water = 1000.0
    return rho / rho_water
`,

  tests: [
    {
      name: "density(10.0, 0.01) = 1000.0 kg/m³",
      code: `{{FUNC}}
print(density(10.0, 0.01))`,
      expected: "1000.0\n",
    },
    {
      name: "density(5.0, 0.005) = 1000.0 kg/m³",
      code: `{{FUNC}}
print(density(5.0, 0.005))`,
      expected: "1000.0\n",
    },
    {
      name: "specific_gravity of mercury (13546 kg/m³) = 13.546",
      code: `{{FUNC}}
print(specific_gravity(13546.0))`,
      expected: "13.546\n",
    },
    {
      name: "specific_gravity of ethanol (789 kg/m³) = 0.789",
      code: `{{FUNC}}
print(specific_gravity(789.0))`,
      expected: "0.789\n",
    },
  ],
};
