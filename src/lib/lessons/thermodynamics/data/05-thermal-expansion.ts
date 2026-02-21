import type { Lesson } from "../../types";

export const thermalExpansion: Lesson = {
  id: "thermal-expansion",
  title: "Thermal Expansion",
  chapterId: "laws",
  content: `# Thermal Expansion

When a substance is heated, its particles move faster and tend to occupy more space, causing the material to expand. This phenomenon — **thermal expansion** — is important in engineering design, from bridge expansion joints to bimetallic thermostats.

## Linear Expansion

For a solid rod or beam of initial length $L_0$, a temperature change $\\Delta T$ produces a length change:

$$\\Delta L = \\alpha L_0 \\Delta T$$

The final length is therefore:

$$L = L_0(1 + \\alpha \\Delta T)$$

Where $\\alpha$ is the **coefficient of linear expansion** (units: per °C or per K, since only differences matter).

### Common Coefficients of Linear Expansion

| Material | $\\alpha$ (per °C) |
|---|---|
| Steel | $12 \\times 10^{-6}$ |
| Aluminium | $23 \\times 10^{-6}$ |
| Copper | $17 \\times 10^{-6}$ |
| Glass (Pyrex) | $3.3 \\times 10^{-6}$ |
| Invar (Fe-Ni alloy) | $1.2 \\times 10^{-6}$ |

Invar's exceptionally low $\\alpha$ makes it ideal for precision instruments.

## Volumetric Expansion

For three-dimensional objects, the volume change is:

$$\\Delta V = \\beta V_0 \\Delta T$$

Where $\\beta$ is the **coefficient of volumetric expansion**. For **isotropic solids** (equal expansion in all directions):

$$\\beta \\approx 3\\alpha$$

This follows from $(1 + \\alpha \\Delta T)^3 \\approx 1 + 3\\alpha \\Delta T$ for small $\\alpha \\Delta T$.

Liquids expand volumetrically but not linearly in a defined sense. For example, water has $\\beta \\approx 207 \\times 10^{-6}\\,\\text{per °C}$ near 20°C (water's expansion behavior is unusual near 4°C, where it reaches its maximum density).

## Engineering Significance

Thermal expansion must be accounted for in:
- **Bridge and rail design** — expansion gaps prevent buckling
- **Pipeline systems** — expansion loops absorb length changes
- **Precision instruments** — low-expansion alloys maintain dimensional stability
- **Bimetallic strips** — two metals with different $\\alpha$ values bend when heated, acting as a temperature switch

## Your Task

Implement the three functions below.
`,
  starterCode: `def linear_expansion(alpha, L0, delta_T):
    # Return the change in length ΔL for a rod with expansion coefficient alpha,
    # initial length L0, and temperature change delta_T
    pass

def final_length(alpha, L0, delta_T):
    # Return the final length of the rod after thermal expansion
    pass

def volumetric_expansion(beta, V0, delta_T):
    # Return the change in volume ΔV for volumetric expansion coefficient beta,
    # initial volume V0, and temperature change delta_T
    pass
`,
  solution: `def linear_expansion(alpha, L0, delta_T):
    return alpha * L0 * delta_T

def final_length(alpha, L0, delta_T):
    return L0 * (1 + alpha * delta_T)

def volumetric_expansion(beta, V0, delta_T):
    return beta * V0 * delta_T
`,
  tests: [
    {
      name: "Steel rod 10m heated 100°C: ΔL = 0.012m",
      code: `{{FUNC}}
print(round(linear_expansion(12e-6, 10, 100), 4))`,
      expected: "0.012\n",
    },
    {
      name: "Aluminium 1m heated 50°C: final length",
      code: `{{FUNC}}
print(round(final_length(23e-6, 1.0, 50), 6))`,
      expected: "1.00115\n",
    },
    {
      name: "Water 1L heated 20°C: ΔV",
      code: `{{FUNC}}
print(round(volumetric_expansion(207e-6, 1.0, 20), 5))`,
      expected: "0.00414\n",
    },
    {
      name: "Steel β≈3α: volumetric expansion of 1m³, 100°C",
      code: `{{FUNC}}
print(round(volumetric_expansion(3*12e-6, 1.0, 100), 4))`,
      expected: "0.0036\n",
    },
  ],
};
