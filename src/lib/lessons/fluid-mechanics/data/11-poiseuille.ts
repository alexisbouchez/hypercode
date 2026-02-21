import type { Lesson } from "../../types";

export const poiseuilleFlow: Lesson = {
  id: "poiseuille-flow",
  title: "Hagen-Poiseuille Flow",
  chapterId: "viscous",
  content: `# Hagen-Poiseuille Flow

**Hagen-Poiseuille flow** describes steady, fully-developed laminar flow of a viscous fluid through a straight circular pipe driven by a pressure difference.

## Volumetric Flow Rate

The flow rate $Q$ (m³/s) is:

$$Q = \\frac{\\pi r^4 \\Delta P}{8 \\mu L}$$

where:
- $r$ — pipe radius (m)
- $\\Delta P$ — pressure difference $P_1 - P_2$ (Pa)
- $\\mu$ — dynamic viscosity (Pa·s)
- $L$ — pipe length (m)

Notice the strong $r^4$ dependence — halving the radius reduces flow by a factor of 16.

## Mean and Maximum Velocity

The **mean (average) velocity** across the cross-section:

$$\\bar{v} = \\frac{Q}{\\pi r^2} = \\frac{r^2 \\Delta P}{8 \\mu L}$$

The velocity profile is parabolic, with the **maximum velocity at the centreline** being exactly twice the mean:

$$v_{\\max} = 2\\bar{v} = \\frac{r^2 \\Delta P}{4 \\mu L}$$

## Validity

This law applies only to **laminar flow**, which requires $Re < 2300$. For turbulent flow a different (empirical) approach is needed.

## Your Task

Implement:

- \`poiseuille_flow(r, delta_P, mu, L)\` — volumetric flow rate $Q$ (m³/s)
- \`mean_velocity(r, delta_P, mu, L)\` — mean velocity $\\bar{v}$ (m/s)
- \`max_velocity(r, delta_P, mu, L)\` — centreline velocity $v_{\\max}$ (m/s)
`,

  starterCode: `import math

def poiseuille_flow(r, delta_P, mu, L):
    # Q = pi * r^4 * delta_P / (8 * mu * L)
    pass

def mean_velocity(r, delta_P, mu, L):
    # v_mean = r^2 * delta_P / (8 * mu * L)
    pass

def max_velocity(r, delta_P, mu, L):
    # v_max = r^2 * delta_P / (4 * mu * L)
    pass
`,

  solution: `import math

def poiseuille_flow(r, delta_P, mu, L):
    return math.pi * r**4 * delta_P / (8 * mu * L)

def mean_velocity(r, delta_P, mu, L):
    return r**2 * delta_P / (8 * mu * L)

def max_velocity(r, delta_P, mu, L):
    return r**2 * delta_P / (4 * mu * L)
`,

  tests: [
    {
      name: "poiseuille_flow(0.01, 1000.0, 1.002e-3, 1.0) ≈ 0.003919 m³/s (water in 1 cm pipe)",
      code: `{{FUNC}}
print(round(poiseuille_flow(0.01, 1000.0, 1.002e-3, 1.0), 6))`,
      expected: "0.003919\n",
    },
    {
      name: "mean_velocity(0.01, 1000.0, 1.002e-3, 1.0) ≈ 12.475 m/s",
      code: `{{FUNC}}
print(round(mean_velocity(0.01, 1000.0, 1.002e-3, 1.0), 4))`,
      expected: "12.475\n",
    },
    {
      name: "max_velocity(0.01, 1000.0, 1.002e-3, 1.0) ≈ 24.9501 m/s (twice mean)",
      code: `{{FUNC}}
print(round(max_velocity(0.01, 1000.0, 1.002e-3, 1.0), 4))`,
      expected: "24.9501\n",
    },
    {
      name: "poiseuille_flow(0.1, 100.0, 1.0, 1.0) ≈ 0.003927 m³/s (unit viscosity)",
      code: `{{FUNC}}
print(round(poiseuille_flow(0.1, 100.0, 1.0, 1.0), 6))`,
      expected: "0.003927\n",
    },
  ],
};
