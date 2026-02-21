import type { Lesson } from "../../types";

export const viscosity: Lesson = {
  id: "viscosity",
  title: "Viscosity and Shear Stress",
  chapterId: "viscous",
  content: `# Viscosity and Shear Stress

**Viscosity** is a fluid's resistance to shear deformation — its internal "stickiness". Honey is highly viscous; water is only mildly so; air has very low viscosity.

## Newton's Law of Viscosity

For a **Newtonian fluid**, shear stress $\\tau$ is proportional to the velocity gradient perpendicular to the flow:

$$\\tau = \\mu \\frac{du}{dy}$$

where:
- $\\tau$ — shear stress (Pa)
- $\\mu$ — **dynamic viscosity** (Pa·s), also written mPa·s or cP
- $du/dy$ — velocity gradient (s⁻¹); how fast velocity changes across the fluid layer

The shear **force** on an area $A$ is simply:

$$F = \\tau A = \\mu \\frac{du}{dy} A$$

## Kinematic Viscosity

**Kinematic viscosity** $\\nu$ normalises dynamic viscosity by density:

$$\\nu = \\frac{\\mu}{\\rho}$$

Units: m²/s (also written as cSt, where 1 cSt = 10⁻⁶ m²/s). It appears naturally in the Reynolds number $Re = vL/\\nu$.

## Temperature Dependence

| Fluid | $T$ (°C) | $\\mu$ (Pa·s) |
|-------|----------|--------------|
| Water | 20 | $1.002 \\times 10^{-3}$ |
| Water | 80 | $3.54 \\times 10^{-4}$ |
| Air | 20 | $1.81 \\times 10^{-5}$ |

**Liquids** become less viscous as temperature rises (thermal agitation breaks intermolecular bonds). **Gases** become more viscous as temperature rises (more molecular collisions transfer momentum).

## Your Task

Implement:

- \`shear_stress(mu, du_dy)\` — returns shear stress $\\tau = \\mu \\cdot (du/dy)$ in Pa
- \`kinematic_viscosity(mu, rho)\` — returns $\\nu = \\mu / \\rho$ in m²/s
- \`shear_force(mu, du_dy, A)\` — returns shear force $F = \\mu \\cdot (du/dy) \\cdot A$ in N
`,

  starterCode: `def shear_stress(mu, du_dy):
    # tau = mu * du_dy
    pass

def kinematic_viscosity(mu, rho):
    # nu = mu / rho
    pass

def shear_force(mu, du_dy, A):
    # F = mu * du_dy * A
    pass
`,

  solution: `def shear_stress(mu, du_dy):
    return mu * du_dy

def kinematic_viscosity(mu, rho):
    return mu / rho

def shear_force(mu, du_dy, A):
    return mu * du_dy * A
`,

  tests: [
    {
      name: "shear_stress(1.002e-3, 100.0) = 0.1002 Pa (water, steep gradient)",
      code: `{{FUNC}}
print(round(shear_stress(1.002e-3, 100.0), 4))`,
      expected: "0.1002\n",
    },
    {
      name: "kinematic_viscosity(1.002e-3, 1000.0) = 1.002e-06 m²/s (water at 20°C)",
      code: `{{FUNC}}
print(round(kinematic_viscosity(1.002e-3, 1000.0), 9))`,
      expected: "1.002e-06\n",
    },
    {
      name: "shear_stress(1.81e-5, 500.0) = 0.00905 Pa (air, high gradient)",
      code: `{{FUNC}}
print(round(shear_stress(1.81e-5, 500.0), 5))`,
      expected: "0.00905\n",
    },
    {
      name: "shear_force(1.002e-3, 100.0, 0.5) = 0.0501 N",
      code: `{{FUNC}}
print(round(shear_force(1.002e-3, 100.0, 0.5), 4))`,
      expected: "0.0501\n",
    },
  ],
};
