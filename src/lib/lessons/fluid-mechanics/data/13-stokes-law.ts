import type { Lesson } from "../../types";

export const stokesLaw: Lesson = {
  id: "stokes-law",
  title: "Stokes' Law and Sedimentation",
  chapterId: "viscous",
  content: `# Stokes' Law and Sedimentation

When a small sphere moves slowly through a viscous fluid ($Re \\ll 1$), the drag is dominated by viscous forces rather than inertia. **Stokes' law** gives an exact analytical result:

$$F_D = 6\\pi\\mu r v$$

where:
- $\\mu$ — dynamic viscosity of the fluid (Pa·s)
- $r$ — radius of the sphere (m)
- $v$ — velocity of the sphere relative to the fluid (m/s)

This is linear in velocity — very different from the quadratic drag seen at high Reynolds numbers.

## Terminal Velocity (Sedimentation)

A particle settling through a fluid under gravity reaches a constant **terminal velocity** when the net downward force (weight minus buoyancy) balances the Stokes drag:

$$6\\pi\\mu r v_t = \\frac{4}{3}\\pi r^3(\\rho_p - \\rho_f)g$$

Solving for $v_t$:

$$v_t = \\frac{2r^2(\\rho_p - \\rho_f)g}{9\\mu}$$

where $\\rho_p$ is the particle density and $\\rho_f$ is the fluid density.

## Applications

- **Viscometry**: measuring $\\mu$ by timing a sphere's fall
- **Sedimentation**: designing centrifuges and settling tanks
- **Aerosol science**: predicting how dust or droplets settle in air

## Your Task

Implement:

- \`stokes_drag(mu, r, v)\` — Stokes drag force $F_D$ (N)
- \`stokes_terminal_velocity(r, rho_particle, rho_fluid, mu)\` — terminal velocity $v_t$ (m/s). Use $g = 9.81$ m/s² inside the function.
`,

  starterCode: `import math

def stokes_drag(mu, r, v):
    # F_D = 6 * pi * mu * r * v
    pass

def stokes_terminal_velocity(r, rho_particle, rho_fluid, mu):
    # g = 9.81 m/s²
    # v_t = 2 * r^2 * (rho_particle - rho_fluid) * g / (9 * mu)
    pass
`,

  solution: `import math

def stokes_drag(mu, r, v):
    return 6 * math.pi * mu * r * v

def stokes_terminal_velocity(r, rho_particle, rho_fluid, mu):
    g = 9.81
    return 2 * r**2 * (rho_particle - rho_fluid) * g / (9 * mu)
`,

  tests: [
    {
      name: "stokes_drag(1.0, 1.0, 1.0) = 6π ≈ 18.8496 N (unit sphere, unit fluid)",
      code: `{{FUNC}}
print(round(stokes_drag(1.0, 1.0, 1.0), 4))`,
      expected: "18.8496\n",
    },
    {
      name: "stokes_drag(0.1, 0.1, 1.0) ≈ 0.1885 N (smaller sphere and viscosity)",
      code: `{{FUNC}}
print(round(stokes_drag(0.1, 0.1, 1.0), 4))`,
      expected: "0.1885\n",
    },
    {
      name: "stokes_terminal_velocity(0.001, 2500.0, 1000.0, 1.002e-3) ≈ 3.2635 m/s (sand in water)",
      code: `{{FUNC}}
print(round(stokes_terminal_velocity(0.001, 2500.0, 1000.0, 1.002e-3), 4))`,
      expected: "3.2635\n",
    },
    {
      name: "stokes_terminal_velocity(0.0005, 7874.0, 1000.0, 1.002e-3) ≈ 3.7389 m/s (iron particle)",
      code: `{{FUNC}}
print(round(stokes_terminal_velocity(0.0005, 7874.0, 1000.0, 1.002e-3), 4))`,
      expected: "3.7389\n",
    },
  ],
};
