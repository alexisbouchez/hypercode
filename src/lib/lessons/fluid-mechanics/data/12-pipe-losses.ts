import type { Lesson } from "../../types";

export const pipeLosses: Lesson = {
  id: "pipe-losses",
  title: "Pipe Friction Losses",
  chapterId: "viscous",
  content: `# Pipe Friction Losses

Real pipe flow loses energy to viscous friction. The **Darcy-Weisbach equation** quantifies this as a **head loss** $h_f$ (metres of fluid):

$$h_f = f \\frac{L}{D} \\frac{v^2}{2g}$$

where:
- $f$ — Darcy friction factor (dimensionless)
- $L$ — pipe length (m)
- $D$ — pipe diameter (m)
- $v$ — mean flow velocity (m/s)
- $g = 9.81$ m/s²

Head loss is the height a fluid column would need to fall to supply the same energy. It is proportional to $v^2$, so turbulent flow is expensive.

## Friction Factor for Laminar Flow

For laminar flow ($Re < 2300$) the friction factor has an exact analytical value:

$$f = \\frac{64}{Re}$$

For turbulent flow, empirical correlations such as the **Moody chart** or the **Colebrook equation** are used instead.

## Pressure Drop

Head loss converts to a **pressure drop** via:

$$\\Delta P = \\rho g h_f$$

This is the pressure the pump must provide to maintain the flow.

## Your Task

Implement:

- \`head_loss(f, L, D, v)\` — returns $h_f$ (m). Use $g = 9.81$ m/s² inside the function.
- \`friction_factor_laminar(Re)\` — returns Darcy $f = 64/Re$ for laminar flow
- \`pressure_drop(rho, h_f)\` — returns $\\Delta P$ (Pa). Use $g = 9.81$ m/s² inside the function.
`,

  starterCode: `def head_loss(f, L, D, v):
    # g = 9.81 m/s²
    # h_f = f * (L/D) * v^2 / (2*g)
    pass

def friction_factor_laminar(Re):
    # f = 64 / Re
    pass

def pressure_drop(rho, h_f):
    # g = 9.81 m/s²
    # delta_P = rho * g * h_f
    pass
`,

  solution: `def head_loss(f, L, D, v):
    g = 9.81
    return f * (L / D) * (v**2 / (2 * g))

def friction_factor_laminar(Re):
    return 64.0 / Re

def pressure_drop(rho, h_f):
    g = 9.81
    return rho * g * h_f
`,

  tests: [
    {
      name: "head_loss(0.02, 100.0, 0.1, 3.0) = 9.1743 m (100 m pipe, 3 m/s flow)",
      code: `{{FUNC}}
print(round(head_loss(0.02, 100.0, 0.1, 3.0), 4))`,
      expected: "9.1743\n",
    },
    {
      name: "friction_factor_laminar(1600) = 0.04 (laminar water flow)",
      code: `{{FUNC}}
print(friction_factor_laminar(1600))`,
      expected: "0.04\n",
    },
    {
      name: "friction_factor_laminar(2000) = 0.032",
      code: `{{FUNC}}
print(friction_factor_laminar(2000))`,
      expected: "0.032\n",
    },
    {
      name: "pressure_drop(1000.0, 5.0) = 49050.0 Pa (water, 5 m head loss)",
      code: `{{FUNC}}
print(round(pressure_drop(1000.0, 5.0), 2))`,
      expected: "49050.0\n",
    },
  ],
};
