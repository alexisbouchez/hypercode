import type { Lesson } from "../../types";

export const reynoldsNumber: Lesson = {
  id: "reynolds-number",
  title: "The Reynolds Number",
  chapterId: "dynamics",
  content: `# The Reynolds Number

The **Reynolds number** $Re$ is a dimensionless ratio of inertial forces to viscous forces in a flow:

$$Re = \\frac{\\rho v L}{\\mu} = \\frac{v L}{\\nu}$$

where:
- $\\rho$ — fluid density (kg/m³)
- $v$ — flow velocity (m/s)
- $L$ — characteristic length (m); for a pipe, use the inner diameter
- $\\mu$ — dynamic viscosity (Pa·s)
- $\\nu = \\mu / \\rho$ — kinematic viscosity (m²/s)

## Flow Regimes

The Reynolds number predicts whether flow is orderly or chaotic:

| $Re$ | Regime |
|------|--------|
| $< 2300$ | **Laminar** — smooth, layered flow; viscosity dominates |
| $2300$ to $4000$ | **Transitional** — unstable, intermittent turbulence |
| $> 4000$ | **Turbulent** — chaotic mixing; inertia dominates |

## Reference Viscosities

| Fluid | $\\mu$ (Pa·s) |
|-------|--------------|
| Water at 20 °C | $1.002 \\times 10^{-3}$ |
| Water at 80 °C | $3.54 \\times 10^{-4}$ |
| Air at 20 °C | $1.81 \\times 10^{-5}$ |
| Honey | $\\sim 2$ to $10$ |

For water in a 50 mm pipe at 0.1 m/s: $Re = (1000)(0.1)(0.05) / 1.002 \\times 10^{-3} \\approx 4990$ — turbulent.

## Your Task

Implement:

- \`reynolds_number(rho, v, L, mu)\` — returns the Reynolds number (dimensionless)
- \`flow_regime(Re)\` — returns \`"laminar"\`, \`"transitional"\`, or \`"turbulent"\`
`,

  starterCode: `def reynolds_number(rho, v, L, mu):
    # Re = rho * v * L / mu
    pass

def flow_regime(Re):
    # Re < 2300: "laminar"
    # 2300 <= Re <= 4000: "transitional"
    # Re > 4000: "turbulent"
    pass
`,

  solution: `def reynolds_number(rho, v, L, mu):
    return rho * v * L / mu

def flow_regime(Re):
    if Re < 2300:
        return "laminar"
    elif Re <= 4000:
        return "transitional"
    else:
        return "turbulent"
`,

  tests: [
    {
      name: "reynolds_number(1000.0, 0.1, 0.05, 1.002e-3) ≈ 4990.0",
      code: `{{FUNC}}
print(round(reynolds_number(1000.0, 0.1, 0.05, 1.002e-3), 1))`,
      expected: "4990.0\n",
    },
    {
      name: "flow_regime(1000) = laminar",
      code: `{{FUNC}}
print(flow_regime(1000))`,
      expected: "laminar\n",
    },
    {
      name: "flow_regime(3000) = transitional",
      code: `{{FUNC}}
print(flow_regime(3000))`,
      expected: "transitional\n",
    },
    {
      name: "flow_regime(50000) = turbulent",
      code: `{{FUNC}}
print(flow_regime(50000))`,
      expected: "turbulent\n",
    },
  ],
};
