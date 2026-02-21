import type { Lesson } from "../../types";

export const continuityEquation: Lesson = {
  id: "continuity-equation",
  title: "The Continuity Equation",
  chapterId: "dynamics",
  content: `# The Continuity Equation

For an **incompressible fluid** (constant density), conservation of mass requires that the **volume flow rate** $Q$ is constant along a streamtube:

$$A_1 v_1 = A_2 v_2 \\implies Q = Av = \\text{const}$$

This is the **continuity equation** — what flows in must flow out.

## Volume Flow Rate

The volume flow rate $Q$ is the volume of fluid passing a cross-section per unit time:

$$Q = Av$$

where $A$ is the cross-sectional area (m²) and $v$ is the flow velocity (m/s). Units: m³/s.

## Exit Velocity

Rearranging the continuity equation gives the velocity at any cross-section:

$$v_2 = \\frac{A_1 v_1}{A_2}$$

A **narrowing pipe** ($A_2 < A_1$) produces a **higher velocity** — this is why water speeds up at a garden hose nozzle. A widening pipe slows the flow.

## Mass Flow Rate

The **mass flow rate** $\\dot{m}$ accounts for fluid density $\\rho$ (kg/m³):

$$\\dot{m} = \\rho Q = \\rho A v$$

Units: kg/s. For an incompressible fluid, $\\dot{m}$ is also conserved along a streamtube.

## Your Task

Implement the following functions:

- \`flow_rate(A, v)\` — returns volume flow rate $Q = Av$ in m³/s
- \`exit_velocity(A1, v1, A2)\` — returns exit velocity $v_2 = A_1 v_1 / A_2$ in m/s
- \`mass_flow_rate(rho, A, v)\` — returns mass flow rate $\\dot{m} = \\rho A v$ in kg/s
`,

  starterCode: `def flow_rate(A, v):
    # Return Q = A * v in m³/s
    pass

def exit_velocity(A1, v1, A2):
    # Return v2 = A1 * v1 / A2 in m/s
    pass

def mass_flow_rate(rho, A, v):
    # Return m_dot = rho * A * v in kg/s
    pass
`,

  solution: `def flow_rate(A, v):
    return A * v

def exit_velocity(A1, v1, A2):
    return A1 * v1 / A2

def mass_flow_rate(rho, A, v):
    return rho * A * v
`,

  tests: [
    {
      name: "flow_rate(0.01, 5.0) = 0.05 m³/s",
      code: `{{FUNC}}
print(flow_rate(0.01, 5.0))`,
      expected: "0.05\n",
    },
    {
      name: "exit_velocity(0.1, 2.0, 0.05) = 4.0 m/s (pipe narrows, velocity doubles)",
      code: `{{FUNC}}
print(exit_velocity(0.1, 2.0, 0.05))`,
      expected: "4.0\n",
    },
    {
      name: "exit_velocity(0.05, 4.0, 0.1) = 2.0 m/s (pipe widens, velocity halves)",
      code: `{{FUNC}}
print(exit_velocity(0.05, 4.0, 0.1))`,
      expected: "2.0\n",
    },
    {
      name: "mass_flow_rate(1000.0, 0.01, 5.0) = 50.0 kg/s",
      code: `{{FUNC}}
print(mass_flow_rate(1000.0, 0.01, 5.0))`,
      expected: "50.0\n",
    },
  ],
};
