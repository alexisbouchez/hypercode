import type { Lesson } from "../../types";

export const flowMeasurement: Lesson = {
  id: "flow-measurement",
  title: "Flow Measurement Devices",
  chapterId: "dynamics",
  content: `# Flow Measurement Devices

Bernoulli's equation is the basis for many practical flow measurement instruments. By measuring the pressure difference between two points, the velocity or flow rate can be inferred.

## Venturi Meter

A **Venturi meter** is a converging-diverging tube inserted in a pipe. Flow accelerates at the narrowest section (the throat), dropping the pressure. Applying Bernoulli and continuity between the inlet ($A_1$) and throat ($A_2$):

$$Q = A_2\\sqrt{\\frac{2\\Delta P}{\\rho\\left(1 - \\left(\\dfrac{A_2}{A_1}\\right)^2\\right)}}$$

where $\\Delta P = P_1 - P_2$ is the measured pressure difference.

The throat-to-inlet area ratio $A_2/A_1$ is typically 0.25–0.75. Smaller ratios give a larger $\\Delta P$ signal but higher permanent pressure loss.

In practice a **discharge coefficient** $C_d \\approx 0.98$ accounts for viscous losses, but we omit it here for clarity.

## Pitot Tube

A **Pitot tube** points directly into the flow. It measures the **stagnation pressure** $P_0$ at its tip (where the fluid is brought to rest) against the **static pressure** $P_s$ from a side port. Bernoulli gives:

$$v = \\sqrt{\\frac{2(P_0 - P_s)}{\\rho}}$$

Pitot tubes are used on aircraft to measure airspeed. The pressure difference for a 100 m/s aircraft in air ($\\rho = 1.225$ kg/m³) is about 6 kPa.

## Your Task

Implement:

- \`venturi_flow(A1, A2, delta_P, rho)\` — volumetric flow rate $Q$ (m³/s)
- \`pitot_velocity(P_stagnation, P_static, rho)\` — flow velocity $v$ (m/s)
`,

  starterCode: `import math

def venturi_flow(A1, A2, delta_P, rho):
    # Q = A2 * sqrt(2 * delta_P / (rho * (1 - (A2/A1)^2)))
    pass

def pitot_velocity(P_stagnation, P_static, rho):
    # v = sqrt(2 * (P_stagnation - P_static) / rho)
    pass
`,

  solution: `import math

def venturi_flow(A1, A2, delta_P, rho):
    return A2 * math.sqrt(2 * delta_P / (rho * (1 - (A2/A1)**2)))

def pitot_velocity(P_stagnation, P_static, rho):
    return math.sqrt(2 * (P_stagnation - P_static) / rho)
`,

  tests: [
    {
      name: "pitot_velocity(101825.0, 101325.0, 1.225) ≈ 28.5714 m/s (aircraft airspeed)",
      code: `{{FUNC}}
print(round(pitot_velocity(101825.0, 101325.0, 1.225), 4))`,
      expected: "28.5714\n",
    },
    {
      name: "pitot_velocity(102325.0, 101325.0, 1.000) ≈ 44.7214 m/s (1000 Pa difference)",
      code: `{{FUNC}}
print(round(pitot_velocity(102325.0, 101325.0, 1.000), 4))`,
      expected: "44.7214\n",
    },
    {
      name: "venturi_flow(0.01, 0.005, 10000.0, 1000.0) ≈ 0.02582 m³/s",
      code: `{{FUNC}}
print(round(venturi_flow(0.01, 0.005, 10000.0, 1000.0), 6))`,
      expected: "0.02582\n",
    },
    {
      name: "venturi_flow(0.05, 0.02, 50000.0, 1000.0) ≈ 0.2182 m³/s",
      code: `{{FUNC}}
print(round(venturi_flow(0.05, 0.02, 50000.0, 1000.0), 4))`,
      expected: "0.2182\n",
    },
  ],
};
