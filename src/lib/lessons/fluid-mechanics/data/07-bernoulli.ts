import type { Lesson } from "../../types";

export const bernoulliEquation: Lesson = {
  id: "bernoulli-equation",
  title: "Bernoulli's Equation",
  chapterId: "dynamics",
  content: `# Bernoulli's Equation

**Bernoulli's equation** expresses conservation of energy for an ideal (inviscid), incompressible, steady flow along a streamline:

$$P_1 + \\frac{1}{2}\\rho v_1^2 + \\rho g z_1 = P_2 + \\frac{1}{2}\\rho v_2^2 + \\rho g z_2$$

The three terms represent **pressure energy**, **kinetic energy**, and **potential energy** per unit volume. Their sum is constant along any streamline.

## Solving for Downstream Pressure

Rearranging for the pressure at point 2:

$$P_2 = P_1 + \\frac{1}{2}\\rho(v_1^2 - v_2^2) + \\rho g(z_1 - z_2)$$

where $g = 9.81$ m/s² is gravitational acceleration, $z$ is elevation (m), and $\rho$ is fluid density (kg/m³).

**Key insight:** speed and pressure trade off. A faster-moving fluid exerts less static pressure — the basis of the Venturi effect and aircraft lift.

## Torricelli's Theorem

For a large open tank draining through a small hole at depth $h$ below the surface, we set $v_1 \\approx 0$ (large tank, surface barely moves) and $P_1 = P_2 = P_{\\text{atm}}$. Bernoulli's equation simplifies to:

$$v_2 = \\sqrt{2gh}$$

This is **Torricelli's theorem** — the exit velocity equals the free-fall speed from height $h$.

## Your Task

Implement:

- \`bernoulli_pressure(P1, rho, v1, z1, v2, z2)\` — pressure at point 2 (Pa). Use $g = 9.81$ m/s² inside the function.
- \`torricelli_velocity(h)\` — drain velocity (m/s) for a tank with head $h$. Use $g = 9.81$ m/s² inside the function.
`,

  starterCode: `import math

def bernoulli_pressure(P1, rho, v1, z1, v2, z2):
    # g = 9.81 m/s²
    # P2 = P1 + 0.5 * rho * (v1² - v2²) + rho * g * (z1 - z2)
    pass

def torricelli_velocity(h):
    # g = 9.81 m/s²
    # v = sqrt(2 * g * h)
    pass
`,

  solution: `import math

def bernoulli_pressure(P1, rho, v1, z1, v2, z2):
    g = 9.81
    return P1 + 0.5 * rho * (v1**2 - v2**2) + rho * g * (z1 - z2)

def torricelli_velocity(h):
    g = 9.81
    return math.sqrt(2 * g * h)
`,

  tests: [
    {
      name: "torricelli_velocity(5.0) ≈ 9.9045 m/s",
      code: `{{FUNC}}
print(round(torricelli_velocity(5.0), 4))`,
      expected: "9.9045\n",
    },
    {
      name: "torricelli_velocity(10.0) ≈ 14.0071 m/s",
      code: `{{FUNC}}
print(round(torricelli_velocity(10.0), 4))`,
      expected: "14.0071\n",
    },
    {
      name: "bernoulli_pressure: high elevation, slow upstream → 186925.0 Pa",
      code: `{{FUNC}}
print(bernoulli_pressure(101325.0, 1000.0, 0.0, 10.0, 5.0, 0.0))`,
      expected: "186925.0\n",
    },
    {
      name: "bernoulli_pressure: same elevation, faster downstream → lower pressure",
      code: `{{FUNC}}
print(bernoulli_pressure(200000.0, 1000.0, 2.0, 0.0, 4.0, 0.0))`,
      expected: "194000.0\n",
    },
  ],
};
