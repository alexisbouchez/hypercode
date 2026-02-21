import type { Lesson } from "../../types";

export const dragForce: Lesson = {
  id: "drag-force",
  title: "Drag Force",
  chapterId: "dynamics",
  content: `# Drag Force

When an object moves through a fluid, the fluid exerts a **drag force** opposing the motion. For turbulent/high-$Re$ flow the drag is dominated by **pressure drag** and follows the quadratic drag law:

$$F_D = \\frac{1}{2} \\rho v^2 C_D A$$

where:
- $\\rho$ — fluid density (kg/m³)
- $v$ — velocity of the object relative to the fluid (m/s)
- $C_D$ — **drag coefficient** (dimensionless), depends on shape and flow regime
- $A$ — **reference area** (m²), usually the frontal (projected) area

Note that drag grows with $v^2$ — doubling speed quadruples drag.

## Common Drag Coefficients

| Object | $C_D$ |
|--------|-------|
| Streamlined airfoil | $\\sim 0.04$ |
| Modern car | $\\sim 0.3$ |
| Sphere | $\\sim 0.47$ |
| Cube | $\\sim 1.05$ |
| Flat plate (broadside) | $\\sim 1.28$ |

## Terminal Velocity

An object falling through a fluid accelerates until drag equals gravity ($F_D = mg$). Solving for this **terminal velocity**:

$$v_t = \\sqrt{\\frac{2mg}{\\rho C_D A}}$$

A skydiver ($m = 70$ kg, $C_D = 1.0$, $A = 0.5$ m²) reaches about 47 m/s (~170 km/h) in air.

## Your Task

Implement:

- \`drag_force(rho, v, Cd, A)\` — returns drag force $F_D$ in newtons
- \`terminal_velocity(m, rho, Cd, A)\` — returns terminal velocity $v_t$ in m/s. Use $g = 9.81$ m/s² inside the function.
`,

  starterCode: `import math

def drag_force(rho, v, Cd, A):
    # F_D = 0.5 * rho * v² * Cd * A
    pass

def terminal_velocity(m, rho, Cd, A):
    # g = 9.81 m/s²
    # v_t = sqrt(2 * m * g / (rho * Cd * A))
    pass
`,

  solution: `import math

def drag_force(rho, v, Cd, A):
    return 0.5 * rho * v**2 * Cd * A

def terminal_velocity(m, rho, Cd, A):
    g = 9.81
    return math.sqrt(2 * m * g / (rho * Cd * A))
`,

  tests: [
    {
      name: "drag_force(1.225, 30.0, 0.3, 2.5) = 413.4375 N (car at 108 km/h)",
      code: `{{FUNC}}
print(drag_force(1.225, 30.0, 0.3, 2.5))`,
      expected: "413.4375\n",
    },
    {
      name: "drag_force(1000.0, 2.0, 0.47, 0.05) = 47.0 N (sphere in water)",
      code: `{{FUNC}}
print(drag_force(1000.0, 2.0, 0.47, 0.05))`,
      expected: "47.0\n",
    },
    {
      name: "terminal_velocity(1.0, 1.225, 0.47, 0.1) ≈ 18.46 m/s",
      code: `{{FUNC}}
print(round(terminal_velocity(1.0, 1.225, 0.47, 0.1), 2))`,
      expected: "18.46\n",
    },
    {
      name: "terminal_velocity(70.0, 1.225, 1.0, 0.5) ≈ 47.35 m/s (skydiver)",
      code: `{{FUNC}}
print(round(terminal_velocity(70.0, 1.225, 1.0, 0.5), 2))`,
      expected: "47.35\n",
    },
  ],
};
