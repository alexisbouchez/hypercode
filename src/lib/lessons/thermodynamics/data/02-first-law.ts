import type { Lesson } from "../../types";

export const firstLaw: Lesson = {
  id: "first-law",
  title: "The First Law of Thermodynamics",
  chapterId: "laws",
  content: `# The First Law of Thermodynamics

The First Law of Thermodynamics is a statement of **conservation of energy** applied to thermodynamic systems. It relates the change in internal energy of a system to the heat exchanged with its surroundings and the work done.

## The Equation

$$\\Delta U = Q - W$$

Where:
- $\\Delta U$ is the change in **internal energy** of the system (J)
- $Q$ is the **heat added to** the system (J) — positive when heat flows in, negative when it flows out
- $W$ is the **work done by** the system (J) — positive when the system expands, negative when compressed

## Sign Conventions

| Quantity | Positive | Negative |
|---|---|---|
| $Q$ | Heat absorbed by system | Heat released by system |
| $W$ | Work done by system | Work done on system |
| $\\Delta U$ | Internal energy increases | Internal energy decreases |

## Work by Expansion

When a gas expands against a constant external pressure $P$, it does work on the surroundings:

$$W = P \\Delta V$$

Where $\\Delta V = V_{\\text{final}} - V_{\\text{initial}}$ is the change in volume. If the gas is compressed, $\\Delta V < 0$ and the work is negative (work done **on** the system).

## Examples

**Isothermal process (constant temperature):** If a gas absorbs 1000 J of heat and does 1000 J of work expanding, then $\\Delta U = 1000 - 1000 = 0$. The internal energy does not change — consistent with constant temperature for an ideal gas.

**Adiabatic compression:** If no heat is exchanged ($Q = 0$) but 500 J of work is done **on** the gas ($W = -500$), then $\\Delta U = 0 - (-500) = 500\\,\\text{J}$. The gas heats up.

## Your Task

Implement the two functions below using the First Law equations.
`,
  starterCode: `def internal_energy_change(Q, W):
    # Return the change in internal energy given heat Q added and work W done by the system
    pass

def work_by_expansion(P, delta_V):
    # Return the work done by a gas expanding at constant pressure P by volume delta_V
    pass
`,
  solution: `def internal_energy_change(Q, W):
    return Q - W

def work_by_expansion(P, delta_V):
    return P * delta_V
`,
  tests: [
    {
      name: "ΔU = 500J heat, 200J work = 300J",
      code: `{{FUNC}}
print(internal_energy_change(500, 200))`,
      expected: "300\n",
    },
    {
      name: "ΔU = 0 when Q = W",
      code: `{{FUNC}}
print(internal_energy_change(1000, 1000))`,
      expected: "0\n",
    },
    {
      name: "W = PΔV: 101325 Pa × 0.001 m³ = 101.325 J",
      code: `{{FUNC}}
print(round(work_by_expansion(101325, 0.001), 3))`,
      expected: "101.325\n",
    },
    {
      name: "ΔU with heat loss and compression",
      code: `{{FUNC}}
print(internal_energy_change(-200, -500))`,
      expected: "300\n",
    },
  ],
};
