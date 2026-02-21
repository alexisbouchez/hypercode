import type { Lesson } from "../../types";

export const carnotCycle: Lesson = {
  id: "carnot-cycle",
  title: "The Carnot Cycle",
  chapterId: "cycles",
  content: `## The Carnot Cycle

A complete Carnot cycle consists of four reversible steps executed on a working substance (typically an ideal gas):

1. **Isothermal expansion** at $T_h$: the gas expands slowly while absorbing heat $Q_h$ from the hot reservoir. Temperature is held constant.
2. **Adiabatic expansion**: the gas continues to expand with no heat exchange. Temperature drops from $T_h$ to $T_c$.
3. **Isothermal compression** at $T_c$: the gas is compressed slowly while rejecting heat $Q_c$ to the cold reservoir.
4. **Adiabatic compression**: the gas is compressed with no heat exchange. Temperature rises from $T_c$ back to $T_h$.

### Energy Relations

The net work output equals the difference between heat absorbed and heat rejected:

$$W_{\\text{net}} = Q_h - Q_c$$

The efficiency relates these quantities:

$$\\eta = \\frac{W_{\\text{net}}}{Q_h} = 1 - \\frac{T_c}{T_h}$$

A fundamental result of the Carnot cycle is that the heat ratio equals the temperature ratio:

$$\\frac{Q_c}{Q_h} = \\frac{T_c}{T_h}$$

This allows us to compute the heat rejected and work output given only $Q_h$ and the two reservoir temperatures:

$$W = Q_h \\left(1 - \\frac{T_c}{T_h}\\right), \\qquad Q_c = Q_h \\cdot \\frac{T_c}{T_h}$$

### Energy Conservation Check

By construction, $W + Q_c = Q_h$, confirming the First Law for the cycle (the working fluid returns to its original state after each complete cycle, so $\\Delta U = 0$).
`,
  starterCode: `def carnot_work(Q_h, T_hot, T_cold):
    # Return the net work output of a Carnot engine
    pass

def carnot_heat_rejected(Q_h, T_hot, T_cold):
    # Return the heat rejected to the cold reservoir
    pass
`,
  solution: `def carnot_work(Q_h, T_hot, T_cold):
    eta = 1 - T_cold / T_hot
    return eta * Q_h

def carnot_heat_rejected(Q_h, T_hot, T_cold):
    return Q_h * T_cold / T_hot
`,
  tests: [
    {
      name: "Work output: Q_h=1000J, T_hot=500K, T_cold=300K → 400J",
      code: `{{FUNC}}
print(carnot_work(1000, 500, 300))`,
      expected: "400.0\n",
    },
    {
      name: "Heat rejected: Q_h=1000J, T_hot=500K, T_cold=300K → 600J",
      code: `{{FUNC}}
print(carnot_heat_rejected(1000, 500, 300))`,
      expected: "600.0\n",
    },
    {
      name: "Energy conservation: W + Q_c = Q_h (Q_h=2000J, T_hot=800K, T_cold=300K)",
      code: `{{FUNC}}
print(carnot_work(2000, 800, 300) + carnot_heat_rejected(2000, 800, 300))`,
      expected: "2000.0\n",
    },
    {
      name: "Work output: Q_h=5000J, T_hot=1000K, T_cold=300K → 3500J",
      code: `{{FUNC}}
print(carnot_work(5000, 1000, 300))`,
      expected: "3500.0\n",
    },
  ],
};
