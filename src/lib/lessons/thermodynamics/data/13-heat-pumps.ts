import type { Lesson } from "../../types";

export const heatPumps: Lesson = {
  id: "heat-pumps",
  title: "Heat Pumps and Refrigerators",
  chapterId: "cycles",
  content: `## Heat Pumps and Refrigerators

A heat pump is a device that transfers heat from a cold reservoir to a hot reservoir — the opposite of the natural direction of heat flow — by consuming work. Both refrigerators and heating heat pumps operate on this principle; they differ only in which heat transfer is the desired output.

### Refrigerator

A refrigerator's goal is to remove heat $Q_c$ from the cold space (e.g., the inside of a freezer). The Carnot COP for a refrigerator is:

$$\\text{COP}_{\\text{ref}} = \\frac{Q_c}{W} = \\frac{T_c}{T_h - T_c}$$

A higher COP means less work is needed per joule of heat removed. When $T_c$ is close to $T_h$ (small temperature difference), the COP is large.

### Heating Heat Pump

A heating heat pump's goal is to deliver heat $Q_h$ to the hot space (e.g., the interior of a building). The Carnot COP is:

$$\\text{COP}_{\\text{hp}} = \\frac{Q_h}{W} = \\frac{T_h}{T_h - T_c}$$

### Key Relationship

The two COPs are always related by:

$$\\text{COP}_{\\text{hp}} = \\text{COP}_{\\text{ref}} + 1$$

This follows from energy conservation: $Q_h = Q_c + W$, so $Q_h/W = Q_c/W + 1$.

A heat pump with $\\text{COP}_{\\text{hp}} = 4$ delivers 4 J of heat for every 1 J of electrical work consumed — far more efficient than a simple resistive heater (which has COP = 1).

### Computing Heat Transferred

Given the COP and work input $W$:

$$Q = \\text{COP} \\times W$$
`,
  starterCode: `def cop_refrigerator(T_hot, T_cold):
    # Return the Carnot COP for a refrigerator
    pass

def cop_heat_pump(T_hot, T_cold):
    # Return the Carnot COP for a heat pump
    pass

def heat_pumped(cop, W):
    # Return the heat transferred given COP and work input W
    pass
`,
  solution: `def cop_refrigerator(T_hot, T_cold):
    return T_cold / (T_hot - T_cold)

def cop_heat_pump(T_hot, T_cold):
    return T_hot / (T_hot - T_cold)

def heat_pumped(cop, W):
    return cop * W
`,
  tests: [
    {
      name: "COP_ref: T_hot=300K, T_cold=250K → 5.0",
      code: `{{FUNC}}
print(cop_refrigerator(300, 250))`,
      expected: "5.0\n",
    },
    {
      name: "COP_hp: T_hot=300K, T_cold=250K → 6.0",
      code: `{{FUNC}}
print(cop_heat_pump(300, 250))`,
      expected: "6.0\n",
    },
    {
      name: "COP_hp = COP_ref + 1",
      code: `{{FUNC}}
print(cop_heat_pump(400, 300) - cop_refrigerator(400, 300))`,
      expected: "1.0\n",
    },
    {
      name: "Heat pump delivers Q_h = COP × W: COP=4, W=500J",
      code: `{{FUNC}}
print(heat_pumped(4, 500))`,
      expected: "2000\n",
    },
  ],
};
