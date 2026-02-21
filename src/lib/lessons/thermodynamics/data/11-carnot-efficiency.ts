import type { Lesson } from "../../types";

export const carnotEfficiency: Lesson = {
  id: "carnot-efficiency",
  title: "Carnot Efficiency",
  chapterId: "cycles",
  content: `## Carnot Efficiency

The Carnot engine is the most efficient possible heat engine operating between two temperature reservoirs. Its efficiency depends **only** on the temperatures of those reservoirs:

$$\\eta_{\\text{Carnot}} = 1 - \\frac{T_c}{T_h}$$

where $T_h$ is the hot reservoir temperature and $T_c$ is the cold reservoir temperature, both measured in **Kelvin**.

This result follows directly from the Second Law of Thermodynamics: no real engine can exceed Carnot efficiency. As $T_c \\to 0$ the efficiency approaches 1, but absolute zero is unattainable (Third Law). As $T_c \\to T_h$ the efficiency approaches 0 — you cannot extract net work when both reservoirs are at the same temperature.

### Coefficient of Performance (COP)

A refrigerator is a heat engine run in reverse: it uses work input to move heat from a cold reservoir to a hot reservoir. The Carnot COP for a refrigerator is:

$$\\text{COP}_{\\text{refrigerator}} = \\frac{Q_c}{W} = \\frac{T_c}{T_h - T_c}$$

A high COP means the refrigerator is very efficient — it removes a lot of heat per unit of work input. Note that when $T_c$ is close to $T_h$, the COP is large (easy to pump heat across a small temperature difference), and when the gap is large the COP is small.

### Key Insight

The Carnot efficiency sets an upper bound. Real engines (Otto, Diesel, Rankine) are always less efficient due to irreversibilities such as friction, heat transfer across finite temperature differences, and non-quasi-static processes.
`,
  starterCode: `def carnot_efficiency(T_hot, T_cold):
    # Return the Carnot efficiency given T_hot and T_cold in Kelvin
    pass

def carnot_cop_refrigerator(T_hot, T_cold):
    # Return the Carnot COP for a refrigerator
    pass
`,
  solution: `def carnot_efficiency(T_hot, T_cold):
    return 1 - T_cold / T_hot

def carnot_cop_refrigerator(T_hot, T_cold):
    return T_cold / (T_hot - T_cold)
`,
  tests: [
    {
      name: "Carnot efficiency: T_hot=500K, T_cold=300K → η=0.4",
      code: `{{FUNC}}
print(carnot_efficiency(500, 300))`,
      expected: "0.4\n",
    },
    {
      name: "Carnot efficiency: T_hot=800K, T_cold=300K → η=0.625",
      code: `{{FUNC}}
print(carnot_efficiency(800, 300))`,
      expected: "0.625\n",
    },
    {
      name: "COP refrigerator: T_hot=300K, T_cold=250K → 5.0",
      code: `{{FUNC}}
print(carnot_cop_refrigerator(300, 250))`,
      expected: "5.0\n",
    },
    {
      name: "Closer temperatures → lower efficiency",
      code: `{{FUNC}}
print(round(carnot_efficiency(310, 300), 4))`,
      expected: "0.0323\n",
    },
  ],
};
