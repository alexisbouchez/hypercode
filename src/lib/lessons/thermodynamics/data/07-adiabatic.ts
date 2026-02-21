import type { Lesson } from "../../types";

export const adiabaticProcess: Lesson = {
  id: "adiabatic-process",
  title: "Adiabatic Process",
  chapterId: "processes",
  content: `# Adiabatic Process

In an adiabatic process, **no heat is exchanged** with the surroundings ($Q = 0$). From the First Law of Thermodynamics:

$$\\Delta U = Q - W = -W$$

Any work done by the gas comes entirely at the expense of its internal energy, so the temperature changes.

## Adiabatic Relations for an Ideal Gas

The ratio of heat capacities $\\gamma = C_p / C_v$ governs adiabatic behaviour. Two useful relations:

**Temperature–volume:**
$$TV^{\\gamma-1} = \\text{const} \\implies T_2 = T_1\\!\\left(\\frac{V_1}{V_2}\\right)^{\\!\\gamma-1}$$

**Pressure–volume:**
$$PV^{\\gamma} = \\text{const} \\implies P_2 = P_1\\!\\left(\\frac{V_1}{V_2}\\right)^{\\!\\gamma}$$

## Work Done by the Gas

Since $Q = 0$, the work equals the drop in internal energy:

$$W = \\frac{P_1 V_1 - P_2 V_2}{\\gamma - 1} = \\frac{nR(T_1 - T_2)}{\\gamma - 1}$$

## Heat Capacity Ratios

| Gas type | Example | $\\gamma$ | $C_v$ |
|---|---|---|---|
| Monatomic | He, Ar | $5/3 \\approx 1.667$ | $\\tfrac{3}{2}R$ |
| Diatomic | N₂, O₂, air | $1.4$ | $\\tfrac{5}{2}R$ |

## Your Task

Implement the two functions below.
`,
  starterCode: `R = 8.314

def adiabatic_final_temp(T1, V1, V2, gamma):
    # Final temperature after adiabatic expansion/compression
    # T1: initial temperature (K), V1/V2: volumes, gamma: Cp/Cv
    pass

def adiabatic_work(n, T1, T2, gamma):
    # Work done BY the gas during an adiabatic process
    # n: moles, T1: initial temp (K), T2: final temp (K), gamma: Cp/Cv
    pass
`,
  solution: `R = 8.314

def adiabatic_final_temp(T1, V1, V2, gamma):
    return T1 * (V1 / V2) ** (gamma - 1)

def adiabatic_work(n, T1, T2, gamma):
    R = 8.314
    return n * R * (T1 - T2) / (gamma - 1)
`,
  tests: [
    {
      name: "Diatomic gas (γ=1.4): T2 when volume doubles from 300 K",
      code: `{{FUNC}}
print(round(adiabatic_final_temp(300, 1.0, 2.0, 1.4), 2))`,
      expected: "227.36\n",
    },
    {
      name: "Monatomic gas (γ=5/3): T2 when volume triples from 300 K",
      code: `{{FUNC}}
print(round(adiabatic_final_temp(300, 1.0, 3.0, 5/3), 2))`,
      expected: "144.22\n",
    },
    {
      name: "Adiabatic work: n=2 mol, diatomic, T drops from 400 K to 300 K",
      code: `{{FUNC}}
print(round(adiabatic_work(2, 400, 300, 1.4), 2))`,
      expected: "4157.0\n",
    },
    {
      name: "Adiabatic work: n=1 mol, monatomic, T drops from 500 K to 300 K",
      code: `{{FUNC}}
print(round(adiabatic_work(1, 500, 300, 5/3), 2))`,
      expected: "2494.2\n",
    },
  ],
};
