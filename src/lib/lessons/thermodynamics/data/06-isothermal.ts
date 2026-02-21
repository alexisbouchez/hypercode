import type { Lesson } from "../../types";

export const isothermalProcess: Lesson = {
  id: "isothermal-process",
  title: "Isothermal Process",
  chapterId: "processes",
  content: `# Isothermal Process

An isothermal process occurs at **constant temperature**. For an ideal gas, since $T$ is constant, the product $PV$ is also constant:

$$PV = nRT = \\text{const}$$

## Work Done in an Isothermal Process

When a gas expands isothermally from volume $V_1$ to $V_2$, the work done **by** the gas is found by integrating $P\\,dV$ with $P = nRT/V$:

$$W = \\int_{V_1}^{V_2} P\\,dV = nRT \\int_{V_1}^{V_2} \\frac{dV}{V} = nRT \\ln\\!\\left(\\frac{V_2}{V_1}\\right) = P_1 V_1 \\ln\\!\\left(\\frac{V_2}{V_1}\\right)$$

If $V_2 > V_1$ (expansion), $W > 0$ — the gas does positive work. If $V_2 < V_1$ (compression), $W < 0$.

## Heat and Internal Energy

For an ideal gas, internal energy depends only on temperature. Since $T$ is constant:

$$\\Delta U = 0 \\implies Q = W$$

All heat absorbed from the surroundings is converted entirely into work.

## Boyle's Law

At constant temperature, pressure and volume are inversely proportional:

$$P_1 V_1 = P_2 V_2 \\implies P_2 = \\frac{P_1 V_1}{V_2}$$

So doubling the volume halves the pressure — and vice versa.

## Your Task

Implement the two functions below.
`,
  starterCode: `import math

R = 8.314

def isothermal_work(n, T, V1, V2):
    # Work done by the gas during isothermal expansion/compression
    # n: moles, T: temperature (K), V1/V2: initial/final volumes (m^3)
    pass

def boyles_pressure(P1, V1, V2):
    # Final pressure after isothermal change from V1 to V2
    # P1: initial pressure (Pa), V1: initial volume, V2: final volume
    pass
`,
  solution: `import math

R = 8.314

def isothermal_work(n, T, V1, V2):
    R = 8.314
    return n * R * T * math.log(V2 / V1)

def boyles_pressure(P1, V1, V2):
    return P1 * V1 / V2
`,
  tests: [
    {
      name: "n=1 mol, T=300 K, volume doubles: W = RT·ln(2)",
      code: `{{FUNC}}
print(round(isothermal_work(1, 300, 1.0, 2.0), 2))`,
      expected: "1728.85\n",
    },
    {
      name: "n=2 mol, T=400 K, V1=0.01 m³ → V2=0.05 m³",
      code: `{{FUNC}}
print(round(isothermal_work(2, 400, 0.01, 0.05), 2))`,
      expected: "10704.69\n",
    },
    {
      name: "Boyle's law: P1=100 kPa, V1=2 L → V2=4 L gives P2=50 kPa",
      code: `{{FUNC}}
print(boyles_pressure(100000, 2, 4))`,
      expected: "50000.0\n",
    },
    {
      name: "Isothermal compression: n=1, T=300 K, V halves → W is negative",
      code: `{{FUNC}}
print(round(isothermal_work(1, 300, 2.0, 1.0), 2))`,
      expected: "-1728.85\n",
    },
  ],
};
