import type { Lesson } from "../../types";

export const idealGas: Lesson = {
  id: "ideal-gas",
  title: "The Ideal Gas Law",
  chapterId: "laws",
  content: `# The Ideal Gas Law

The ideal gas law combines Boyle's Law, Charles's Law, and Avogadro's Law into a single equation that describes the state of an ideal gas — one in which molecules have no volume and no intermolecular forces.

## The Equation

$$PV = nRT$$

Where:
- $P$ is the **pressure** (Pa = N/m²)
- $V$ is the **volume** (m³)
- $n$ is the **amount** of gas (mol)
- $R = 8.314\\,\\text{J/(mol·K)}$ is the **universal gas constant**
- $T$ is the **absolute temperature** (K)

## Three Rearrangements

Solving for each variable:

$$P = \\frac{nRT}{V}$$

$$V = \\frac{nRT}{P}$$

$$T = \\frac{PV}{nR}$$

## Standard Conditions

At **standard temperature and pressure** (STP: $T = 273.15\\,\\text{K}$, $P = 101325\\,\\text{Pa}$), one mole of an ideal gas occupies approximately $22.4\\,\\text{L} = 0.0224\\,\\text{m}^3$.

This is the **molar volume** of an ideal gas at STP, a useful benchmark for checking calculations.

## Limits of the Ideal Gas Model

Real gases deviate from ideal behavior at:
- **High pressure** — molecules are compressed close enough that their volume matters
- **Low temperature** — intermolecular attractions become significant

The van der Waals equation corrects for these effects:

$$\\left(P + \\frac{an^2}{V^2}\\right)(V - nb) = nRT$$

Where $a$ accounts for intermolecular attractions and $b$ accounts for molecular volume. For most introductory problems, the ideal gas law is an excellent approximation.

## Your Task

Implement the three functions below using $R = 8.314\\,\\text{J/(mol·K)}$.
`,
  starterCode: `R = 8.314  # Universal gas constant in J/(mol·K)

def ideal_gas_pressure(n, T, V):
    # Return the pressure of n moles of gas at temperature T in volume V
    pass

def ideal_gas_volume(n, T, P):
    # Return the volume of n moles of gas at temperature T and pressure P
    pass

def ideal_gas_temperature(P, V, n):
    # Return the temperature of n moles of gas at pressure P in volume V
    pass
`,
  solution: `R = 8.314

def ideal_gas_pressure(n, T, V):
    R = 8.314
    return n * R * T / V

def ideal_gas_volume(n, T, P):
    R = 8.314
    return n * R * T / P

def ideal_gas_temperature(P, V, n):
    R = 8.314
    return P * V / (n * R)
`,
  tests: [
    {
      name: "P: n=2, T=300K, V=0.05m³",
      code: `{{FUNC}}
print(round(ideal_gas_pressure(2, 300, 0.05), 1))`,
      expected: "99768.0\n",
    },
    {
      name: "V: n=1, T=273.15K, P=101325Pa (molar volume at STP)",
      code: `{{FUNC}}
print(round(ideal_gas_volume(1, 273.15, 101325), 5))`,
      expected: "0.02241\n",
    },
    {
      name: "T: P=101325, V=0.02241, n=1",
      code: `{{FUNC}}
print(round(ideal_gas_temperature(101325, 0.02241, 1), 1))`,
      expected: "273.1\n",
    },
    {
      name: "P: n=0.5, T=500K, V=0.02m³",
      code: `{{FUNC}}
print(round(ideal_gas_pressure(0.5, 500, 0.02), 1))`,
      expected: "103925.0\n",
    },
  ],
};
