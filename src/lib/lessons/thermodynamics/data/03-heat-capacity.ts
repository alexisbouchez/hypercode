import type { Lesson } from "../../types";

export const heatCapacity: Lesson = {
  id: "heat-capacity",
  title: "Heat Capacity",
  chapterId: "laws",
  content: `# Heat Capacity

Heat capacity describes how much heat energy a substance must absorb or release to change its temperature by a given amount. It is central to understanding how materials store and transfer thermal energy.

## Specific Heat Capacity

The heat required to change the temperature of a mass $m$ by $\\Delta T$ is:

$$Q = mc\\Delta T$$

Where:
- $Q$ is the heat energy added (J)
- $m$ is the mass (kg)
- $c$ is the **specific heat capacity** of the material (J/kg·K)
- $\\Delta T$ is the temperature change (K or °C — they are equivalent for differences)

### Common Specific Heats

| Material | $c$ (J/kg·K) |
|---|---|
| Water | 4186 |
| Aluminium | 900 |
| Iron | 450 |
| Copper | 385 |

Water's unusually high specific heat is why it is so effective as a coolant and why coastal climates are mild.

## Molar Heat Capacity

For gases and chemical processes it is often more natural to work in moles. The molar heat capacity $C_v$ (at constant volume) gives:

$$Q = nC_v\\Delta T$$

Where $n$ is the number of moles and $C_v$ has units of J/(mol·K).

## Mayer's Relation for Ideal Gases

For an ideal gas, the molar heat capacity at constant pressure $C_p$ exceeds $C_v$ by exactly $R$, the universal gas constant:

$$C_p = C_v + R$$

Where $R = 8.314\\,\\text{J/(mol·K)}$. This extra energy accounts for the work the gas does when it expands at constant pressure.

For a monatomic ideal gas: $C_v = \\frac{3}{2}R \\approx 12.47\\,\\text{J/(mol·K)}$ and $C_p = \\frac{5}{2}R \\approx 20.79\\,\\text{J/(mol·K)}$.

## Your Task

Implement the three functions below. Use $R = 8.314\\,\\text{J/(mol·K)}$ as a module-level constant.
`,
  starterCode: `R = 8.314  # Universal gas constant in J/(mol·K)

def heat_required(mass, specific_heat, delta_T):
    # Return the heat needed to change 'mass' kg of material with 'specific_heat' by 'delta_T' degrees
    pass

def molar_heat(n, Cv, delta_T):
    # Return the heat needed for n moles with molar heat capacity Cv and temperature change delta_T
    pass

def cp_from_cv(Cv):
    # Return Cp given Cv using Mayer's relation for an ideal gas
    pass
`,
  solution: `R = 8.314

def heat_required(mass, specific_heat, delta_T):
    return mass * specific_heat * delta_T

def molar_heat(n, Cv, delta_T):
    return n * Cv * delta_T

def cp_from_cv(Cv):
    R = 8.314
    return Cv + R
`,
  tests: [
    {
      name: "1 kg water, 10°C rise: Q = 41860 J",
      code: `{{FUNC}}
print(heat_required(1.0, 4186, 10))`,
      expected: "41860.0\n",
    },
    {
      name: "0.5 kg aluminium, 50°C rise: Q = 22500 J",
      code: `{{FUNC}}
print(heat_required(0.5, 900, 50))`,
      expected: "22500.0\n",
    },
    {
      name: "2 mol at Cv=20.8, ΔT=30K: Q = 1248 J",
      code: `{{FUNC}}
print(molar_heat(2, 20.8, 30))`,
      expected: "1248.0\n",
    },
    {
      name: "Cp = Cv + R: Cv=20.786 → Cp=29.1",
      code: `{{FUNC}}
print(round(cp_from_cv(20.786), 3))`,
      expected: "29.1\n",
    },
  ],
};
