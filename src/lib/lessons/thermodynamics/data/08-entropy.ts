import type { Lesson } from "../../types";

export const entropyChange: Lesson = {
  id: "entropy-change",
  title: "Entropy and the Second Law",
  chapterId: "processes",
  content: `# Entropy and the Second Law

**Entropy** ($S$) is a state function measuring the dispersal of energy — loosely, the "disorder" of a system. For any reversible process:

$$dS = \\frac{dQ_{\\text{rev}}}{T} \\implies \\Delta S = \\int \\frac{dQ_{\\text{rev}}}{T}$$

## Isothermal Entropy Change

At constant temperature, the integral simplifies to:

$$\\Delta S = \\frac{Q}{T}$$

## Entropy Change of an Ideal Gas

For a general process taking an ideal gas from $(T_1, V_1)$ to $(T_2, V_2)$:

$$\\Delta S = nC_v \\ln\\!\\frac{T_2}{T_1} + nR\\ln\\!\\frac{V_2}{V_1}$$

The first term captures the temperature change; the second captures the volume change.

## Phase Transitions

At a phase transition (e.g. melting or boiling) the process is isothermal at temperature $T$:

$$\\Delta S = \\frac{\\Delta H}{T}$$

where $\\Delta H$ is the enthalpy of the transition (latent heat per mole).

## The Second Law

The total entropy of an **isolated** system never decreases:

$$\\Delta S_{\\text{universe}} \\geq 0$$

Equality holds for reversible processes; strict inequality holds for irreversible ones.

## Your Task

Implement the three functions below.
`,
  starterCode: `import math

R = 8.314

def entropy_isothermal(Q, T):
    # Entropy change for an isothermal process
    # Q: heat added to the system (J), T: temperature (K)
    pass

def entropy_ideal_gas(n, Cv, T1, T2, V1, V2):
    # Entropy change of n moles of ideal gas
    # Cv: molar heat capacity at constant volume (J/mol/K)
    # T1/T2: initial/final temperatures (K), V1/V2: volumes (m^3)
    pass

def entropy_phase_transition(delta_H, T):
    # Entropy change during a phase transition
    # delta_H: enthalpy of transition (J/mol), T: transition temperature (K)
    pass
`,
  solution: `import math

R = 8.314

def entropy_isothermal(Q, T):
    return Q / T

def entropy_ideal_gas(n, Cv, T1, T2, V1, V2):
    R = 8.314
    return n * Cv * math.log(T2 / T1) + n * R * math.log(V2 / V1)

def entropy_phase_transition(delta_H, T):
    return delta_H / T
`,
  tests: [
    {
      name: "Isothermal: Q=1000 J at T=300 K → ΔS = 3.333 J/K",
      code: `{{FUNC}}
print(round(entropy_isothermal(1000, 300), 3))`,
      expected: "3.333\n",
    },
    {
      name: "Ideal gas: n=1, Cv=12.47 J/mol/K, temperature doubles, volume doubles",
      code: `{{FUNC}}
print(round(entropy_ideal_gas(1, 12.47, 300, 600, 1.0, 2.0), 4))`,
      expected: "14.4064\n",
    },
    {
      name: "Ideal gas isothermal expansion: T unchanged, volume doubles",
      code: `{{FUNC}}
print(round(entropy_ideal_gas(1, 12.47, 300, 300, 1.0, 2.0), 4))`,
      expected: "5.7628\n",
    },
    {
      name: "Water melting: ΔH=6010 J/mol at T=273.15 K",
      code: `{{FUNC}}
print(round(entropy_phase_transition(6010, 273.15), 2))`,
      expected: "22.0\n",
    },
  ],
};
