import type { Lesson } from "../../types";

export const gibbsEnergy: Lesson = {
  id: "gibbs-energy",
  title: "Gibbs Free Energy",
  chapterId: "processes",
  content: `# Gibbs Free Energy

**Gibbs free energy** $G$ is the thermodynamic potential that determines whether a process is spontaneous at constant temperature and pressure:

$$G = H - TS$$
$$\\Delta G = \\Delta H - T\\Delta S$$

## Spontaneity Criterion

| $\\Delta G$ | Meaning |
|---|---|
| $\\Delta G < 0$ | Spontaneous (process proceeds forward) |
| $\\Delta G = 0$ | Equilibrium |
| $\\Delta G > 0$ | Non-spontaneous (requires energy input) |

Notice that a process can be spontaneous even if $\\Delta H > 0$ (endothermic), provided the entropy gain $T\\Delta S$ is large enough.

## Relation to the Equilibrium Constant

At standard conditions, the standard Gibbs energy change $\\Delta G^\\circ$ is related to the equilibrium constant $K$ by:

$$\\Delta G^\\circ = -RT\\ln K \\implies K = e^{-\\Delta G^\\circ/(RT)}$$

A large negative $\\Delta G^\\circ$ gives $K \\gg 1$ (products strongly favoured).

## Helmholtz Free Energy

At constant **volume** (rather than pressure), the relevant potential is the Helmholtz free energy:

$$\\Delta A = \\Delta U - T\\Delta S$$

## Your Task

Implement the three functions below.
`,
  starterCode: `import math

R = 8.314

def gibbs_free_energy(delta_H, T, delta_S):
    # Gibbs free energy change
    # delta_H: enthalpy change (J), T: temperature (K), delta_S: entropy change (J/K)
    pass

def is_spontaneous(delta_G):
    # Return True if the process is spontaneous (delta_G < 0)
    pass

def equilibrium_constant(delta_G0, T):
    # Equilibrium constant from standard Gibbs energy change
    # delta_G0: standard Gibbs energy change (J/mol), T: temperature (K)
    pass
`,
  solution: `import math

R = 8.314

def gibbs_free_energy(delta_H, T, delta_S):
    return delta_H - T * delta_S

def is_spontaneous(delta_G):
    return delta_G < 0

def equilibrium_constant(delta_G0, T):
    R = 8.314
    return math.exp(-delta_G0 / (R * T))
`,
  tests: [
    {
      name: "ΔG for exothermic reaction: ΔH=-100 kJ, T=298 K, ΔS=100 J/K",
      code: `{{FUNC}}
print(round(gibbs_free_energy(-100000, 298, 100), 1))`,
      expected: "-129800\n",
    },
    {
      name: "Endothermic reaction ΔH=50 kJ at 298 K with ΔS=100 J/K is not spontaneous",
      code: `{{FUNC}}
print(is_spontaneous(gibbs_free_energy(50000, 298, 100)))`,
      expected: "False\n",
    },
    {
      name: "Exothermic reaction ΔH=-50 kJ at 298 K with ΔS=100 J/K is spontaneous",
      code: `{{FUNC}}
print(is_spontaneous(gibbs_free_energy(-50000, 298, 100)))`,
      expected: "True\n",
    },
    {
      name: "Equilibrium constant: ΔG°=-5000 J/mol at 298 K",
      code: `{{FUNC}}
print(round(equilibrium_constant(-5000, 298), 2))`,
      expected: "7.52\n",
    },
  ],
};
