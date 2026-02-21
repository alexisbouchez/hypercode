import type { Lesson } from "../../types";

export const boltzmannDistribution: Lesson = {
  id: "boltzmann-distribution",
  title: "The Boltzmann Distribution",
  chapterId: "statistical",
  content: `## The Boltzmann Distribution

Statistical mechanics connects microscopic energy levels to macroscopic thermodynamic properties. At thermal equilibrium, the probability that a system occupies a state with energy $E$ is given by the **Boltzmann distribution**:

$$P(E) = \\frac{e^{-E/(k_B T)}}{Z}$$

where $k_B = 1.381 \\times 10^{-23}\\,\\text{J/K}$ is the Boltzmann constant, $T$ is the absolute temperature in Kelvin, and $Z$ is the **partition function**.

### Partition Function

The partition function sums the Boltzmann weights over all accessible states:

$$Z = \\sum_i e^{-E_i / (k_B T)}$$

It acts as a normalisation constant ensuring all probabilities sum to 1. It also encodes all thermodynamic information about the system — free energy, entropy, and average energy can all be derived from $Z$.

### Boltzmann Factor

The **ratio** of populations in two states with energies $E_1$ and $E_2$ is:

$$\\frac{N_2}{N_1} = e^{-(E_2 - E_1)/(k_B T)}$$

This ratio is called the Boltzmann factor. When $E_2 > E_1$, higher states are exponentially less populated. At high $T$ the populations become equal; at low $T$ the ground state dominates.

### Physical Intuition

The factor $e^{-E/(k_B T)}$ captures the competition between energy minimisation (favouring low-energy states) and thermal fluctuations (which can promote states to higher energies). The characteristic energy scale is $k_B T \\approx 4.1 \\times 10^{-21}\\,\\text{J}$ at room temperature (300 K).
`,
  starterCode: `import math

k_B = 1.381e-23

def boltzmann_factor(E1, E2, T):
    # Return the ratio N2/N1 = exp(-(E2-E1)/(k_B*T))
    pass

def partition_function(energies, T):
    # Return Z = sum of exp(-E/(k_B*T)) for each E in energies
    pass

def boltzmann_probability(E, energies, T):
    # Return the probability of the state with energy E
    pass
`,
  solution: `import math

k_B = 1.381e-23

def boltzmann_factor(E1, E2, T):
    k_B = 1.381e-23
    return math.exp(-(E2 - E1) / (k_B * T))

def partition_function(energies, T):
    k_B = 1.381e-23
    return sum(math.exp(-E / (k_B * T)) for E in energies)

def boltzmann_probability(E, energies, T):
    k_B = 1.381e-23
    Z = sum(math.exp(-ei / (k_B * T)) for ei in energies)
    return math.exp(-E / (k_B * T)) / Z
`,
  tests: [
    {
      name: "Boltzmann factor: E1=0, E2=k_B*T → ratio=e^{-1} ≈ 0.3679",
      code: `import math
{{FUNC}}
print(round(boltzmann_factor(0, 1.381e-23 * 300, 300), 4))`,
      expected: "0.3679\n",
    },
    {
      name: "Partition function: two levels at 0 and k_B*T → 1 + e^{-1} ≈ 1.3679",
      code: `import math
{{FUNC}}
print(round(partition_function([0, 1.381e-23 * 300], 300), 4))`,
      expected: "1.3679\n",
    },
    {
      name: "Probability of ground state (E=0) in two-level system ≈ 0.7311",
      code: `import math
{{FUNC}}
print(round(boltzmann_probability(0, [0, 1.381e-23 * 300], 300), 4))`,
      expected: "0.7311\n",
    },
    {
      name: "Equal energies → equal probabilities (each 0.5)",
      code: `import math
{{FUNC}}
print(round(boltzmann_probability(0, [0, 0], 300), 1))`,
      expected: "0.5\n",
    },
  ],
};
