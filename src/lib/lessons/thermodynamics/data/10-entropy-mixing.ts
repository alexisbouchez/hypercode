import type { Lesson } from "../../types";

export const entropyMixing: Lesson = {
  id: "entropy-mixing",
  title: "Entropy of Mixing",
  chapterId: "processes",
  content: `# Entropy of Mixing

When two different ideal gases mix, entropy increases **even without any heat exchange**. This is called the **entropy of mixing** and arises purely from the increased number of accessible microstates.

## Formula

For a mixture of $k$ ideal gas components:

$$\\Delta S_{\\text{mix}} = -nR\\sum_{i} x_i \\ln x_i$$

where $x_i = n_i / n_{\\text{total}}$ is the mole fraction of species $i$ and $n = \\sum_i n_i$ is the total number of moles. For two gases:

$$\\Delta S_{\\text{mix}} = -R\\!\\left(n_1 \\ln x_1 + n_2 \\ln x_2\\right)$$

Since $0 < x_i < 1$, each $\\ln x_i < 0$, so $\\Delta S_{\\text{mix}} > 0$ always — mixing is always spontaneous for ideal gases.

## Maximum Mixing Entropy

$\\Delta S_{\\text{mix}}$ is maximised when all mole fractions are equal ($x_i = 1/k$). For two gases this means $x_1 = x_2 = 0.5$.

## Boltzmann's Entropy Formula

Ludwig Boltzmann connected macroscopic entropy to the microscopic world:

$$S = k_B \\ln \\Omega$$

where $k_B = 1.381 \\times 10^{-23}\\,\\text{J/K}$ is the Boltzmann constant and $\\Omega$ is the number of **microstates** consistent with the macroscopic state. More microstates $\\Rightarrow$ higher entropy.

## Your Task

Implement the two functions below.
`,
  starterCode: `import math

R = 8.314
k_B = 1.381e-23

def entropy_of_mixing(n1, n2):
    # Entropy of mixing two ideal gases with n1 and n2 moles (J/K)
    pass

def boltzmann_entropy(omega):
    # Boltzmann entropy for omega microstates (J/K)
    pass
`,
  solution: `import math

R = 8.314
k_B = 1.381e-23

def entropy_of_mixing(n1, n2):
    R = 8.314
    n_total = n1 + n2
    x1 = n1 / n_total
    x2 = n2 / n_total
    return -R * (n1 * math.log(x1) + n2 * math.log(x2))

def boltzmann_entropy(omega):
    k_B = 1.381e-23
    return k_B * math.log(omega)
`,
  tests: [
    {
      name: "Equal moles (1+1): ΔS_mix = 2R·ln(2)",
      code: `{{FUNC}}
print(round(entropy_of_mixing(1, 1), 2))`,
      expected: "11.53\n",
    },
    {
      name: "Unequal moles (1+3): mixing entropy is less than maximum",
      code: `{{FUNC}}
print(round(entropy_of_mixing(1, 3), 2))`,
      expected: "18.7\n",
    },
    {
      name: "Boltzmann: for Ω = e^100, S = 100·k_B",
      code: `{{FUNC}}
print(round(boltzmann_entropy(math.exp(100)) / 1.381e-23, 0))`,
      expected: "100.0\n",
    },
    {
      name: "Double moles (2+2): mixing entropy scales with total moles",
      code: `{{FUNC}}
print(round(entropy_of_mixing(2, 2), 2))`,
      expected: "23.05\n",
    },
  ],
};
