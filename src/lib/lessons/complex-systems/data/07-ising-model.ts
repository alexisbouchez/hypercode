import type { Lesson } from "../../types";

export const isingModel: Lesson = {
  id: "ising-model",
  title: "Ising Model",
  chapterId: "statistical-physics",
  content: `# Ising Model

The **Ising model** is the workhorse of statistical mechanics — a minimal model of ferromagnetism that reveals deep truths about phase transitions and collective behaviour.

## Setup

Consider a 1D chain of $N$ spins, each taking the value $s_i \\in \\{+1, -1\\}$ (spin-up or spin-down). Spins interact with their nearest neighbours, and the system uses **periodic boundary conditions** ($s_{N} \\equiv s_0$).

## Energy

The Hamiltonian (total energy) is:

$$E = -J \\sum_{i=0}^{N-1} s_i \\, s_{i+1}$$

- $J > 0$: ferromagnetic (aligned spins are favoured, lowering energy)
- $J < 0$: antiferromagnetic (anti-aligned spins are favoured)

## Magnetisation

The **magnetisation per spin** measures the average alignment:

$$M = \\frac{1}{N} \\sum_{i=0}^{N-1} s_i$$

$M = +1$ is fully aligned up, $M = -1$ is fully aligned down, $M = 0$ is disordered.

## Analytical Results (1D)

In one dimension, there is no phase transition at finite temperature. The analytical energy per spin is:

$$\\langle e \\rangle = -J \\tanh\\!\\left(\\frac{J}{k_B T}\\right)$$

(setting $k_B = 1$ for simplicity).

## Metropolis Algorithm

The Metropolis Monte Carlo algorithm samples spin configurations at temperature $T$:

1. Pick a random spin $s_i$
2. Compute the energy change $\\Delta E$ if $s_i$ is flipped
3. Accept the flip with probability $\\min(1,\\, e^{-\\Delta E / T})$

The **acceptance probability** as a standalone function is:

$$A(\\Delta E, T) = \\begin{cases} 1 & \\text{if } \\Delta E \\leq 0 \\\\ e^{-\\Delta E / T} & \\text{if } \\Delta E > 0 \\end{cases}$$

## Your Task

Implement four functions:
- \`ising_energy(spins, J=1.0)\` — compute total energy with periodic boundary conditions
- \`ising_magnetization(spins)\` — compute magnetisation per spin
- \`ising_energy_analytical(T_K, J=1.0)\` — return analytical energy per spin ($k_B = 1$)
- \`metropolis_acceptance(delta_E, T_K)\` — return the Metropolis acceptance probability
`,
  starterCode: `import math

def ising_energy(spins, J=1.0):
    pass

def ising_magnetization(spins):
    pass

def ising_energy_analytical(T_K, J=1.0):
    pass

def metropolis_acceptance(delta_E, T_K):
    pass
`,
  solution: `import math

def ising_energy(spins, J=1.0):
    N = len(spins)
    E = 0.0
    for i in range(N):
        E += spins[i] * spins[(i + 1) % N]
    return -J * E

def ising_magnetization(spins):
    return sum(spins) / len(spins)

def ising_energy_analytical(T_K, J=1.0):
    return -J * math.tanh(J / T_K)

def metropolis_acceptance(delta_E, T_K):
    if delta_E <= 0:
        return 1.0
    return math.exp(-delta_E / T_K)
`,
  tests: [
    {
      name: "ising_energy([1,1,-1,1,-1]) equals 3.0",
      expected: "3.0000\n",
      code: `{{FUNC}}
print(f"{ising_energy([1, 1, -1, 1, -1], 1.0):.4f}")`,
    },
    {
      name: "ising_magnetization([1,1,-1,1,-1]) equals 0.2",
      expected: "0.2000\n",
      code: `{{FUNC}}
print(f"{ising_magnetization([1, 1, -1, 1, -1]):.4f}")`,
    },
    {
      name: "ising_energy_analytical(1.0, 1.0) equals -tanh(1)",
      expected: "-0.7616\n",
      code: `{{FUNC}}
print(f"{ising_energy_analytical(1.0, 1.0):.4f}")`,
    },
    {
      name: "metropolis_acceptance with delta_E < 0 returns 1.0",
      expected: "1.0000\n",
      code: `{{FUNC}}
print(f"{metropolis_acceptance(-1.0, 1.0):.4f}")`,
    },
  ],
};
