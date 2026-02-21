import type { Lesson } from "../../types";

export const perturbationTheory: Lesson = {
  id: "perturbation-theory",
  title: "Perturbation Theory",
  chapterId: "quantum-methods",
  content: `# Perturbation Theory

**Perturbation theory** lets us find approximate solutions to quantum systems that are "close" to a solvable one. We write the Hamiltonian as:

$$H = H_0 + \\lambda H'$$

where $H_0$ has known solutions $H_0 |n\\rangle = E_n^{(0)} |n\\rangle$ and $\\lambda H'$ is a small perturbation.

## First-Order Energy Correction

The first-order correction to the energy of state $|n\\rangle$ is simply the expectation value of the perturbation:

$$E_n^{(1)} = \\langle n | H' | n \\rangle = \\int \\psi_n^*(x)\\, H'(x)\\, \\psi_n(x)\\, dx$$

## Particle in a Box

The **1D particle in a box** (infinite square well) on $[0, L]$ is a classic exactly solvable system:

$$\\psi_n(x) = \\sqrt{\\frac{2}{L}} \\sin\\!\\left(\\frac{n\\pi x}{L}\\right), \\quad E_n^{(0)} = \\frac{n^2 \\pi^2 \\hbar^2}{2 m L^2}$$

with $n = 1, 2, 3, \\ldots$ (quantum number starts at 1).

### Common Perturbations

- **Constant perturbation** $H' = V_0$: correction is $E_n^{(1)} = V_0$ for all $n$ (the wavefunctions are already orthonormal and $\\int \\psi_n^2 = 1$).
- **Linear ramp** $H' = V_0 x/L$: correction is $E_n^{(1)} = V_0/2$ for all $n$ (by symmetry of the sin² integral).

## Physical Constants

Using SI units with $\\hbar = 1.055 \\times 10^{-34}$ J·s, $m_e = 9.109 \\times 10^{-31}$ kg, and $1\\,\\text{eV} = 1.602 \\times 10^{-19}$ J.

## Implementation

- \`pib_wavefunction(x, n, L)\` — $\\psi_n(x) = \\sqrt{2/L}\\sin(n\\pi x/L)$
- \`pib_energy_eV(n, L_nm)\` — unperturbed energy in eV for a box of width $L_{\\text{nm}}$ nanometres
- \`first_order_correction(V_func, n, L, N=1000)\` — numerically integrates $\\langle n|V|n\\rangle$ over $[0,L]$

\`\`\`python
import math

def pib_wavefunction(x, n, L):
    return math.sqrt(2.0 / L) * math.sin(n * math.pi * x / L)

def pib_energy_eV(n, L_nm):
    hbar = 1.055e-34
    m = 9.109e-31
    eV = 1.602e-19
    L = L_nm * 1e-9
    return n**2 * math.pi**2 * hbar**2 / (2 * m * L**2) / eV
\`\`\`
`,
  starterCode: `import math

def pib_wavefunction(x, n, L):
    pass

def pib_energy_eV(n, L_nm):
    pass

def first_order_correction(V_func, n, L, N=1000):
    pass
`,
  solution: `import math

def pib_wavefunction(x, n, L):
    return math.sqrt(2.0 / L) * math.sin(n * math.pi * x / L)

def pib_energy_eV(n, L_nm):
    hbar = 1.055e-34
    m = 9.109e-31
    eV = 1.602e-19
    L = L_nm * 1e-9
    return n**2 * math.pi**2 * hbar**2 / (2 * m * L**2) / eV

def first_order_correction(V_func, n, L, N=1000):
    dx = L / N
    result = 0.0
    for i in range(N):
        x = (i + 0.5) * dx
        psi = pib_wavefunction(x, n, L)
        result += psi * V_func(x) * psi * dx
    return result
`,
  tests: [
    {
      name: "pib_energy_eV(1, 1.0) ground state of 1nm box",
      expected: "0.3764\n",
      code: `{{FUNC}}\nprint(f"{pib_energy_eV(1, 1.0):.4f}")`,
    },
    {
      name: "pib_energy_eV(2, 1.0) = 4 * E_1",
      expected: "1.5056\n",
      code: `{{FUNC}}\nprint(f"{pib_energy_eV(2, 1.0):.4f}")`,
    },
    {
      name: "first_order_correction with V=1 (constant) = 1.0",
      expected: "1.0000\n",
      code: `{{FUNC}}\nprint(f"{first_order_correction(lambda x: 1.0, 1, 1.0):.4f}")`,
    },
    {
      name: "first_order_correction with V=x (linear ramp) = L/2 = 0.5",
      expected: "0.5000\n",
      code: `{{FUNC}}\nprint(f"{first_order_correction(lambda x: x, 1, 1.0):.4f}")`,
    },
  ],
};
