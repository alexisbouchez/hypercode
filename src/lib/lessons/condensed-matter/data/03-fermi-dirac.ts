import type { Lesson } from "../../types";

export const fermiDirac: Lesson = {
  id: "fermi-dirac",
  title: "Fermi-Dirac Distribution",
  chapterId: "crystal-structure",
  content: `## Fermi-Dirac Distribution

The **Fermi-Dirac distribution** gives the probability that a quantum state with energy E is occupied by an electron at temperature T:

$$f(E) = \\frac{1}{\\exp\\left(\\frac{E - \\mu}{k_B T}\\right) + 1}$$

Where:
- **μ** = chemical potential (≈ E_F at low temperatures)
- **k_B** = Boltzmann constant = 1.381 × 10⁻²³ J/K
- **T** = temperature in Kelvin

### Key Behaviors

- At **T = 0**: f = 1 for E < E_F, f = 0 for E > E_F (sharp step)
- At **T > 0**: thermal smearing occurs over an energy range ~k_B T around E_F
- At **E = μ**: f = 0.5 always (regardless of temperature)

### Density of States

The **density of states** g(E) counts available quantum states per unit energy per unit volume. For free electrons:

$$g(E) = \\frac{1}{2\\pi^2} \\left(\\frac{2m_e}{\\hbar^2}\\right)^{3/2} \\sqrt{E}$$

In units of J⁻¹·m⁻³. The electron density is:

$$n = \\int_0^{E_F} g(E)\\, dE$$

### Physical Constants (inside your functions)
- k_B = 1.381 × 10⁻²³ J/K
- ħ = 1.055 × 10⁻³⁴ J·s
- m_e = 9.109 × 10⁻³¹ kg
- 1 eV = 1.602 × 10⁻¹⁹ J
`,
  starterCode: `import math

def fermi_dirac(E_J, mu_J, T_K):
    pass

def fermi_dirac_eV(E_eV, mu_eV, T_K):
    pass

def dos_free_electron_per_J_m3(E_J):
    pass
`,
  solution: `import math

def fermi_dirac(E_J, mu_J, T_K):
    k_B = 1.381e-23
    if T_K == 0:
        return 1.0 if E_J < mu_J else 0.0
    x = (E_J - mu_J) / (k_B * T_K)
    return 1.0 / (math.exp(x) + 1)

def fermi_dirac_eV(E_eV, mu_eV, T_K):
    eV = 1.602e-19
    return fermi_dirac(E_eV * eV, mu_eV * eV, T_K)

def dos_free_electron_per_J_m3(E_J):
    hbar = 1.055e-34
    m_e = 9.109e-31
    return (1 / (2 * math.pi**2)) * (2 * m_e / hbar**2)**(3/2) * math.sqrt(E_J)
`,
  tests: [
    {
      name: "Fermi-Dirac at E=μ (always 0.5)",
      expected: "0.5000\n",
      code: `{{FUNC}}
print(f"{fermi_dirac_eV(7.04, 7.04, 300):.4f}")`,
    },
    {
      name: "Fermi-Dirac 0.5 eV above chemical potential",
      expected: "4.0126e-09\n",
      code: `{{FUNC}}
print(f"{fermi_dirac_eV(7.04 + 0.5, 7.04, 300):.4e}")`,
    },
    {
      name: "Fermi-Dirac 0.5 eV below chemical potential",
      expected: "1.0000\n",
      code: `{{FUNC}}
print(f"{fermi_dirac_eV(7.04 - 0.5, 7.04, 300):.4f}")`,
    },
    {
      name: "Free electron DOS at copper Fermi energy",
      expected: "1.1266e+47\n",
      code: `{{FUNC}}
print(f"{dos_free_electron_per_J_m3(7.04 * 1.602e-19):.4e}")`,
    },
  ],
};
