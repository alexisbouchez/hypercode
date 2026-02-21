import type { Lesson } from "../../types";

export const semiconductorCarriers: Lesson = {
  id: "semiconductor-carriers",
  title: "Semiconductor Carrier Concentration",
  chapterId: "semiconductors",
  content: `# Semiconductor Carrier Concentration

In an **intrinsic (undoped) semiconductor**, thermal excitation promotes electrons from the valence band to the conduction band, creating equal numbers of electrons (n) and holes (p).

## Intrinsic Carrier Concentration

$$n_i = \\sqrt{N_c N_v} \\exp\\left(-\\frac{E_g}{2k_BT}\\right)$$

where the **effective density of states** for the conduction band is:

$$N_c = 2\\left(\\frac{2\\pi m^* k_B T}{h^2}\\right)^{3/2}$$

For simplicity we assume N_c = N_v (equal effective masses for electrons and holes).

## Temperature Dependence

The exponential factor dominates: doubling T dramatically increases n_i. This is why semiconductors become better conductors at high temperature (opposite to metals).

| Material | E_g (eV) | n_i at 300K (approx) |
|----------|-----------|----------------------|
| Silicon  | 1.12      | ~10¹⁰ m⁻³ (using real m*) |
| Germanium | 0.67     | ~10¹³ m⁻³ (using real m*) |

Note: Using free electron mass gives higher n_i than experimental values because real effective masses are smaller.

## Conductivity

$$\\sigma = n e (\\mu_e + \\mu_h)$$

where μ_e and μ_h are the electron and hole mobilities (m²/V·s).

## Your Task

\`\`\`python
import math

def effective_dos_m3(T_K, m_eff_kg):
    # N_c = 2 * (2*pi*m*k_B*T / h^2)^(3/2)
    # k_B=1.381e-23, h=6.626e-34
    pass

def intrinsic_carrier_m3(E_g_eV, T_K, m_eff_kg):
    # n_i = sqrt(N_c * N_v) * exp(-E_g_J / (2*k_B*T))
    # N_c = N_v = effective_dos_m3(T_K, m_eff_kg)
    # e=1.602e-19 (to convert eV to J)
    pass

def conductivity_S_m(n_m3, mu_e_m2_V_s, mu_h_m2_V_s):
    # sigma = n * e * (mu_e + mu_h), e=1.602e-19
    pass
\`\`\`
`,
  starterCode: `import math

def effective_dos_m3(T_K, m_eff_kg):
    # N_c = 2 * (2*pi*m*k_B*T / h^2)^(3/2)
    # k_B = 1.381e-23, h = 6.626e-34
    pass

def intrinsic_carrier_m3(E_g_eV, T_K, m_eff_kg):
    # n_i = sqrt(N_c * N_v) * exp(-E_g_J / (2*k_B*T))
    # Assume N_c = N_v, e = 1.602e-19 to convert eV -> J
    pass

def conductivity_S_m(n_m3, mu_e_m2_V_s, mu_h_m2_V_s):
    # sigma = n * e * (mu_e + mu_h), e = 1.602e-19
    pass
`,
  solution: `import math

def effective_dos_m3(T_K, m_eff_kg):
    k_B = 1.381e-23
    h = 6.626e-34
    return 2 * (2 * math.pi * m_eff_kg * k_B * T_K / h**2)**(3/2)

def intrinsic_carrier_m3(E_g_eV, T_K, m_eff_kg):
    k_B = 1.381e-23
    e = 1.602e-19
    N_c = effective_dos_m3(T_K, m_eff_kg)
    N_v = N_c
    E_g_J = E_g_eV * e
    return math.sqrt(N_c * N_v) * math.exp(-E_g_J / (2 * k_B * T_K))

def conductivity_S_m(n_m3, mu_e_m2_V_s, mu_h_m2_V_s):
    e = 1.602e-19
    return n_m3 * e * (mu_e_m2_V_s + mu_h_m2_V_s)
`,
  tests: [
    {
      name: "Effective DOS at 300K with free electron mass",
      expected: "2.5103e+25\n",
      code: `{{FUNC}}
print(f"{effective_dos_m3(300, 9.109e-31):.4e}")`,
    },
    {
      name: "Intrinsic carrier concentration of silicon (E_g=1.12 eV) at 300K",
      expected: "9.8984e+15\n",
      code: `{{FUNC}}
print(f"{intrinsic_carrier_m3(1.12, 300, 9.109e-31):.4e}")`,
    },
    {
      name: "Intrinsic carrier concentration of germanium (E_g=0.67 eV) at 300K",
      expected: "5.9432e+19\n",
      code: `{{FUNC}}
print(f"{intrinsic_carrier_m3(0.67, 300, 9.109e-31):.4e}")`,
    },
    {
      name: "Conductivity of doped Si: n=1e16, mu_e=0.135, mu_h=0.048 m²/Vs",
      expected: "0.0003\n",
      code: `{{FUNC}}
print(f"{conductivity_S_m(1e16, 0.135, 0.048):.4f}")`,
    },
  ],
};
