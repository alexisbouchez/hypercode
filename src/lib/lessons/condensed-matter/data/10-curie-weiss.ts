import type { Lesson } from "../../types";

export const curieWeiss: Lesson = {
  id: "curie-weiss",
  title: "Curie-Weiss Law",
  chapterId: "quantum-phenomena",
  content: `# Curie-Weiss Law

**Magnetic susceptibility** χ describes how strongly a material magnetizes in response to an applied field: M = χ H. For paramagnets and ferromagnets, χ depends strongly on temperature.

## Curie Law (Paramagnets)

For non-interacting magnetic moments (paramagnet), susceptibility follows the **Curie law**:

$$\\chi = \\frac{C}{T}$$

The **Curie constant** for spin-J ions with g-factor g and number density n is:

$$C = \\frac{n \\mu_0 (g \\mu_B)^2 J(J+1)}{3 k_B}$$

For spin-1/2 particles (J = 1/2, g = 2): J(J+1) = 3/4.

## Curie-Weiss Law (Ferromagnets above T_c)

When magnetic moments interact (mean-field theory), susceptibility diverges at the Curie temperature T_C:

$$\\chi = \\frac{C}{T - T_C}$$

Above T_C, the material is paramagnetic but with enhanced susceptibility. At T_C, χ → ∞ (spontaneous magnetization appears for T < T_C).

## Saturation Magnetization

At absolute zero in a strong field, all moments align:

$$M_{sat} = n \\cdot g \\cdot \\mu_B \\cdot J$$

This is the maximum magnetization achievable.

## Constants

- μ₀ = 4π×10⁻⁷ T·m/A (permeability of free space)
- μ_B = 9.274×10⁻²⁴ J/T (Bohr magneton)
- k_B = 1.381×10⁻²³ J/K

## Your Task

\`\`\`python
def curie_constant(n_m3, J=0.5, g=2):
    # C = n * mu0 * (g*mu_B)^2 * J*(J+1) / (3*k_B)
    pass

def curie_susceptibility(T_K, n_m3, J=0.5, g=2):
    # chi = C / T
    pass

def curie_weiss_susceptibility(T_K, T_c_K, n_m3, J=0.5, g=2):
    # chi = C / (T - T_c)
    pass

def saturation_magnetization_A_m(n_m3, J=0.5, g=2):
    # M_sat = n * g * mu_B * J
    pass
\`\`\`
`,
  starterCode: `def curie_constant(n_m3, J=0.5, g=2):
    # C = n * mu0 * (g*mu_B)^2 * J*(J+1) / (3*k_B)
    # mu0 = 4*pi*1e-7, mu_B = 9.274e-24, k_B = 1.381e-23
    pass

def curie_susceptibility(T_K, n_m3, J=0.5, g=2):
    # chi = C / T
    pass

def curie_weiss_susceptibility(T_K, T_c_K, n_m3, J=0.5, g=2):
    # chi = C / (T - T_c)
    pass

def saturation_magnetization_A_m(n_m3, J=0.5, g=2):
    # M_sat = n * g * mu_B * J, mu_B = 9.274e-24
    pass
`,
  solution: `import math

def curie_constant(n_m3, J=0.5, g=2):
    mu0 = 4 * math.pi * 1e-7
    mu_B = 9.274e-24
    k_B = 1.381e-23
    return n_m3 * mu0 * (g * mu_B)**2 * J * (J + 1) / (3 * k_B)

def curie_susceptibility(T_K, n_m3, J=0.5, g=2):
    C = curie_constant(n_m3, J, g)
    return C / T_K

def curie_weiss_susceptibility(T_K, T_c_K, n_m3, J=0.5, g=2):
    C = curie_constant(n_m3, J, g)
    return C / (T_K - T_c_K)

def saturation_magnetization_A_m(n_m3, J=0.5, g=2):
    mu_B = 9.274e-24
    return n_m3 * g * mu_B * J
`,
  tests: [
    {
      name: "Curie susceptibility of spin-1/2 paramagnet at 300K, n=1e28",
      expected: "2.6087e-04\n",
      code: `{{FUNC}}
print(f"{curie_susceptibility(300, 1e28):.4e}")`,
    },
    {
      name: "Curie susceptibility at 100K is three times larger than at 300K",
      expected: "7.8262e-04\n",
      code: `{{FUNC}}
print(f"{curie_susceptibility(100, 1e28):.4e}")`,
    },
    {
      name: "Curie-Weiss susceptibility at T=400K with T_c=300K, n=1e28",
      expected: "7.8262e-04\n",
      code: `{{FUNC}}
print(f"{curie_weiss_susceptibility(400, 300, 1e28):.4e}")`,
    },
    {
      name: "Saturation magnetization for n=1e28 spin-1/2",
      expected: "9.2740e+04\n",
      code: `{{FUNC}}
print(f"{saturation_magnetization_A_m(1e28):.4e}")`,
    },
  ],
};
