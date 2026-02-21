import type { Lesson } from "../../types";

export const thermalConductivity: Lesson = {
  id: "thermal-conductivity",
  title: "Thermal Conductivity",
  chapterId: "phonons",
  content: `# Thermal Conductivity

Thermal conductivity κ describes how readily a material conducts heat. In insulators it is dominated by phonons; in metals both phonons and electrons contribute.

## Kinetic Theory (Phonon Contribution)

From kinetic theory, the lattice thermal conductivity is:

$$\\kappa = \\frac{1}{3} C_V v_s \\lambda_{\\text{mfp}}$$

where:
- C_V — heat capacity per unit volume (J·m⁻³·K⁻¹)
- v_s — sound velocity (m/s)
- λ_mfp — phonon mean free path (m)

## Wiedemann–Franz Law (Electronic Contribution)

For metals, the electronic thermal conductivity is linked to electrical conductivity σ by:

$$\\frac{\\kappa_e}{\\sigma T} = L_0$$

where L₀ is the **Lorenz number**:

$$L_0 = \\frac{\\pi^2 k_B^2}{3 e^2} \\approx 2.44 \\times 10^{-8} \; \\text{W·Ω·K}^{-2}$$

This gives κ_e = L₀ σ T.

## Thermal Resistance

For a rod of length L, cross-sectional area A, and conductivity κ:

$$R_{\\text{th}} = \\frac{L}{\\kappa A} \\quad [\\text{K/W}]$$

## Implement

\`\`\`python
def kinetic_thermal_conductivity_W_mK(C_V_J_m3K, v_s_m_s, lambda_mfp_m):
    ...

def lorenz_number():
    # Returns L_0 in W·Ω·K⁻²
    ...

def wiedemann_franz_kappa(sigma_S_m, T_K):
    # Returns electron thermal conductivity in W/(m·K)
    ...

def thermal_resistance_K_W(kappa_W_mK, L_m, A_m2):
    # Returns thermal resistance in K/W
    ...
\`\`\`
`,
  starterCode: `import math

def kinetic_thermal_conductivity_W_mK(C_V_J_m3K, v_s_m_s, lambda_mfp_m):
    pass

def lorenz_number():
    pass

def wiedemann_franz_kappa(sigma_S_m, T_K):
    pass

def thermal_resistance_K_W(kappa_W_mK, L_m, A_m2):
    pass
`,
  solution: `import math

def kinetic_thermal_conductivity_W_mK(C_V_J_m3K, v_s_m_s, lambda_mfp_m):
    return (1 / 3) * C_V_J_m3K * v_s_m_s * lambda_mfp_m

def lorenz_number():
    k_B = 1.381e-23
    e = 1.602e-19
    return math.pi**2 * k_B**2 / (3 * e**2)

def wiedemann_franz_kappa(sigma_S_m, T_K):
    L0 = lorenz_number()
    return L0 * sigma_S_m * T_K

def thermal_resistance_K_W(kappa_W_mK, L_m, A_m2):
    return L_m / (kappa_W_mK * A_m2)
`,
  tests: [
    {
      name: "Kinetic thermal conductivity (copper phonon contribution)",
      expected: "107.8933\n",
      code: `{{FUNC}}\nprint(f"{kinetic_thermal_conductivity_W_mK(1.7e6, 4760, 4e-8):.4f}")`,
    },
    {
      name: "Lorenz number",
      expected: "2.4448e-08\n",
      code: `{{FUNC}}\nprint(f"{lorenz_number():.4e}")`,
    },
    {
      name: "Wiedemann-Franz electronic thermal conductivity (copper, 300 K)",
      expected: "440.06\n",
      code: `{{FUNC}}\nprint(f"{wiedemann_franz_kappa(6e7, 300):.2f}")`,
    },
    {
      name: "Thermal resistance of 10 cm copper rod (1 cm² cross-section)",
      expected: "0.2500\n",
      code: `{{FUNC}}\nprint(f"{thermal_resistance_K_W(400, 0.01, 1e-4):.4f}")`,
    },
  ],
};
