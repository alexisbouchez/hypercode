import type { Lesson } from "../../types";

export const debyeModel: Lesson = {
  id: "debye-model",
  title: "Debye Model",
  chapterId: "phonons",
  content: `## Debye Model of Heat Capacity

The **Debye model** treats lattice vibrations (phonons) as sound waves with a cutoff frequency ω_D. It correctly predicts the T³ dependence of heat capacity at low temperatures.

### Debye Frequency and Temperature

The Debye cutoff frequency is set by the condition that there are exactly 3N phonon modes per N atoms:

$$\\omega_D = v_s \\left(6\\pi^2 n\\right)^{1/3}$$

The corresponding **Debye temperature**:

$$\\theta_D = \\frac{\\hbar \\omega_D}{k_B}$$

Where v_s is the (Debye-averaged) sound speed and n is the atom number density.

### Heat Capacity: Low Temperature Limit (T ≪ θ_D)

$$C_V \\approx \\frac{12\\pi^4}{5} N k_B \\left(\\frac{T}{\\theta_D}\\right)^3$$

This **Debye T³ law** matches experimental data well below about θ_D/10.

### Heat Capacity: High Temperature Limit (T ≫ θ_D)

The **Dulong-Petit law** — classical equipartition:

$$C_V \\rightarrow 3 N k_B$$

For one mole, this gives ~24.9 J/K regardless of the material.

### Physical Constants (inside your functions)
- ħ = 1.055 × 10⁻³⁴ J·s
- k_B = 1.381 × 10⁻²³ J/K
`,
  starterCode: `import math

def debye_temperature_K(v_s_m_s, n_m3):
    pass

def debye_cv_low_T(T_K, theta_D_K, N):
    pass

def debye_cv_high_T(N):
    pass
`,
  solution: `import math

def debye_temperature_K(v_s_m_s, n_m3):
    hbar = 1.055e-34
    k_B = 1.381e-23
    omega_D = v_s_m_s * (6 * math.pi**2 * n_m3)**(1/3)
    return hbar * omega_D / k_B

def debye_cv_low_T(T_K, theta_D_K, N):
    k_B = 1.381e-23
    return (12 * math.pi**4 / 5) * N * k_B * (T_K / theta_D_K)**3

def debye_cv_high_T(N):
    k_B = 1.381e-23
    return 3 * N * k_B
`,
  tests: [
    {
      name: "Debye temperature of copper",
      expected: "467.21\n",
      code: `{{FUNC}}
print(f"{debye_temperature_K(3570, 8.49e28):.2f}")`,
    },
    {
      name: "Debye heat capacity at T=10K (low-T limit)",
      expected: "4.8180e-02\n",
      code: `{{FUNC}}
print(f"{debye_cv_low_T(10, 343, 6.022e23):.4e}")`,
    },
    {
      name: "Debye heat capacity at T=50K (low-T limit)",
      expected: "6.0224e+00\n",
      code: `{{FUNC}}
print(f"{debye_cv_low_T(50, 343, 6.022e23):.4e}")`,
    },
    {
      name: "Dulong-Petit limit (1 mole)",
      expected: "24.9491\n",
      code: `{{FUNC}}
print(f"{debye_cv_high_T(6.022e23):.4f}")`,
    },
  ],
};
