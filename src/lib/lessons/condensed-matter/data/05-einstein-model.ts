import type { Lesson } from "../../types";

export const einsteinModel: Lesson = {
  id: "einstein-model",
  title: "Einstein Model",
  chapterId: "phonons",
  content: `## Einstein Model of Heat Capacity

The **Einstein model** (1907) was the first quantum mechanical treatment of lattice heat capacity. It assumes all N atoms oscillate independently at the same frequency ω_E.

### Einstein Temperature

$$\\theta_E = \\frac{\\hbar \\omega_E}{k_B}$$

This characteristic temperature separates the quantum (T ≪ θ_E) from the classical (T ≫ θ_E) regimes.

### Heat Capacity

$$C_V = 3Nk_B \\left(\\frac{\\theta_E}{T}\\right)^2 \\frac{e^{\\theta_E/T}}{\\left(e^{\\theta_E/T} - 1\\right)^2}$$

**Limits:**
- **High T** (T ≫ θ_E): C_V → 3Nk_B (Dulong-Petit law)
- **Low T** (T ≪ θ_E): C_V → 3Nk_B (θ_E/T)² exp(-θ_E/T) (exponential suppression)

The Einstein model correctly captures the quantum freeze-out of vibrations at low T, though it falls off too fast compared to experiment (the Debye T³ law is better at very low T).

### Mean Energy per Oscillator

Including the zero-point energy (½ħω per mode, 3 modes):

$$\\langle E \\rangle = \\frac{3k_B\\theta_E}{e^{\\theta_E/T} - 1} + \\frac{3}{2}k_B\\theta_E$$

### Physical Constants (inside your functions)
- k_B = 1.381 × 10⁻²³ J/K
- ħ = 1.055 × 10⁻³⁴ J·s
`,
  starterCode: `import math

def einstein_cv(T_K, theta_E_K, N):
    pass

def einstein_temperature_K(omega_E_rad_s):
    pass

def mean_energy_J(T_K, theta_E_K):
    pass
`,
  solution: `import math

def einstein_cv(T_K, theta_E_K, N):
    k_B = 1.381e-23
    x = theta_E_K / T_K
    return 3 * N * k_B * x**2 * math.exp(x) / (math.exp(x) - 1)**2

def einstein_temperature_K(omega_E_rad_s):
    hbar = 1.055e-34
    k_B = 1.381e-23
    return hbar * omega_E_rad_s / k_B

def mean_energy_J(T_K, theta_E_K):
    k_B = 1.381e-23
    return 3 * k_B * theta_E_K / (math.exp(theta_E_K / T_K) - 1) + (3/2) * k_B * theta_E_K
`,
  tests: [
    {
      name: "Einstein heat capacity at 300K",
      expected: "2.3660e+01\n",
      code: `{{FUNC}}
print(f"{einstein_cv(300, 240, 6.022e23):.4e}")`,
    },
    {
      name: "Einstein heat capacity at 1000K (near Dulong-Petit)",
      expected: "24.8297\n",
      code: `{{FUNC}}
print(f"{einstein_cv(1000, 240, 6.022e23):.4f}")`,
    },
    {
      name: "Einstein temperature from angular frequency",
      expected: "190.98\n",
      code: `{{FUNC}}
print(f"{einstein_temperature_K(2.5e13):.2f}")`,
    },
    {
      name: "Mean energy of Einstein oscillator at 300K",
      expected: "1.3085e-20\n",
      code: `{{FUNC}}
print(f"{mean_energy_J(300, 240):.4e}")`,
    },
  ],
};
