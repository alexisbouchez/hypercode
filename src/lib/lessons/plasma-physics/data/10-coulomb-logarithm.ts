import type { Lesson } from "../../types";

export const coulombLogarithm: Lesson = {
  id: "coulomb-logarithm",
  title: "Coulomb Logarithm",
  chapterId: "plasma-fundamentals",
  content: `## Coulomb Logarithm

The **Coulomb logarithm** \\(\\ln \\Lambda\\) appears throughout plasma physics whenever we calculate collision rates, transport coefficients, or energy exchange rates. It accounts for the cumulative effect of many small-angle Coulomb collisions.

### Why a Logarithm?

In a Coulomb interaction, the scattering cross-section diverges at large impact parameters (weak, distant encounters). In a plasma, Debye shielding cuts off this divergence at the Debye length \\(\\lambda_D\\). The ratio of the maximum to minimum impact parameter gives the logarithm.

### The 90° Deflection Parameter

The minimum impact parameter is the **90° deflection parameter** \\(b_{90}\\) — the distance at which the electron's kinetic energy equals the potential energy:

$$b_{90} = \\frac{e^2}{4\\pi\\varepsilon_0 m_e v^2}$$

Using the thermal velocity \\(v = \\sqrt{k_B T / m_e}\\), we get \\(b_{90} \\propto 1/T\\).

### Coulomb Logarithm Formula

$$\\ln \\Lambda = \\ln\\left(\\frac{\\lambda_D}{b_{90}}\\right) \\approx \\ln\\left(12\\pi n \\lambda_D^3\\right)$$

where the Debye length is:

$$\\lambda_D = \\sqrt{\\frac{\\varepsilon_0 k_B T}{n e^2}}$$

Typical values range from \\(\\ln \\Lambda \\approx 10\\) to \\(20\\) in laboratory and space plasmas.

### Electron-Ion Collision Frequency

Using a simplified SI expression, the electron-ion collision frequency is:

$$\\nu_{ei} = \\frac{2.91 \\times 10^{-12} \\cdot n \\cdot \\ln \\Lambda}{T^{3/2}}$$

where \\(n\\) is in m\\(^{-3}\\) and \\(T\\) is in Kelvin. The \\(T^{-3/2}\\) dependence means **hotter plasmas are more collisionless** — a fundamental and counterintuitive property of plasmas.

### Physical Significance

- \\(\\ln \\Lambda \\sim 10\\): Typical fusion plasma
- \\(\\ln \\Lambda \\sim 20\\): Solar corona
- \\(\\ln \\Lambda < 2\\): Strongly coupled plasma (solid-like behavior)

The Coulomb logarithm enters into resistivity, thermal conductivity, viscosity, and energy equilibration times — making it one of the most important parameters in plasma transport theory.
`,
  starterCode: `import math

def coulomb_logarithm(n_m3, T_K):
    # Compute ln(Lambda) using Debye length formula
    # eps0 = 8.854187817e-12 F/m
    # k_B = 1.380649e-23 J/K
    # e = 1.602176634e-19 C
    # debye_length = sqrt(eps0 * k_B * T / (n * e^2))
    # ln_lambda = log(12 * pi * n * debye_length^3)
    pass

def collision_frequency_s(n_m3, T_K):
    # Return electron-ion collision frequency in s^-1
    # nu = 2.91e-12 * n * ln_lambda / T^1.5
    pass`,
  solution: `import math

def coulomb_logarithm(n_m3, T_K):
    eps0 = 8.854187817e-12
    k_B = 1.380649e-23
    e = 1.602176634e-19
    debye_length = math.sqrt(eps0 * k_B * T_K / (n_m3 * e**2))
    ln_lambda = math.log(12 * math.pi * n_m3 * debye_length**3)
    return ln_lambda

def collision_frequency_s(n_m3, T_K):
    ln_lambda = coulomb_logarithm(n_m3, T_K)
    return 2.91e-12 * n_m3 * ln_lambda / T_K**1.5`,
  tests: [
    {
      name: "Coulomb logarithm: n=1e19 m⁻³, T=10000 K (cold dense plasma)",
      expected: "8.2733\n",
      code: `{{FUNC}}
print(f"{coulomb_logarithm(1e19, 10000):.4f}")`,
    },
    {
      name: "Coulomb logarithm: n=1e20 m⁻³, T=10 MK (fusion plasma)",
      expected: "17.4836\n",
      code: `{{FUNC}}
print(f"{coulomb_logarithm(1e20, 1e7):.4f}")`,
    },
    {
      name: "Collision frequency: n=1e19 m⁻³, T=10000 K",
      expected: "2.4075e+02\n",
      code: `{{FUNC}}
print(f"{collision_frequency_s(1e19, 10000):.4e}")`,
    },
    {
      name: "Collision frequency: n=1e20 m⁻³, T=10 MK (collisionless fusion plasma)",
      expected: "1.6089e-01\n",
      code: `{{FUNC}}
print(f"{collision_frequency_s(1e20, 1e7):.4e}")`,
    },
  ],
};
