import type { Lesson } from "../../types";

export const fermiEnergy: Lesson = {
  id: "fermi-energy",
  title: "Fermi Energy",
  chapterId: "crystal-structure",
  content: `## Fermi Energy and the Free Electron Model

The **free electron model** treats conduction electrons in a metal as a gas of non-interacting fermions. Electrons fill energy states from the ground state up to the **Fermi energy** E_F at absolute zero.

**Fermi Energy:**

$$E_F = \\frac{\\hbar^2}{2m_e}(3\\pi^2 n)^{2/3}$$

Where:
- **ħ** = reduced Planck constant = 1.055 × 10⁻³⁴ J·s
- **m_e** = electron mass = 9.109 × 10⁻³¹ kg
- **n** = electron number density (electrons/m³)

Typical metals have E_F in the range of 1–12 eV, much larger than k_B T at room temperature (~0.025 eV).

### Fermi Velocity

The speed of electrons at the Fermi surface:

$$v_F = \\sqrt{\\frac{2E_F}{m_e}}$$

Fermi velocities are ~10⁶ m/s — a significant fraction of the speed of light, despite being at "zero temperature."

### Fermi Temperature

A characteristic temperature scale:

$$T_F = \\frac{E_F}{k_B}$$

Since T_F ≫ room temperature for metals, conduction electrons are highly degenerate at normal conditions.

### Physical Constants (inside your functions)
- ħ = 1.055 × 10⁻³⁴ J·s
- m_e = 9.109 × 10⁻³¹ kg
- k_B = 1.381 × 10⁻²³ J/K
- 1 eV = 1.602 × 10⁻¹⁹ J
`,
  starterCode: `import math

def fermi_energy_J(n_m3):
    pass

def fermi_energy_eV(n_m3):
    pass

def fermi_velocity_m_s(n_m3):
    pass

def fermi_temperature_K(n_m3):
    pass
`,
  solution: `import math

def fermi_energy_J(n_m3):
    hbar = 1.055e-34
    m_e = 9.109e-31
    return (hbar**2 / (2 * m_e)) * (3 * math.pi**2 * n_m3)**(2/3)

def fermi_energy_eV(n_m3):
    eV = 1.602e-19
    return fermi_energy_J(n_m3) / eV

def fermi_velocity_m_s(n_m3):
    m_e = 9.109e-31
    EF = fermi_energy_J(n_m3)
    return math.sqrt(2 * EF / m_e)

def fermi_temperature_K(n_m3):
    k_B = 1.381e-23
    EF = fermi_energy_J(n_m3)
    return EF / k_B
`,
  tests: [
    {
      name: "Fermi energy of copper in eV",
      expected: "7.0506\n",
      code: `{{FUNC}}
print(f"{fermi_energy_eV(8.49e28):.4f}")`,
    },
    {
      name: "Fermi energy of gold in eV",
      expected: "5.5317\n",
      code: `{{FUNC}}
print(f"{fermi_energy_eV(5.90e28):.4f}")`,
    },
    {
      name: "Fermi velocity of copper",
      expected: "1.5748e+06\n",
      code: `{{FUNC}}
print(f"{fermi_velocity_m_s(8.49e28):.4e}")`,
    },
    {
      name: "Fermi temperature of copper",
      expected: "8.1789e+04\n",
      code: `{{FUNC}}
print(f"{fermi_temperature_K(8.49e28):.4e}")`,
    },
  ],
};
