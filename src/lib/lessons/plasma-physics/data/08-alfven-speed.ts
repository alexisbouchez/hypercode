import type { Lesson } from "../../types";

export const alfvenSpeed: Lesson = {
  id: "alfven-speed",
  title: "Alfvén Speed",
  chapterId: "mhd",
  content: `## Alfvén Speed

**Alfvén waves** are low-frequency, transverse electromagnetic waves that propagate along magnetic field lines in a magnetized plasma. They are one of the fundamental wave modes in magnetohydrodynamics (MHD).

### Alfvén Speed

The speed at which Alfvén waves propagate is:

$$v_A = \\frac{B}{\\sqrt{\\mu_0 \\rho}}$$

where:
- \\(B\\) is the magnetic field strength (T)
- \\(\\mu_0 = 4\\pi \\times 10^{-7}\\) H/m is the permeability of free space
- \\(\\rho = n m_i\\) is the mass density of the plasma (kg/m³), with \\(n\\) the number density and \\(m_i\\) the ion mass

The Alfvén speed is the plasma analog of the speed of sound in a gas, with magnetic pressure playing the role of thermal pressure.

### Alfvén Mach Number

The **Alfvén Mach number** measures how fast a flow is relative to the Alfvén speed:

$$M_A = \\frac{v}{v_A}$$

When \\(M_A < 1\\), the flow is sub-Alfvénic. When \\(M_A > 1\\), it is super-Alfvénic.

### Sound Speed

For comparison, the sound speed in a plasma is:

$$v_s = \\sqrt{\\frac{\\gamma k_B T}{m_i}}$$

where \\(\\gamma = 5/3\\) is the adiabatic index and \\(k_B = 1.38 \\times 10^{-23}\\) J/K is the Boltzmann constant.

### Physical Significance

- **Solar wind**: The Alfvén point marks where the solar wind becomes super-Alfvénic
- **Fusion plasmas**: Alfvén waves can heat plasma via wave-particle resonance
- **Space weather**: Alfvénic fluctuations carry energy from the Sun to Earth's magnetosphere
`,
  starterCode: `import math

def alfven_speed_m_s(B_T, n_m3, m_i_kg):
    # Return Alfvén speed in m/s
    # mu0 = 4 * pi * 1e-7
    pass

def alfven_mach_number(v_m_s, B_T, n_m3, m_i_kg):
    # Return Alfvén Mach number (dimensionless)
    pass

def sound_speed_m_s(T_K, m_i_kg, gamma=5/3):
    # Return sound speed in m/s
    # k_B = 1.380649e-23 J/K
    pass`,
  solution: `import math

def alfven_speed_m_s(B_T, n_m3, m_i_kg):
    mu0 = 4 * math.pi * 1e-7
    rho = n_m3 * m_i_kg
    return B_T / math.sqrt(mu0 * rho)

def alfven_mach_number(v_m_s, B_T, n_m3, m_i_kg):
    return v_m_s / alfven_speed_m_s(B_T, n_m3, m_i_kg)

def sound_speed_m_s(T_K, m_i_kg, gamma=5/3):
    k_B = 1.380649e-23
    return math.sqrt(gamma * k_B * T_K / m_i_kg)`,
  tests: [
    {
      name: "Alfvén speed in fusion plasma (B=5 T, n=1e20 m⁻³, protons)",
      expected: "1.0905e+07\n",
      code: `{{FUNC}}
print(f"{alfven_speed_m_s(5.0, 1e20, 1.673e-27):.4e}")`,
    },
    {
      name: "Alfvén speed in solar wind (B=10 nT, n=1e6 m⁻³, protons)",
      expected: "2.1810e+05\n",
      code: `{{FUNC}}
print(f"{alfven_speed_m_s(1e-8, 1e6, 1.673e-27):.4e}")`,
    },
    {
      name: "Sound speed at T=10 MK for protons",
      expected: "3.7087e+05\n",
      code: `{{FUNC}}
print(f"{sound_speed_m_s(1e7, 1.673e-27):.4e}")`,
    },
    {
      name: "Alfvén Mach number for solar wind speed 400 km/s",
      expected: "1.8341\n",
      code: `{{FUNC}}
print(f"{alfven_mach_number(4e5, 1e-8, 1e6, 1.673e-27):.4f}")`,
    },
  ],
};
