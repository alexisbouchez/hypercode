import type { Lesson } from "../../types";

export const cyclotronMotion: Lesson = {
  id: "cyclotron-motion",
  title: "Cyclotron Motion",
  chapterId: "single-particle",
  content: `# Cyclotron Motion

When a charged particle moves in a magnetic field, the Lorentz force $\\mathbf{F} = q\\mathbf{v} \\times \\mathbf{B}$ acts perpendicular to the velocity, causing the particle to **gyrate** in a circle. This circular motion is called **cyclotron motion** (or gyromotion).

## Gyrofrequency (Cyclotron Frequency)

The angular frequency of the circular orbit:

$$\\omega_c = \\frac{|q| B}{m}$$

This is independent of the particle's speed — all particles of the same charge-to-mass ratio gyrate at the same frequency in a given field.

## Gyroperiod

The time for one complete orbit:

$$T_c = \\frac{2\\pi}{\\omega_c} = \\frac{2\\pi m}{|q| B}$$

## Larmor Radius (Gyroradius)

The radius of the circular orbit depends on the perpendicular velocity $v_\\perp$:

$$r_L = \\frac{m v_\\perp}{|q| B}$$

Key parameters:
- $e = 1.602 \\times 10^{-19}$ C
- $m_e = 9.109 \\times 10^{-31}$ kg (electron)
- $m_p = 1.673 \\times 10^{-27}$ kg (proton)

## Physical Intuition

- **Electrons** gyrate at much higher frequencies than ions (smaller mass → higher $\\omega_c$)
- **Higher energy** particles have larger Larmor radii
- In fusion devices, small Larmor radii confine particles to magnetic field lines
- Cyclotron resonance heating (ICRH, ECRH) drives plasma heating by injecting waves at $\\omega_c$

Implement the three gyration functions below.`,
  starterCode: `import math

def gyrofrequency_rad_s(B_T, m_kg, q_C):
    # omega_c = |q| * B / m
    pass

def gyroradius_m(m_kg, v_perp_m_s, B_T, q_C):
    # r_L = m * v_perp / (|q| * B)
    pass

def gyroperiod_s(B_T, m_kg, q_C):
    # T_c = 2 * pi / omega_c
    pass`,
  solution: `import math

def gyrofrequency_rad_s(B_T, m_kg, q_C):
    return abs(q_C) * B_T / m_kg

def gyroradius_m(m_kg, v_perp_m_s, B_T, q_C):
    return m_kg * v_perp_m_s / (abs(q_C) * B_T)

def gyroperiod_s(B_T, m_kg, q_C):
    return 2 * math.pi / gyrofrequency_rad_s(B_T, m_kg, q_C)`,
  tests: [
    {
      name: "Electron gyrofrequency in B=1T",
      expected: "1.7587e+11\n",
      code: `{{FUNC}}
e = 1.602e-19
m_e = 9.109e-31
print(f"{gyrofrequency_rad_s(1.0, m_e, e):.4e}")`,
    },
    {
      name: "Proton gyrofrequency in B=1T",
      expected: "9.5756e+07\n",
      code: `{{FUNC}}
e = 1.602e-19
m_p = 1.673e-27
print(f"{gyrofrequency_rad_s(1.0, m_p, e):.4e}")`,
    },
    {
      name: "Electron Larmor radius for v_perp=1e6 m/s in B=0.01T",
      expected: "5.6860e-04\n",
      code: `{{FUNC}}
e = 1.602e-19
m_e = 9.109e-31
print(f"{gyroradius_m(m_e, 1e6, 0.01, e):.4e}")`,
    },
    {
      name: "Electron gyroperiod in B=1T",
      expected: "3.5726e-11\n",
      code: `{{FUNC}}
e = 1.602e-19
m_e = 9.109e-31
print(f"{gyroperiod_s(1.0, m_e, e):.4e}")`,
    },
  ],
};
