import type { Lesson } from "../../types";

export const plasmaFrequency: Lesson = {
  id: "plasma-frequency",
  title: "Plasma Frequency",
  chapterId: "plasma-fundamentals",
  content: `# Plasma Frequency

The **plasma frequency** is the natural oscillation frequency of electrons in a plasma. If electrons are displaced from their equilibrium positions, the restoring electric force causes them to oscillate at the plasma frequency.

## Electron Plasma Frequency

$$\\omega_{pe} = \\sqrt{\\frac{n e^2}{\\varepsilon_0 m_e}}$$

In cycles per second (Hz):

$$f_{pe} = \\frac{\\omega_{pe}}{2\\pi}$$

Where:
- $n$ = electron number density (m⁻³)
- $e = 1.602 \\times 10^{-19}$ C
- $\\varepsilon_0 = 8.854 \\times 10^{-12}$ F/m
- $m_e = 9.109 \\times 10^{-31}$ kg

## Ion Plasma Frequency

Ions also have a plasma frequency, but much lower due to their larger mass:

$$\\omega_{pi} = \\sqrt{\\frac{n e^2}{\\varepsilon_0 m_i}}$$

where $m_i = A \\times 1.673 \\times 10^{-27}$ kg, and $A$ is the atomic mass number (e.g., $A=1$ for hydrogen/protons).

## Physical Significance

- Electromagnetic waves with frequency $f < f_{pe}$ **cannot propagate** through the plasma — they are reflected. This is why the ionosphere reflects AM radio waves!
- The ratio $\\omega_{pe} / \\omega_{pi} = \\sqrt{m_i / m_e} \\approx 43$ for hydrogen — electrons oscillate much faster than ions.

Implement the three functions below to compute these frequencies.`,
  starterCode: `import math

def plasma_frequency_rad_s(n_m3):
    # eps0 = 8.854e-12, e = 1.602e-19, m_e = 9.109e-31
    # omega_pe = sqrt(n * e^2 / (eps0 * m_e))
    pass

def plasma_frequency_Hz(n_m3):
    # f_pe = omega_pe / (2 * pi)
    pass

def ion_plasma_frequency_rad_s(n_m3, A):
    # m_i = A * 1.673e-27
    # omega_pi = sqrt(n * e^2 / (eps0 * m_i))
    pass`,
  solution: `import math

def plasma_frequency_rad_s(n_m3):
    eps0 = 8.854e-12
    e = 1.602e-19
    m_e = 9.109e-31
    return math.sqrt(n_m3 * e**2 / (eps0 * m_e))

def plasma_frequency_Hz(n_m3):
    return plasma_frequency_rad_s(n_m3) / (2 * math.pi)

def ion_plasma_frequency_rad_s(n_m3, A):
    eps0 = 8.854e-12
    e = 1.602e-19
    m_i = A * 1.673e-27
    return math.sqrt(n_m3 * e**2 / (eps0 * m_i))`,
  tests: [
    {
      name: "Electron plasma frequency in Hz at n=1e19",
      expected: "2.8391e+10\n",
      code: `{{FUNC}}
print(f"{plasma_frequency_Hz(1e19):.4e}")`,
    },
    {
      name: "Electron plasma frequency in Hz at n=1e20",
      expected: "8.9780e+10\n",
      code: `{{FUNC}}
print(f"{plasma_frequency_Hz(1e20):.4e}")`,
    },
    {
      name: "Hydrogen ion plasma frequency at n=1e19",
      expected: "4.1624e+09\n",
      code: `{{FUNC}}
print(f"{ion_plasma_frequency_rad_s(1e19, 1):.4e}")`,
    },
    {
      name: "Ratio of electron to ion plasma frequency (should be ~sqrt(m_i/m_e))",
      expected: "42.8561\n",
      code: `{{FUNC}}
print(f"{plasma_frequency_rad_s(1e19) / ion_plasma_frequency_rad_s(1e19, 1):.4f}")`,
    },
  ],
};
