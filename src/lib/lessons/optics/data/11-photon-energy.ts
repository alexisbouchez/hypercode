import type { Lesson } from "../../types";

export const photonEnergy: Lesson = {
  id: "photon-energy",
  title: "Photon Energy and the Photoelectric Effect",
  chapterId: "modern",
  content: `# Photon Energy and the Photoelectric Effect

Light is not only a wave — it also behaves as a stream of discrete energy packets called **photons**. The energy of each photon depends on its frequency (or equivalently, its wavelength).

## Photon Energy

$$E = hf = \\frac{hc}{\\lambda}$$

Where:
- $h = 6.626 \\times 10^{-34}\\,\\text{J·s}$ is Planck's constant
- $f$ is the frequency in Hz
- $c = 3 \\times 10^8\\,\\text{m/s}$ is the speed of light
- $\\lambda$ is the wavelength in meters

In electron-volts ($1\\,\\text{eV} = 1.602 \\times 10^{-19}\\,\\text{J}$):

$$E_{\\text{eV}} = \\frac{hc}{\\lambda \\cdot e}$$

**Visible light energies:**

| Color | Wavelength (nm) | Energy (eV) |
|-------|----------------|-------------|
| Red | 700 | 1.77 |
| Yellow | 589 | 2.11 |
| Green | 550 | 2.26 |
| Violet | 400 | 3.10 |

## The Photoelectric Effect

Einstein's 1905 explanation of the photoelectric effect earned him the Nobel Prize: when light shines on a metal, electrons are ejected only if the photon energy exceeds the metal's **work function** $\\phi$.

The maximum kinetic energy of ejected electrons is:

$$KE_{\\max} = hf - \\phi = \\frac{hc}{\\lambda} - \\phi$$

Key observations:
- No electrons are emitted below a threshold frequency, regardless of intensity.
- Above threshold, $KE_{\\max}$ depends only on frequency, not intensity.
- Intensity determines the number of ejected electrons, not their energy.

**Example:** UV light at $\\lambda = 200\\,\\text{nm}$ on sodium ($\\phi = 2.3\\,\\text{eV}$):

$$E = \\frac{hc}{\\lambda} \\approx 6.2\\,\\text{eV}$$
$$KE_{\\max} = 6.2 - 2.3 = 3.9\\,\\text{eV}$$

## Your Task

Implement the two functions. Use $h = 6.626 \\times 10^{-34}\\,\\text{J·s}$, $c = 3 \\times 10^8\\,\\text{m/s}$, $e = 1.602 \\times 10^{-19}\\,\\text{J/eV}$.
`,
  starterCode: `def photon_energy_eV(lam_nm):
    # Return the photon energy in electron-volts for wavelength lam_nm (in nm)
    pass

def photoelectric_ke_eV(lam_nm, work_function_eV):
    # Return the maximum kinetic energy in eV of photoelectrons
    # lam_nm: photon wavelength in nm, work_function_eV: metal work function in eV
    pass
`,
  solution: `def photon_energy_eV(lam_nm):
    h = 6.626e-34
    c = 3e8
    eV = 1.602e-19
    lam_m = lam_nm * 1e-9
    return h * c / (lam_m * eV)

def photoelectric_ke_eV(lam_nm, work_function_eV):
    return photon_energy_eV(lam_nm) - work_function_eV
`,
  tests: [
    {
      name: "photon_energy_eV(550 nm) = 2.256 eV",
      code: `{{FUNC}}
print(round(photon_energy_eV(550), 4))`,
      expected: "2.256\n",
    },
    {
      name: "photon_energy_eV(400 nm) = 3.1021 eV",
      code: `{{FUNC}}
print(round(photon_energy_eV(400), 4))`,
      expected: "3.1021\n",
    },
    {
      name: "photoelectric_ke_eV(200 nm, 4.5 eV) = 1.7041 eV",
      code: `{{FUNC}}
print(round(photoelectric_ke_eV(200, 4.5), 4))`,
      expected: "1.7041\n",
    },
    {
      name: "photoelectric_ke_eV(300 nm, 2.0 eV) = 2.1361 eV",
      code: `{{FUNC}}
print(round(photoelectric_ke_eV(300, 2.0), 4))`,
      expected: "2.1361\n",
    },
  ],
};
