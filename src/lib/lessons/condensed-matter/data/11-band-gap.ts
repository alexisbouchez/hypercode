import type { Lesson } from "../../types";

export const bandGap: Lesson = {
  id: "band-gap",
  title: "Band Gap and Optical Absorption",
  chapterId: "semiconductors",
  content: `# Band Gap and Optical Absorption

The **band gap** E_g is the minimum energy required to excite an electron from the valence band to the conduction band. It determines the optical and electronic properties of semiconductors.

## Optical Absorption Edge

A photon is absorbed only if its energy equals or exceeds the band gap:

$$E_{\\text{photon}} = hf \\geq E_g$$

This defines the **absorption edge wavelength** — the longest wavelength (lowest photon energy) that can be absorbed:

$$\\lambda_{\\max} = \\frac{hc}{E_g}$$

where h = 6.626×10⁻³⁴ J·s and c = 2.998×10⁸ m/s.

## Photon Energy from Wavelength

Given a wavelength λ in nanometres, the photon energy in eV is:

$$E_{\\text{photon}} = \\frac{hc}{\\lambda \\cdot e}$$

## Direct-Gap Absorption Coefficient

Near the absorption edge in a **direct-gap semiconductor**, the absorption coefficient scales as:

$$\\alpha \\propto \\sqrt{E_{\\text{photon}} - E_g}$$

A practical model:

$$\\alpha(E) = \\alpha_0 \\sqrt{\\max(0,\\, E_{\\text{photon}} - E_g)} \\quad [\\text{m}^{-1}]$$

with α₀ ~ 10⁶ m⁻¹ for typical III-V semiconductors.

## Examples

| Material | E_g (eV) | λ_max (nm) |
|----------|----------|------------|
| Silicon  | 1.12     | ~1107 (near-IR) |
| GaAs     | 1.42     | ~873 (near-IR) |
| GaN      | 3.4      | ~365 (UV) |

## Implement

\`\`\`python
def absorption_edge_nm(E_g_eV):
    # Returns the absorption edge wavelength in nm
    ...

def photon_energy_eV(lam_nm):
    # Returns the photon energy in eV for wavelength lam_nm (nm)
    ...

def direct_gap_absorption_m(E_photon_eV, E_g_eV, alpha0=1e6):
    # Returns absorption coefficient in m⁻¹
    ...
\`\`\`
`,
  starterCode: `import math

def absorption_edge_nm(E_g_eV):
    pass

def photon_energy_eV(lam_nm):
    pass

def direct_gap_absorption_m(E_photon_eV, E_g_eV, alpha0=1e6):
    pass
`,
  solution: `import math

def absorption_edge_nm(E_g_eV):
    h = 6.626e-34
    c = 2.998e8
    e = 1.602e-19
    lam_m = h * c / (E_g_eV * e)
    return lam_m * 1e9

def photon_energy_eV(lam_nm):
    h = 6.626e-34
    c = 2.998e8
    e = 1.602e-19
    lam_m = lam_nm * 1e-9
    return h * c / (lam_m * e)

def direct_gap_absorption_m(E_photon_eV, E_g_eV, alpha0=1e6):
    return alpha0 * math.sqrt(max(0, E_photon_eV - E_g_eV))
`,
  tests: [
    {
      name: "Absorption edge of silicon (E_g=1.12 eV)",
      expected: "1107.14\n",
      code: `{{FUNC}}\nprint(f"{absorption_edge_nm(1.12):.2f}")`,
    },
    {
      name: "Absorption edge of GaAs (E_g=1.42 eV)",
      expected: "873.24\n",
      code: `{{FUNC}}\nprint(f"{absorption_edge_nm(1.42):.2f}")`,
    },
    {
      name: "Photon energy of 500 nm green light",
      expected: "2.4800\n",
      code: `{{FUNC}}\nprint(f"{photon_energy_eV(500):.4f}")`,
    },
    {
      name: "Direct-gap absorption in GaAs at 1.5 eV (E_g=1.42 eV)",
      expected: "2.8284e+05\n",
      code: `{{FUNC}}\nprint(f"{direct_gap_absorption_m(1.5, 1.42):.4e}")`,
    },
  ],
};
