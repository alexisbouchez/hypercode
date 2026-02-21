import type { Lesson } from "../../types";

export const fusionPower: Lesson = {
  id: "fusion-power",
  title: "Fusion Power Density",
  chapterId: "fusion",
  content: `# Fusion Power Density

The **deuterium-tritium (D-T) reaction** is the most promising fusion fuel cycle for near-term reactors:

$$\\text{D} + \\text{T} \\rightarrow {}^4\\text{He} (3.5\\,\\text{MeV}) + n (14.1\\,\\text{MeV})$$

Total energy release: **17.59 MeV** \\(= 2.818 \\times 10^{-12}\\) J per reaction.

## Fusion Power Density

The volumetric power density is:

$$P_{\\text{fus}} = n_D \\cdot n_T \\cdot \\langle \\sigma v \\rangle \\cdot E_{\\text{fus}}$$

For an equimolar D-T plasma: \\(n_D = n_T = n/2\\).

## D-T Reactivity ⟨σv⟩

The **Lawson approximation** gives the D-T reactivity as a function of temperature (valid ~5–100 keV):

$$\\langle \\sigma v \\rangle \\approx \\frac{3.68 \\times 10^{-18}}{T_{\\text{keV}}^{2/3}} \\exp\\!\\left(\\frac{-19.94}{T_{\\text{keV}}^{1/3}}\\right) \\quad [\\text{m}^3/\\text{s}]$$

The reactivity peaks around **T ≈ 70 keV** but is already substantial at 20 keV, which is the target operating temperature for tokamak reactors.

## Lawson Criterion

The **Lawson criterion** states that for ignition (fusion power ≥ heating power), the plasma must satisfy:

$$n \\cdot \\tau_E > 10^{20} \\text{ m}^{-3}\\text{s}$$

at \\(T \\approx 20\\) keV, where \\(\\tau_E\\) is the energy confinement time. This triple product \\(n T \\tau_E\\) is the key figure of merit for fusion devices.
`,
  starterCode: `import math

def dt_reactivity_m3_s(T_keV):
    # D-T reactivity using Lawson approximation, T in keV
    pass

def fusion_power_density_W_m3(n_m3, T_keV):
    # Fusion power density for equimolar D-T plasma
    pass

def lawson_parameter(n_m3, tau_s):
    # Lawson parameter n*tau in m^-3 s
    pass`,
  solution: `import math

def dt_reactivity_m3_s(T_keV):
    return 3.68e-18 / T_keV**(2/3) * math.exp(-19.94 / T_keV**(1/3))

def fusion_power_density_W_m3(n_m3, T_keV):
    E_fus = 2.818e-12
    n_D = n_m3 / 2
    n_T = n_m3 / 2
    sigma_v = dt_reactivity_m3_s(T_keV)
    return n_D * n_T * sigma_v * E_fus

def lawson_parameter(n_m3, tau_s):
    return n_m3 * tau_s`,
  tests: [
    {
      name: "D-T reactivity at 20 keV (near-optimal operating temperature)",
      expected: "3.2224e-22\n",
      code: `{{FUNC}}
print(f"{dt_reactivity_m3_s(20):.4e}")`,
    },
    {
      name: "D-T reactivity at 10 keV",
      expected: "7.5795e-23\n",
      code: `{{FUNC}}
print(f"{dt_reactivity_m3_s(10):.4e}")`,
    },
    {
      name: "Fusion power density at n=1e20 m^-3, T=20 keV",
      expected: "2.2702e+06\n",
      code: `{{FUNC}}
print(f"{fusion_power_density_W_m3(1e20, 20):.4e}")`,
    },
    {
      name: "Lawson parameter n*tau",
      expected: "3.0000e+20\n",
      code: `{{FUNC}}
print(f"{lawson_parameter(1e20, 3.0):.4e}")`,
    },
  ],
};
