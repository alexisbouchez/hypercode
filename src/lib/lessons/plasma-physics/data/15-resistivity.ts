import type { Lesson } from "../../types";

export const resistivity: Lesson = {
  id: "resistivity",
  title: "Plasma Resistivity",
  chapterId: "diagnostics",
  content: `# Plasma Resistivity

Unlike ordinary conductors, plasma resistivity **decreases** with increasing temperature — hotter plasmas are better conductors. This is because Coulomb collisions become rarer at higher thermal speeds.

## Spitzer Resistivity

The **Spitzer resistivity** is the classical result for fully ionized plasma:

$$\\eta_{\\text{Spitzer}} \\approx 5.2 \\times 10^{-5} \\frac{Z \\ln\\Lambda}{T^{3/2}} \\quad [\\Omega \\cdot \\text{m}]$$

where:
- \\(T\\) is in Kelvin
- \\(Z\\) is the ion charge number
- \\(\\ln\\Lambda \\approx 10\\text{–}20\\) is the **Coulomb logarithm**, accounting for the range of impact parameters in Coulomb collisions

The \\(T^{-3/2}\\) scaling means that a plasma at fusion temperatures (\\(\\sim 10^8\\) K) has resistivity \\(\\sim 10^{10}\\) times lower than at 10,000 K.

## Electrical Conductivity

The plasma conductivity is simply:

$$\\sigma = \\frac{1}{\\eta}$$

At fusion temperatures, plasma conductivity rivals that of copper.

## Mean Free Path

The electron-ion collision frequency is:

$$\\nu_{ei} = 2.91 \\times 10^{-12} \\frac{n \\ln\\Lambda}{T^{3/2}}$$

The electron mean free path between collisions is:

$$\\lambda_{\\text{mfp}} = \\frac{v_{\\text{th}}}{\\nu_{ei}}, \\quad v_{\\text{th}} = \\sqrt{\\frac{k_B T}{m_e}}$$

At fusion temperatures and densities, \\(\\lambda_{\\text{mfp}}\\) can be enormous — electrons travel hundreds of kilometers between collisions.
`,
  starterCode: `import math

def spitzer_resistivity_ohm_m(T_K, ln_lambda=10, Z=1):
    # Spitzer resistivity in Ohm*m
    pass

def plasma_conductivity_S_m(T_K, ln_lambda=10, Z=1):
    # Plasma electrical conductivity in S/m
    pass

def mean_free_path_m(n_m3, T_K):
    # Electron mean free path in meters (uses ln_lambda=10)
    pass`,
  solution: `import math

def spitzer_resistivity_ohm_m(T_K, ln_lambda=10, Z=1):
    return 5.2e-5 * Z * ln_lambda / T_K**1.5

def plasma_conductivity_S_m(T_K, ln_lambda=10, Z=1):
    return 1.0 / spitzer_resistivity_ohm_m(T_K, ln_lambda, Z)

def mean_free_path_m(n_m3, T_K):
    k_B = 1.381e-23
    m_e = 9.109e-31
    ln_lambda = 10
    v_th = math.sqrt(k_B * T_K / m_e)
    nu_ei = 2.91e-12 * n_m3 * ln_lambda / T_K**1.5
    return v_th / nu_ei`,
  tests: [
    {
      name: "Spitzer resistivity at T=10,000 K",
      expected: "5.2000e-10\n",
      code: `{{FUNC}}
print(f"{spitzer_resistivity_ohm_m(10000):.4e}")`,
    },
    {
      name: "Spitzer resistivity at T=1e7 K (fusion-relevant)",
      expected: "1.6444e-14\n",
      code: `{{FUNC}}
print(f"{spitzer_resistivity_ohm_m(1e7):.4e}")`,
    },
    {
      name: "Plasma conductivity at T=1e7 K",
      expected: "6.0813e+13\n",
      code: `{{FUNC}}
print(f"{plasma_conductivity_S_m(1e7):.4e}")`,
    },
    {
      name: "Electron mean free path at n=1e20 m^-3, T=1e7 K",
      expected: "1.3380e+08\n",
      code: `{{FUNC}}
print(f"{mean_free_path_m(1e20, 1e7):.4e}")`,
    },
  ],
};
