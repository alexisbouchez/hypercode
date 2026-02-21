import type { Lesson } from "../../types";

export const electronHeatCapacity: Lesson = {
  id: "electron-heat-capacity",
  title: "Electronic Heat Capacity",
  chapterId: "crystal-structure",
  content: `# Electronic Heat Capacity

At low temperatures the **electronic contribution** to heat capacity dominates over the lattice (Debye) contribution. This is a direct consequence of the Pauli exclusion principle — only electrons near the Fermi level can be thermally excited.

## Sommerfeld Theory

The electronic heat capacity per unit volume is:

$$C_{\\text{el}} = \\gamma T$$

where γ is the **Sommerfeld coefficient**:

$$\\gamma = \\frac{\\pi^2 k_B^2}{3} D(E_F)$$

For a free-electron gas, the density of states at the Fermi level is:

$$D(E_F) = \\frac{3n}{2 E_F}$$

Substituting gives:

$$\\gamma = \\frac{\\pi^2 n k_B^2}{2 E_F}$$

## Fermi Energy (Free Electron Model)

$$E_F = \\frac{\\hbar^2}{2 m_e} \\left(3\\pi^2 n\\right)^{2/3}$$

## Molar Sommerfeld Coefficient

Experimentally, γ is often quoted in mJ/(mol·K²):

$$\\gamma_{\\text{mol}} = \\gamma \\cdot V_{\\text{mol}} \\times 1000 \\quad [\\text{mJ·mol}^{-1}\\text{K}^{-2}]$$

where V_mol = N_A / n is the molar volume.

## Physical Insight

At room temperature C_el ≈ γ × 300 K is much smaller than the Dulong–Petit lattice value of 3R ≈ 25 J/(mol·K), explaining why electrons do not contribute 3/2 k_B per electron as classical theory predicted.

## Implement

\`\`\`python
def sommerfeld_gamma_J_m3_K2(n_m3):
    # Returns γ in J/(m³·K²)
    ...

def electronic_heat_capacity_J_m3K(n_m3, T_K):
    # Returns C_el = γ T in J/(m³·K)
    ...

def sommerfeld_coefficient_mJ_mol_K2(n_m3):
    # Returns γ in mJ/(mol·K²)
    ...
\`\`\`
`,
  starterCode: `import math

def sommerfeld_gamma_J_m3_K2(n_m3):
    pass

def electronic_heat_capacity_J_m3K(n_m3, T_K):
    pass

def sommerfeld_coefficient_mJ_mol_K2(n_m3):
    pass
`,
  solution: `import math

def sommerfeld_gamma_J_m3_K2(n_m3):
    k_B = 1.381e-23
    hbar = 1.055e-34
    m_e = 9.109e-31
    E_F = (hbar**2 / (2 * m_e)) * (3 * math.pi**2 * n_m3)**(2/3)
    return math.pi**2 * n_m3 * k_B**2 / (2 * E_F)

def electronic_heat_capacity_J_m3K(n_m3, T_K):
    gamma = sommerfeld_gamma_J_m3_K2(n_m3)
    return gamma * T_K

def sommerfeld_coefficient_mJ_mol_K2(n_m3):
    N_A = 6.022e23
    gamma = sommerfeld_gamma_J_m3_K2(n_m3)
    V_mol = N_A / n_m3
    return gamma * V_mol * 1000
`,
  tests: [
    {
      name: "Sommerfeld coefficient γ for copper",
      expected: "7.0742e+01\n",
      code: `{{FUNC}}\nprint(f"{sommerfeld_gamma_J_m3_K2(8.49e28):.4e}")`,
    },
    {
      name: "Electronic heat capacity of copper at 300 K",
      expected: "2.1222e+04\n",
      code: `{{FUNC}}\nprint(f"{electronic_heat_capacity_J_m3K(8.49e28, 300):.4e}")`,
    },
    {
      name: "Electronic heat capacity of copper at 10 K",
      expected: "7.0742e+02\n",
      code: `{{FUNC}}\nprint(f"{electronic_heat_capacity_J_m3K(8.49e28, 10):.4e}")`,
    },
    {
      name: "Molar Sommerfeld coefficient for copper",
      expected: "0.5018\n",
      code: `{{FUNC}}\nprint(f"{sommerfeld_coefficient_mJ_mol_K2(8.49e28):.4f}")`,
    },
  ],
};
