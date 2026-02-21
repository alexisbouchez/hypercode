import type { Lesson } from "../../types";

export const beerLambert: Lesson = {
  id: "beer-lambert",
  title: "Beer-Lambert Law",
  chapterId: "biophysics-methods",
  content: `# Beer-Lambert Law

## Optical Absorption

When light passes through a solution, molecules absorb photons at characteristic wavelengths. The **Beer-Lambert Law** relates absorbance to concentration:

$$A = \\varepsilon \\cdot c \\cdot l$$

Where:
- **A** — absorbance (dimensionless, also called optical density OD)
- **ε** — molar extinction coefficient (M⁻¹·cm⁻¹)
- **c** — molar concentration (mol/L = M)
- **l** — path length (cm); cuvettes are typically 1 cm

## Transmittance

**Transmittance** T is the fraction of light that passes through:

$$T = \\frac{I}{I_0} = 10^{-A}$$

Therefore: A = −log₁₀(T)

## Common Extinction Coefficients

| Molecule | λ (nm) | ε (M⁻¹cm⁻¹) |
|----------|---------|--------------|
| Tryptophan | 280 | 5,500 |
| Tyrosine | 274 | 1,405 |
| NADH | 340 | 6,220 |
| Heme (oxyhemoglobin) | 415 | 125,000 |

## Protein Concentration (A280)

Proteins absorb at 280 nm due to Trp and Tyr residues. Using a protein-specific ε:

$$c_{\\text{mg/mL}} = \\frac{A_{280}}{\\varepsilon_{\\text{mg/mL}}}$$

## DNA Concentration (A260)

Double-stranded DNA: 1 A260 unit ≈ 50 μg/mL.
Single-stranded DNA: 1 A260 unit ≈ 33 μg/mL.

The **A260/A280 ratio** indicates purity: pure DNA ≈ 1.8, pure RNA ≈ 2.0.

## Functions to Implement

- \`absorbance(epsilon_M_cm, c_M, l_cm=1)\` — compute A = ε·c·l
- \`transmittance(A)\` — compute T = 10^(−A)
- \`concentration_from_absorbance(A, epsilon_M_cm, l_cm=1)\` — solve for c
- \`absorbance_from_transmittance(T)\` — compute A = −log₁₀(T)
`,
  starterCode: `import math

def absorbance(epsilon_M_cm, c_M, l_cm=1):
    pass

def transmittance(A):
    pass

def concentration_from_absorbance(A, epsilon_M_cm, l_cm=1):
    pass

def absorbance_from_transmittance(T):
    pass
`,
  solution: `import math

def absorbance(epsilon_M_cm, c_M, l_cm=1):
    return epsilon_M_cm * c_M * l_cm

def transmittance(A):
    return 10**(-A)

def concentration_from_absorbance(A, epsilon_M_cm, l_cm=1):
    return A / (epsilon_M_cm * l_cm)

def absorbance_from_transmittance(T):
    return -math.log10(T)
`,
  tests: [
    {
      name: "absorbance of tryptophan at 0.1 mM",
      expected: "0.5500\n",
      code: `{{FUNC}}\nprint(f"{absorbance(5500, 1e-4, 1):.4f}")`,
    },
    {
      name: "transmittance at A=0.55",
      expected: "0.2818\n",
      code: `{{FUNC}}\nprint(f"{transmittance(0.5500):.4f}")`,
    },
    {
      name: "concentration_from_absorbance recovers original concentration",
      expected: "1.0000e-04\n",
      code: `{{FUNC}}\nprint(f"{concentration_from_absorbance(0.55, 5500, 1):.4e}")`,
    },
    {
      name: "absorbance_from_transmittance inverts transmittance",
      expected: "0.5501\n",
      code: `{{FUNC}}\nprint(f"{absorbance_from_transmittance(0.2818):.4f}")`,
    },
  ],
};
