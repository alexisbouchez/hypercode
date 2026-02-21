import type { Lesson } from "../../types";

export const proteinFolding: Lesson = {
  id: "protein-folding",
  title: "Protein Folding Thermodynamics",
  chapterId: "molecular",
  content: `# Protein Folding Thermodynamics

## The Folding Problem

Proteins spontaneously collapse from disordered polypeptide chains into precise three-dimensional structures. This process is driven by thermodynamics: the folded state minimizes the **Gibbs free energy**.

## Two-State Model

Most small globular proteins fold cooperatively through a **two-state equilibrium**:

$$\\text{Unfolded} \\rightleftharpoons \\text{Folded}$$

The free energy of folding is:

$$\\Delta G = \\Delta H - T \\Delta S$$

- **ΔH** < 0: folding is enthalpically favorable (hydrogen bonds, van der Waals)
- **ΔS** < 0: folding reduces conformational entropy (unfavorable)
- The balance determines whether the protein is folded at a given temperature

## Equilibrium Constant & Fraction Folded

$$K_{\\text{fold}} = \\exp\\left(-\\frac{\\Delta G}{RT}\\right)$$

$$f_{\\text{folded}} = \\frac{K}{1 + K}$$

## Melting Temperature

At **T_m**, ΔG = 0, so ΔH = T_m · ΔS:

$$T_m = \\frac{\\Delta H}{\\Delta S}$$

## Urea Denaturation

Chemical denaturants like urea unfold proteins by weakening hydrophobic interactions. The **linear extrapolation model** describes how stability depends on denaturant concentration **c**:

$$\\Delta G_{\\text{unfold}}(c) = \\Delta G_{\\text{unfold}}^{\\text{H}_2\\text{O}} - m \\cdot c$$

- **ΔG_unfold** > 0 means the protein is folded (unfolding is unfavorable)
- **m** = m-value (kJ mol⁻¹ M⁻¹): sensitivity of stability to denaturant
- **C_m** = midpoint concentration where ΔG_unfold = 0: C_m = ΔG_H₂O / m

The fraction folded given ΔG_unfold is:

$$f_{\\text{folded}} = \\frac{1}{1 + \\exp(-\\Delta G_{\\text{unfold}} / RT)}$$

## Your Task

Implement three functions:

1. **\`fraction_folded(delta_H_kJ_mol, delta_S_J_mol_K, T_K)\`** — Fraction of folded protein (ΔH in kJ/mol, ΔS in J/mol/K, T in K)
2. **\`melting_temperature_K(delta_H_kJ_mol, delta_S_J_mol_K)\`** — Melting temperature in Kelvin
3. **\`fraction_folded_urea(c_urea_M, delta_G_H2O_kJ_mol, m_kJ_mol_M, T_K=298)\`** — Fraction folded at given urea concentration (delta_G_H2O is ΔG_unfolding in water, positive = stable protein)
`,
  starterCode: `import math

def fraction_folded(delta_H_kJ_mol, delta_S_J_mol_K, T_K):
    # R = 8.314 J/mol/K
    # dG_fold = delta_H*1000 - T*delta_S  (in J/mol)
    # K_fold = exp(-dG_fold / (R*T))
    # f = K / (1 + K)
    pass

def melting_temperature_K(delta_H_kJ_mol, delta_S_J_mol_K):
    # T_m = delta_H * 1000 / delta_S
    pass

def fraction_folded_urea(c_urea_M, delta_G_H2O_kJ_mol, m_kJ_mol_M, T_K=298):
    # dG_unfold = (delta_G_H2O - m * c_urea) * 1000  (in J/mol)
    # K_unfold = exp(-dG_unfold / (R*T))
    # f_folded = 1 / (1 + K_unfold)
    pass
`,
  solution: `import math

def fraction_folded(delta_H_kJ_mol, delta_S_J_mol_K, T_K):
    R = 8.314
    dG_fold = delta_H_kJ_mol * 1000 - T_K * delta_S_J_mol_K
    K_fold = math.exp(-dG_fold / (R * T_K))
    return K_fold / (1 + K_fold)

def melting_temperature_K(delta_H_kJ_mol, delta_S_J_mol_K):
    return delta_H_kJ_mol * 1000 / delta_S_J_mol_K

def fraction_folded_urea(c_urea_M, delta_G_H2O_kJ_mol, m_kJ_mol_M, T_K=298):
    R = 8.314
    dG_unfold = (delta_G_H2O_kJ_mol - m_kJ_mol_M * c_urea_M) * 1000
    K_unfold = math.exp(-dG_unfold / (R * T_K))
    return 1 / (1 + K_unfold)
`,
  tests: [
    {
      name: "Fraction folded at 298 K (ΔH=-200 kJ/mol, ΔS=-600 J/mol/K)",
      expected: "0.9998\n",
      code: `{{FUNC}}
print(f"{fraction_folded(-200, -600, 298):.4f}")`,
    },
    {
      name: "Melting temperature (ΔH=-200 kJ/mol, ΔS=-600 J/mol/K)",
      expected: "333.33\n",
      code: `{{FUNC}}
print(f"{melting_temperature_K(-200, -600):.2f}")`,
    },
    {
      name: "Fraction folded in water (ΔG_unfold=+20 kJ/mol, m=4 kJ/mol/M)",
      expected: "0.9997\n",
      code: `{{FUNC}}
print(f"{fraction_folded_urea(0, 20, 4):.4f}")`,
    },
    {
      name: "Fraction folded at midpoint concentration C_m=5 M urea",
      expected: "0.5000\n",
      code: `{{FUNC}}
print(f"{fraction_folded_urea(5, 20, 4):.4f}")`,
    },
  ],
};
