import type { Lesson } from "../../types";

export const radiationBiology: Lesson = {
  id: "radiation-biology",
  title: "Radiation Biology & Dosimetry",
  chapterId: "biophysics-methods",
  content: `# Radiation Biology & Dosimetry

## Ionizing Radiation in Biology

Ionizing radiation — X-rays, gamma rays, alpha particles, and beta particles — deposits energy in biological tissue, causing DNA damage, free radical formation, and cell death. Quantifying radiation exposure is essential in medical physics, radiotherapy, and radiation protection.

## Absorbed Dose

The **absorbed dose D** is the energy deposited per unit mass of tissue:

$$D = \\frac{\\text{Energy}}{\\text{Mass}} \\quad [\\text{Gray (Gy) = J/kg}]$$

## Equivalent Dose

Different radiation types cause different degrees of biological damage. The **equivalent dose H** accounts for this via the radiation weighting factor **W_R**:

$$H = D \\times W_R \\quad [\\text{Sievert (Sv)}]$$

| Radiation Type | W_R |
|---------------|-----|
| X-rays / gamma rays | 1 |
| Beta particles | 1 |
| Protons | 2 |
| Alpha particles | 20 |
| Heavy ions | 20 |

## Effective Dose

The **effective dose E** sums over all irradiated tissues, weighted by tissue sensitivity factor **W_T**:

$$E = \\sum_T W_T \\cdot H_T \\quad [\\text{Sv}]$$

## Linear-Quadratic (LQ) Model

The LQ model describes cell survival fraction **S** after dose **D**:

$$S = \\exp\\left(-(\\alpha D + \\beta D^2)\\right)$$

- **α** (Gy⁻¹): linear component — DNA double-strand breaks from single tracks
- **β** (Gy⁻²): quadratic component — breaks from two independent tracks
- **α/β ratio**: characteristic dose where linear and quadratic contributions are equal; typically **10 Gy** for early-responding (tumor) tissues and **3 Gy** for late-responding (normal) tissues

## D₅₀: Dose for 50% Survival

Setting S = 0.5 and solving α·D + β·D² = ln 2:

$$D_{50} = \\frac{-\\alpha + \\sqrt{\\alpha^2 + 4\\beta \\ln 2}}{2\\beta}$$

## Your Task

Implement three functions:

1. **\`equivalent_dose_Sv(D_Gy, W_R=1)\`** — Equivalent dose in Sv
2. **\`cell_survival_LQ(D_Gy, alpha=0.3, beta=0.03)\`** — Cell survival fraction from the LQ model
3. **\`d50_LQ_Gy(alpha=0.3, beta=0.03)\`** — Dose (Gy) at which 50% of cells survive
`,
  starterCode: `import math

def equivalent_dose_Sv(D_Gy, W_R=1):
    # H = D * W_R
    pass

def cell_survival_LQ(D_Gy, alpha=0.3, beta=0.03):
    # S = exp(-(alpha*D + beta*D^2))
    pass

def d50_LQ_Gy(alpha=0.3, beta=0.03):
    # Solve alpha*D + beta*D^2 = ln(2) using quadratic formula
    pass
`,
  solution: `import math

def equivalent_dose_Sv(D_Gy, W_R=1):
    return D_Gy * W_R

def cell_survival_LQ(D_Gy, alpha=0.3, beta=0.03):
    return math.exp(-(alpha * D_Gy + beta * D_Gy**2))

def d50_LQ_Gy(alpha=0.3, beta=0.03):
    return (-alpha + math.sqrt(alpha**2 + 4 * beta * math.log(2))) / (2 * beta)
`,
  tests: [
    {
      name: "Equivalent dose for 2 Gy X-ray treatment (W_R=1)",
      expected: "2.0000\n",
      code: `{{FUNC}}
print(f"{equivalent_dose_Sv(2.0):.4f}")`,
    },
    {
      name: "Equivalent dose for 0.1 Gy alpha radiation (W_R=20)",
      expected: "2.0000\n",
      code: `{{FUNC}}
print(f"{equivalent_dose_Sv(0.1, W_R=20):.4f}")`,
    },
    {
      name: "Cell survival at 2 Gy (alpha=0.3, beta=0.03)",
      expected: "0.4868\n",
      code: `{{FUNC}}
print(f"{cell_survival_LQ(2.0):.4f}")`,
    },
    {
      name: "Cell survival at 6 Gy (alpha=0.3, beta=0.03)",
      expected: "5.6135e-02\n",
      code: `{{FUNC}}
print(f"{cell_survival_LQ(6.0):.4e}")`,
    },
  ],
};
