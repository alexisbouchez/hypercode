import type { Lesson } from "../../types";

export const wlc: Lesson = {
  id: "wlc",
  title: "DNA Mechanics: Worm-Like Chain",
  chapterId: "molecular",
  content: `# DNA Mechanics: Worm-Like Chain

## Semiflexible Polymers

DNA is neither a rigid rod nor a freely flexible string — it is a **semiflexible polymer**. Its mechanical properties are captured by the **worm-like chain (WLC)** model, which characterizes flexibility through the **persistence length** L_p.

## Persistence Length

The persistence length is the distance over which thermal fluctuations bend the polymer significantly:

- **dsDNA:** L_p ≈ 50 nm (~150 bp)
- **ssDNA:** L_p ≈ 1–3 nm
- **Actin:** L_p ≈ 17 μm
- **Microtubule:** L_p ≈ 5 mm

## DNA Contour Length

Each base pair contributes 0.34 nm to the contour length:

$$L = N_{bp} \\times 0.34 \\text{ nm/bp}$$

A 1 kbp DNA fragment has L = 340 nm, but fits in a cell because it is highly coiled.

## End-to-End Distance (Gaussian Regime)

For polymers much longer than L_p (L >> L_p), the mean squared end-to-end distance is:

$$\\langle r^2 \\rangle = 2 L_p L$$

$$r_{\\text{rms}} = \\sqrt{2 L_p L}$$

For 1 kbp DNA: r_rms = √(2 × 50 × 340) ≈ 184 nm (much smaller than L = 340 nm).

## Marko-Siggia Force-Extension

When DNA is stretched by an external force F, the force-extension relationship is (Marko & Siggia, 1995):

$$F = \\frac{k_B T}{L_p} \\left[ \\frac{1}{4\\left(1 - x/L\\right)^2} - \\frac{1}{4} + \\frac{x}{L} \\right]$$

- **x** = end-to-end extension (0 ≤ x < L)
- **L** = contour length
- **L_p** = persistence length

This formula diverges as x → L (infinite force required for full extension) and reduces to Hookean behavior for small extensions.

## Your Task

Implement three functions:

1. **\`dna_contour_length_nm(N_bp)\`** — contour length from number of base pairs
2. **\`end_to_end_rms_nm(L_nm, L_p_nm=50)\`** — RMS end-to-end distance (Gaussian regime)
3. **\`wlc_force_pN(x_nm, L_nm, L_p_nm=50, T_K=310)\`** — stretching force in picoNewtons

For \`wlc_force_pN\`: convert lengths to meters, compute force in Newtons, then return in pN (1 pN = 10⁻¹² N). Use k_B = 1.381 × 10⁻²³ J/K.
`,
  starterCode: `import math

def dna_contour_length_nm(N_bp):
    # L = N_bp * 0.34 nm/bp
    pass

def end_to_end_rms_nm(L_nm, L_p_nm=50):
    # r_rms = sqrt(2 * L_p * L)
    pass

def wlc_force_pN(x_nm, L_nm, L_p_nm=50, T_K=310):
    # F = (k_B * T / L_p) * (1/(4*(1 - x/L)^2) - 1/4 + x/L)
    # k_B = 1.381e-23, convert L_p to meters, return force in pN
    pass
`,
  solution: `import math

def dna_contour_length_nm(N_bp):
    return N_bp * 0.34

def end_to_end_rms_nm(L_nm, L_p_nm=50):
    return math.sqrt(2 * L_p_nm * L_nm)

def wlc_force_pN(x_nm, L_nm, L_p_nm=50, T_K=310):
    k_B = 1.381e-23
    x_frac = x_nm / L_nm
    F_N = (k_B * T_K / (L_p_nm * 1e-9)) * (1 / (4 * (1 - x_frac)**2) - 1/4 + x_frac)
    return F_N / 1e-12
`,
  tests: [
    {
      name: "Contour length of 1000 bp DNA",
      expected: "340.00\n",
      code: `{{FUNC}}
print(f"{dna_contour_length_nm(1000):.2f}")`,
    },
    {
      name: "RMS end-to-end distance for 1 kbp DNA (L=340 nm, L_p=50 nm)",
      expected: "184.39\n",
      code: `{{FUNC}}
print(f"{end_to_end_rms_nm(340):.2f}")`,
    },
    {
      name: "WLC force at x=200 nm extension (L=340 nm, L_p=50 nm)",
      expected: "0.1552\n",
      code: `{{FUNC}}
print(f"{wlc_force_pN(200, 340):.4f}")`,
    },
    {
      name: "WLC force near full extension: x=300 nm out of L=340 nm",
      expected: "1.6007\n",
      code: `{{FUNC}}
print(f"{wlc_force_pN(300, 340):.4f}")`,
    },
  ],
};
