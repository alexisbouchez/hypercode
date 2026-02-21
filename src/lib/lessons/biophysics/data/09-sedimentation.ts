import type { Lesson } from "../../types";

export const sedimentation: Lesson = {
  id: "sedimentation",
  title: "Sedimentation & Ultracentrifugation",
  chapterId: "biophysics-methods",
  content: `# Sedimentation & Ultracentrifugation

## Ultracentrifugation

Ultracentrifuges spin samples at 10,000–100,000 rpm, generating forces up to 600,000 × g. This separates macromolecules by size, shape, and density.

## Sedimentation Coefficient

The sedimentation coefficient **s** describes how fast a particle moves per unit centrifugal force:

$$s = \\frac{m(1 - \\bar{v}\\rho)}{f}$$

Where:
- **m** — particle mass (kg)
- **v̄** — partial specific volume (~0.73 mL/g for proteins)
- **ρ** — solvent density (g/mL; 1.0 for water)
- **f** — translational friction coefficient (f = 6πηr for a sphere)
- **η** — solvent viscosity (1×10⁻³ Pa·s for water at 20°C)

The term (1 − v̄ρ) is the **buoyancy factor** — it accounts for solvent displacement.

## Svedberg Unit

1 S (Svedberg) = 10⁻¹³ s

Typical values: ribosomes (70S, 80S), proteins (1–20S).

## Svedberg Equation

Combining sedimentation (s) and diffusion (D) gives molecular weight:

$$M_{\\text{g/mol}} = \\frac{s \\cdot R \\cdot T}{D \\cdot (1 - \\bar{v}\\rho)} \\times 1000$$

where s is in seconds, R = 8.314 J·mol⁻¹·K⁻¹, D in m²/s. The result in kg/mol × 1000 = g/mol = Da.

## Pelleting Time

Time to move a particle from radius r₁ to r₂:

$$t = \\frac{\\ln(r_2/r_1)}{s \\cdot \\omega^2}$$

Where ω = 2π·rpm/60 (rad/s) is the angular velocity.

## Example Values

| Particle | s (S) | M (kDa) |
|----------|-------|---------|
| Myoglobin | 2.0 | 17 |
| BSA | 4.3 | 66 |
| IgG | 7.0 | 150 |
| 80S ribosome | ~80 | ~4,200 |

## Functions to Implement

- \`sedimentation_coefficient_S(M_Da, r_nm, v_bar=0.73, rho=1.0, eta=1e-3, T_K=293)\`
- \`svedberg_molecular_weight_Da(s_S, D_m2_s, v_bar=0.73, rho=1.0, T_K=293)\`
- \`pelleting_time_s(s_S, r1_m, r2_m, rpm)\`
`,
  starterCode: `import math

def sedimentation_coefficient_S(M_Da, r_nm, v_bar=0.73, rho=1.0, eta=1e-3, T_K=293):
    pass

def svedberg_molecular_weight_Da(s_S, D_m2_s, v_bar=0.73, rho=1.0, T_K=293):
    pass

def pelleting_time_s(s_S, r1_m, r2_m, rpm):
    pass
`,
  solution: `import math

def sedimentation_coefficient_S(M_Da, r_nm, v_bar=0.73, rho=1.0, eta=1e-3, T_K=293):
    Da_to_kg = 1.661e-27
    m = M_Da * Da_to_kg
    f = 6 * math.pi * eta * r_nm * 1e-9
    s_raw = m * (1 - v_bar * rho) / f
    return s_raw / 1e-13

def svedberg_molecular_weight_Da(s_S, D_m2_s, v_bar=0.73, rho=1.0, T_K=293):
    R = 8.314
    M_kg_per_mol = (s_S * 1e-13 * R * T_K) / (D_m2_s * (1 - v_bar * rho))
    return M_kg_per_mol * 1000

def pelleting_time_s(s_S, r1_m, r2_m, rpm):
    omega = 2 * math.pi * rpm / 60
    return math.log(r2_m / r1_m) / (s_S * 1e-13 * omega**2)
`,
  tests: [
    {
      name: "sedimentation_coefficient_S for BSA (66 kDa, r=3.5 nm)",
      expected: "4.49\n",
      code: `{{FUNC}}\nprint(f"{sedimentation_coefficient_S(66000, 3.5):.2f}")`,
    },
    {
      name: "sedimentation_coefficient_S for IgG (150 kDa, r=5.0 nm)",
      expected: "7.14\n",
      code: `{{FUNC}}\nprint(f"{sedimentation_coefficient_S(150000, 5.0):.2f}")`,
    },
    {
      name: "pelleting_time_s for 80S ribosome at 50,000 rpm",
      expected: "34394.40\n",
      code: `{{FUNC}}\nprint(f"{pelleting_time_s(4.3, 0.06, 0.09, 50000):.2f}")`,
    },
    {
      name: "svedberg_molecular_weight_Da from s=4.3S and D=5.9e-11 m²/s",
      expected: "6.5755e+04\n",
      code: `{{FUNC}}\nprint(f"{svedberg_molecular_weight_Da(4.3, 5.9e-11):.4e}")`,
    },
  ],
};
