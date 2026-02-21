import type { Lesson } from "../../types";

export const forceSpectroscopy: Lesson = {
  id: "force-spectroscopy",
  title: "Single-Molecule Force Spectroscopy",
  chapterId: "biophysics-methods",
  content: `# Single-Molecule Force Spectroscopy

## Probing Biology One Molecule at a Time

Single-molecule force spectroscopy (SMFS) uses atomic force microscopy (AFM) or optical tweezers to apply and measure piconewton forces on individual molecules — proteins, DNA, receptor-ligand bonds. This reveals mechanical properties invisible to bulk experiments.

## Bell-Evans Model for Bond Rupture

When a bond is pulled apart at a constant **loading rate r** (force increase per unit time, pN/s), the most probable **rupture force F*** is:

$$F^* = \\frac{k_B T}{x_\\beta} \\ln\\left(\\frac{r \\, x_\\beta}{k_{\\text{off}} \\, k_B T}\\right)$$

- **x_β** = distance to the transition state along the reaction coordinate (~0.1–1 nm)
- **k_off** = spontaneous off-rate at zero force (s⁻¹)
- Higher loading rates → higher rupture forces (the bond has less time to thermally escape)
- F* increases **logarithmically** with loading rate

## Optical Trap Fluctuations

A trapped bead behaves as a **harmonic oscillator**. By the equipartition theorem, thermal energy partitions equally into each degree of freedom:

$$\\frac{1}{2} k_{\\text{trap}} \\langle x^2 \\rangle = \\frac{1}{2} k_B T$$

The root-mean-square positional fluctuation is:

$$\\sigma = \\sqrt{\\frac{k_B T}{k_{\\text{trap}}}}$$

Stiff traps (large k_trap) have small fluctuations — essential for detecting sub-nanometer displacements.

## Stokes Drag Force

When a bead moves through viscous fluid at velocity **v**, the drag force is:

$$F_{\\text{drag}} = 6\\pi \\eta r v$$

where **η** = fluid viscosity (≈ 1 mPa·s for water) and **r** = bead radius. This drag limits the speed of optical tweezers experiments.

## Your Task

Implement three functions:

1. **\`rupture_force_pN(r_pN_s, x_beta_nm, k_off_s=1.0, T_K=298)\`** — Bell-Evans most probable rupture force in pN
2. **\`trap_fluctuation_nm(k_trap_pN_nm, T_K=298)\`** — RMS bead displacement in an optical trap in nm
3. **\`stokes_drag_pN(r_nm, v_nm_s, eta_Pa_s=1e-3)\`** — Stokes drag force in pN
`,
  starterCode: `import math

def rupture_force_pN(r_pN_s, x_beta_nm, k_off_s=1.0, T_K=298):
    # k_B = 1.381e-23 J/K
    # Convert: x_beta to meters, r to N/s
    # F = (k_B*T/x_beta) * ln(r*x_beta / (k_off*k_B*T))
    # Return result in pN
    pass

def trap_fluctuation_nm(k_trap_pN_nm, T_K=298):
    # k_B = 1.381e-23 J/K
    # k_trap in N/m: 1 pN/nm = 1e-12/1e-9 = 1e-3 N/m
    # sigma = sqrt(k_B*T / k_trap)
    # Return result in nm
    pass

def stokes_drag_pN(r_nm, v_nm_s, eta_Pa_s=1e-3):
    # F = 6*pi*eta*r*v
    # Convert r (nm→m), v (nm/s→m/s), return force in pN
    pass
`,
  solution: `import math

def rupture_force_pN(r_pN_s, x_beta_nm, k_off_s=1.0, T_K=298):
    k_B = 1.381e-23
    x_beta_m = x_beta_nm * 1e-9
    r_N_s = r_pN_s * 1e-12
    F_N = (k_B * T_K / x_beta_m) * math.log(r_N_s * x_beta_m / (k_off_s * k_B * T_K))
    return F_N / 1e-12

def trap_fluctuation_nm(k_trap_pN_nm, T_K=298):
    k_B = 1.381e-23
    k_trap_N_m = k_trap_pN_nm * 1e-3
    sigma_m = math.sqrt(k_B * T_K / k_trap_N_m)
    return sigma_m * 1e9

def stokes_drag_pN(r_nm, v_nm_s, eta_Pa_s=1e-3):
    r_m = r_nm * 1e-9
    v_m_s = v_nm_s * 1e-9
    F_N = 6 * math.pi * eta_Pa_s * r_m * v_m_s
    return F_N / 1e-12
`,
  tests: [
    {
      name: "Bell-Evans rupture force at loading rate 1000 pN/s (x_β=0.3 nm, k_off=1/s)",
      expected: "58.8369\n",
      code: `{{FUNC}}
print(f"{rupture_force_pN(1000, 0.3):.4f}")`,
    },
    {
      name: "Bell-Evans rupture force at 10x faster loading rate (10000 pN/s)",
      expected: "90.4236\n",
      code: `{{FUNC}}
print(f"{rupture_force_pN(10000, 0.3):.4f}")`,
    },
    {
      name: "Optical trap fluctuation at k_trap=0.1 pN/nm",
      expected: "6.4151\n",
      code: `{{FUNC}}
print(f"{trap_fluctuation_nm(0.1):.4f}")`,
    },
    {
      name: "Stokes drag on 500 nm bead moving at 1 μm/s in water",
      expected: "0.0094\n",
      code: `{{FUNC}}
print(f"{stokes_drag_pN(500, 1000):.4f}")`,
    },
  ],
};
