import type { Lesson } from "../../types";

export const superconductivity: Lesson = {
  id: "superconductivity",
  title: "Superconductivity",
  chapterId: "quantum-phenomena",
  content: `# Superconductivity

**Superconductivity** is a macroscopic quantum phenomenon in which a material's electrical resistance drops to exactly zero below a critical temperature T_c, and magnetic flux is expelled from its interior (Meissner effect).

## BCS Energy Gap

The **BCS theory** (Bardeen–Cooper–Schrieffer, 1957) explains superconductivity via Cooper pairs — electron pairs bound by phonon-mediated attraction. At T = 0, the energy gap is:

$$\\Delta(0) = 1.764 \\, k_B T_c$$

This gap must be broken to create quasiparticle excitations. Converting to eV: Δ(eV) = Δ(J) / e.

## London Penetration Depth

Magnetic fields are screened from the superconductor interior over the **London penetration depth**:

$$\\lambda_L = \\sqrt{\\frac{m_e}{\\mu_0 n_s e^2}}$$

where n_s is the density of Cooper-pair electrons (superfluid density). Typical values: 20–500 nm.

## Coherence Length and GL Parameter

The **coherence length** ξ is the spatial scale over which the superconducting order parameter varies. The **Ginzburg-Landau parameter** distinguishes two types:

$$\\kappa = \\frac{\\lambda_L}{\\xi}$$

- **Type I**: κ < 1/√2 ≈ 0.707 (e.g., aluminum, tin) — sharp transition, full Meissner effect
- **Type II**: κ > 1/√2 (e.g., niobium, YBCO) — allows partial flux penetration as vortices

## Your Task

\`\`\`python
import math

def bcs_gap_J(T_c_K):
    # Delta = 1.764 * k_B * T_c, k_B = 1.381e-23
    pass

def bcs_gap_eV(T_c_K):
    # Convert bcs_gap_J to eV using e = 1.602e-19
    pass

def london_penetration_depth_m(n_s_m3):
    # lambda_L = sqrt(m_e / (mu0 * n_s * e^2))
    # m_e=9.109e-31, mu0=4*pi*1e-7, e=1.602e-19
    pass

def gl_parameter(lambda_L_m, xi_m):
    # kappa = lambda_L / xi
    pass
\`\`\`
`,
  starterCode: `import math

def bcs_gap_J(T_c_K):
    # Delta(0) = 1.764 * k_B * T_c, k_B = 1.381e-23
    pass

def bcs_gap_eV(T_c_K):
    # Convert bcs_gap_J to eV using e = 1.602e-19
    pass

def london_penetration_depth_m(n_s_m3):
    # lambda_L = sqrt(m_e / (mu0 * n_s * e^2))
    # m_e = 9.109e-31, mu0 = 4*pi*1e-7, e = 1.602e-19
    pass

def gl_parameter(lambda_L_m, xi_m):
    # kappa = lambda_L / xi
    pass
`,
  solution: `import math

def bcs_gap_J(T_c_K):
    k_B = 1.381e-23
    return 1.764 * k_B * T_c_K

def bcs_gap_eV(T_c_K):
    e = 1.602e-19
    return bcs_gap_J(T_c_K) / e

def london_penetration_depth_m(n_s_m3):
    m_e = 9.109e-31
    mu0 = 4 * math.pi * 1e-7
    e = 1.602e-19
    return math.sqrt(m_e / (mu0 * n_s_m3 * e**2))

def gl_parameter(lambda_L_m, xi_m):
    return lambda_L_m / xi_m
`,
  tests: [
    {
      name: "BCS energy gap for niobium (T_c=9.2K) in eV",
      expected: "1.3990e-03\n",
      code: `{{FUNC}}
print(f"{bcs_gap_eV(9.2):.4e}")`,
    },
    {
      name: "BCS energy gap for aluminum (T_c=1.2K) in eV",
      expected: "1.8248e-04\n",
      code: `{{FUNC}}
print(f"{bcs_gap_eV(1.2):.4e}")`,
    },
    {
      name: "London penetration depth for n_s=8.49e28 m⁻³",
      expected: "1.8240e-08\n",
      code: `{{FUNC}}
print(f"{london_penetration_depth_m(8.49e28):.4e}")`,
    },
    {
      name: "GL parameter for niobium (lambda=39nm, xi=38nm) — Type II",
      expected: "1.0263\n",
      code: `{{FUNC}}
print(f"{gl_parameter(3.9e-8, 3.8e-8):.4f}")`,
    },
  ],
};
