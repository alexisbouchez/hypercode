import type { Lesson } from "../../types";

export const meissnerEffect: Lesson = {
  id: "meissner-effect",
  title: "Meissner Effect",
  chapterId: "quantum-phenomena",
  content: `# Meissner Effect

The **Meissner effect** is the expulsion of magnetic flux from the interior of a superconductor when it transitions below its critical temperature T_c. It distinguishes a superconductor from a perfect conductor.

## London Penetration Depth

The magnetic field inside a superconductor decays exponentially from the surface:

$$B(x) = B_0 \\, e^{-x/\\lambda_L}$$

The **London penetration depth** λ_L is:

$$\\lambda_L = \\sqrt{\\frac{m_e}{\\mu_0 n_s e^2}}$$

where n_s is the superfluid density (superconducting electron density).

## Critical Field (Type I)

A type-I superconductor loses superconductivity above the **critical field**:

$$H_c(T) = H_{c0}\\left[1 - \\left(\\frac{T}{T_c}\\right)^2\\right]$$

## Type II Superconductors

Type-II superconductors have two critical fields:
- **Lower critical field** H_c1 — flux vortices begin to penetrate
- **Upper critical field** H_c2 — superconductivity is destroyed:

$$H_{c2} = \\frac{\\Phi_0}{2\\pi \\mu_0 \\xi^2}$$

where Φ₀ = h/(2e) = 2.068×10⁻¹⁵ Wb is the **flux quantum** and ξ is the **coherence length**.

## Implement

\`\`\`python
def london_depth_m(n_s_m3):
    # Returns London penetration depth in metres
    ...

def field_inside_superconductor_T(B0_T, x_m, lambda_L_m):
    # Returns B(x) in Tesla
    ...

def critical_field_H_c_A_m(H_c0_A_m, T_K, T_c_K):
    # Returns H_c(T) in A/m
    ...

def upper_critical_field_H_c2_A_m(xi_m):
    # Returns H_c2 in A/m
    ...
\`\`\`
`,
  starterCode: `import math

def london_depth_m(n_s_m3):
    pass

def field_inside_superconductor_T(B0_T, x_m, lambda_L_m):
    pass

def critical_field_H_c_A_m(H_c0_A_m, T_K, T_c_K):
    pass

def upper_critical_field_H_c2_A_m(xi_m):
    pass
`,
  solution: `import math

def london_depth_m(n_s_m3):
    m_e = 9.109e-31
    mu0 = 4 * math.pi * 1e-7
    e = 1.602e-19
    return math.sqrt(m_e / (mu0 * n_s_m3 * e**2))

def field_inside_superconductor_T(B0_T, x_m, lambda_L_m):
    return B0_T * math.exp(-x_m / lambda_L_m)

def critical_field_H_c_A_m(H_c0_A_m, T_K, T_c_K):
    return H_c0_A_m * (1 - (T_K / T_c_K)**2)

def upper_critical_field_H_c2_A_m(xi_m):
    mu0 = 4 * math.pi * 1e-7
    Phi0 = 2.068e-15
    return Phi0 / (2 * math.pi * mu0 * xi_m**2)
`,
  tests: [
    {
      name: "London penetration depth for copper electron density",
      expected: "1.8240e-08\n",
      code: `{{FUNC}}\nprint(f"{london_depth_m(8.49e28):.4e}")`,
    },
    {
      name: "Magnetic field at 50 nm depth with λ_L=39 nm",
      expected: "0.2775\n",
      code: `{{FUNC}}\nprint(f"{field_inside_superconductor_T(1.0, 5e-8, 3.9e-8):.4f}")`,
    },
    {
      name: "Critical field of Nb at T=4 K (T_c=9.2 K, H_c0=160 kA/m)",
      expected: "129754.25\n",
      code: `{{FUNC}}\nprint(f"{critical_field_H_c_A_m(160000, 4.0, 9.2):.2f}")`,
    },
    {
      name: "Upper critical field H_c2 for Nb with coherence length 38 nm",
      expected: "1.8138e+05\n",
      code: `{{FUNC}}\nprint(f"{upper_critical_field_H_c2_A_m(3.8e-8):.4e}")`,
    },
  ],
};
