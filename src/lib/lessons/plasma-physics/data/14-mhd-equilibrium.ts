import type { Lesson } from "../../types";

export const mhdEquilibrium: Lesson = {
  id: "mhd-equilibrium",
  title: "MHD Equilibrium",
  chapterId: "mhd",
  content: `# MHD Equilibrium

In magnetohydrodynamics (MHD), a plasma is in **equilibrium** when the pressure gradient force is balanced by the magnetic force:

$$\\nabla p = \\mathbf{J} \\times \\mathbf{B}$$

This fundamental relation constrains the geometry of magnetically confined plasmas.

## Z-Pinch: Bennett Relation

In a **z-pinch** (current flows along the z-axis), the magnetic field wraps around the plasma column. The Bennett relation gives the current required to confine a plasma with line density \\(N\\) (particles per meter) at temperature \\(T\\):

$$I^2 = \\frac{8\\pi}{\\mu_0} N k_B T$$

where \\(\\mu_0 = 4\\pi \\times 10^{-7}\\) H/m. This shows that higher current can confine denser or hotter plasmas.

## Theta-Pinch: Pressure Balance

In a **theta-pinch** (current flows azimuthally), the axial magnetic field provides confinement. Outside the plasma, the magnetic pressure must balance the plasma kinetic pressure:

$$\\frac{B^2}{2\\mu_0} = n k_B T$$

Solving for the required field: \\(B = \\sqrt{2\\mu_0 n k_B T}\\).

## Tokamak Safety Factor

In a **tokamak**, field lines wind helically around the torus. The **safety factor** \\(q\\) measures how many toroidal turns a field line makes per poloidal turn:

$$q = \\frac{r B_T}{R B_P}$$

where \\(r\\) is the minor radius, \\(R\\) is the major radius, \\(B_T\\) is the toroidal field, and \\(B_P\\) is the poloidal field. Stability requires \\(q > 1\\) (Kruskal-Shafranov condition).
`,
  starterCode: `import math

def bennett_current_A(N_per_m, T_K):
    # Current for z-pinch confinement via Bennett relation
    pass

def theta_pinch_field_T(n_m3, T_K):
    # Magnetic field for theta-pinch pressure balance
    pass

def safety_factor(r_m, R_m, B_T_T, B_P_T):
    # Tokamak safety factor q
    pass`,
  solution: `import math

def bennett_current_A(N_per_m, T_K):
    mu0 = 4 * math.pi * 1e-7
    k_B = 1.381e-23
    return math.sqrt(8 * math.pi / mu0 * N_per_m * k_B * T_K)

def theta_pinch_field_T(n_m3, T_K):
    mu0 = 4 * math.pi * 1e-7
    k_B = 1.381e-23
    return math.sqrt(2 * mu0 * n_m3 * k_B * T_K)

def safety_factor(r_m, R_m, B_T_T, B_P_T):
    return r_m * B_T_T / (R_m * B_P_T)`,
  tests: [
    {
      name: "Bennett current for N=1e20 /m, T=1e7 K",
      expected: "5.2555e+05\n",
      code: `{{FUNC}}
print(f"{bennett_current_A(1e20, 1e7):.4e}")`,
    },
    {
      name: "Bennett current for N=1e19 /m, T=1e6 K",
      expected: "5.2555e+04\n",
      code: `{{FUNC}}
print(f"{bennett_current_A(1e19, 1e6):.4e}")`,
    },
    {
      name: "Theta-pinch field at n=1e20 m^-3, T=1e7 K",
      expected: "1.8630e-01\n",
      code: `{{FUNC}}
print(f"{theta_pinch_field_T(1e20, 1e7):.4e}")`,
    },
    {
      name: "Safety factor for typical tokamak parameters",
      expected: "1.0000\n",
      code: `{{FUNC}}
print(f"{safety_factor(0.3, 3.0, 5.0, 0.5):.4f}")`,
    },
  ],
};
