import type { Lesson } from "../../types";

export const membraneElasticity: Lesson = {
  id: "membrane-elasticity",
  title: "Membrane Elasticity",
  chapterId: "molecular",
  content: `# Membrane Elasticity

## Lipid Bilayer Mechanics

Biological membranes are fluid lipid bilayers roughly 4–5 nm thick. Despite being only two molecules thick, they resist bending and area changes, controlling cell shape and vesicle formation.

## Helfrich Bending Energy

The bending energy per unit area of a membrane is described by the **Helfrich Hamiltonian**:

$$\\frac{E_{\\text{bend}}}{A} = \\frac{\\kappa}{2}(c_1 + c_2 - c_0)^2$$

where **c₁** and **c₂** are the two principal curvatures, **c₀** is the spontaneous curvature (zero for symmetric bilayers), and **κ** is the **bending modulus**.

For a **sphere of radius R** with c₁ = c₂ = 1/R and c₀ = 0, integrating over the surface area A = 4πR²:

$$E_{\\text{sphere}} = \\frac{\\kappa}{2}\\left(\\frac{2}{R} - c_0\\right)^2 \\cdot 4\\pi R^2$$

Remarkably, for c₀ = 0 this simplifies to **E = 8πκ**, independent of vesicle size!

Typical values: **κ ≈ 10–25 k_BT** (≈ 4–10 × 10⁻²⁰ J at 37°C).

## Area Stretching

The bilayer strongly resists changes in area. The **area stretch energy** is:

$$E_{\\text{stretch}} = \\frac{K_A}{2} \\left(\\frac{\\Delta A}{A_0}\\right)^2 A_0$$

where **K_A ≈ 0.24 N/m** is the area stretch modulus. The membrane tension is:

$$\\sigma = K_A \\cdot \\frac{\\Delta A}{A_0}$$

Membranes lyse (rupture) when stretched by only ~3–5% (σ_lytic ≈ 5–10 mN/m).

## Thermal Fluctuations

Thermal energy k_BT sets the scale for bending fluctuations. The dimensionless bending modulus:

$$\\tilde{\\kappa} = \\frac{\\kappa}{k_B T}$$

quantifies how "stiff" a membrane is relative to thermal energy.

## Your Task

Implement four functions:

1. **\`bending_energy_sphere_J(kappa_J, R_m, c0_m=0)\`** — Total bending energy of a spherical vesicle in Joules
2. **\`area_stretch_energy_J(K_A_N_m, A0_m2, dA_m2)\`** — Area stretching energy in Joules
3. **\`lytic_tension_N_m(K_A_N_m=0.24, strain=0.05)\`** — Membrane tension at 5% strain in N/m
4. **\`thermal_bending_modulus_kBT(kappa_J, T_K=310)\`** — Bending modulus in units of k_BT
`,
  starterCode: `import math

def bending_energy_sphere_J(kappa_J, R_m, c0_m=0):
    # E = (kappa/2) * (2/R - c0)^2 * 4*pi*R^2
    pass

def area_stretch_energy_J(K_A_N_m, A0_m2, dA_m2):
    # E = (K_A / 2) * (dA / A0)^2 * A0
    pass

def lytic_tension_N_m(K_A_N_m=0.24, strain=0.05):
    # sigma = K_A * strain
    pass

def thermal_bending_modulus_kBT(kappa_J, T_K=310):
    # kappa / (k_B * T), k_B = 1.381e-23 J/K
    pass
`,
  solution: `import math

def bending_energy_sphere_J(kappa_J, R_m, c0_m=0):
    return (kappa_J / 2) * (2 / R_m - c0_m)**2 * 4 * math.pi * R_m**2

def area_stretch_energy_J(K_A_N_m, A0_m2, dA_m2):
    return (K_A_N_m / 2) * (dA_m2 / A0_m2)**2 * A0_m2

def lytic_tension_N_m(K_A_N_m=0.24, strain=0.05):
    return K_A_N_m * strain

def thermal_bending_modulus_kBT(kappa_J, T_K=310):
    k_B = 1.381e-23
    return kappa_J / (k_B * T_K)
`,
  tests: [
    {
      name: "Bending energy of a 50 nm vesicle (kappa=8.3e-20 J ≈ 20 k_BT)",
      expected: "2.0860e-18\n",
      code: `{{FUNC}}
print(f"{bending_energy_sphere_J(8.3e-20, 50e-9):.4e}")`,
    },
    {
      name: "Bending energy of a 5 μm vesicle (same kappa — topologically invariant)",
      expected: "2.0860e-18\n",
      code: `{{FUNC}}
print(f"{bending_energy_sphere_J(8.3e-20, 5e-6):.4e}")`,
    },
    {
      name: "Lytic tension at 5% strain (K_A=0.24 N/m)",
      expected: "1.2000e-02\n",
      code: `{{FUNC}}
print(f"{lytic_tension_N_m():.4e}")`,
    },
    {
      name: "Bending modulus in k_BT units at 310 K",
      expected: "19.3875\n",
      code: `{{FUNC}}
print(f"{thermal_bending_modulus_kBT(8.3e-20):.4f}")`,
    },
  ],
};
