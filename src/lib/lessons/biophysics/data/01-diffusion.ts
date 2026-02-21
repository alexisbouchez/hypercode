import type { Lesson } from "../../types";

export const diffusion: Lesson = {
  id: "diffusion",
  title: "Diffusion & Brownian Motion",
  chapterId: "molecular",
  content: `# Diffusion & Brownian Motion

## Brownian Motion

Brownian motion describes the random thermal motion of particles suspended in a fluid. In 1905, Einstein derived the relationship between this random motion and the diffusion coefficient.

## Stokes-Einstein Relation

For a spherical particle of radius **r** in a fluid of viscosity **η** at temperature **T**:

$$D = \\frac{k_B T}{6\\pi\\eta r}$$

- **k_B** = 1.381 × 10⁻²³ J/K (Boltzmann constant)
- **T** = temperature in Kelvin
- **η** = dynamic viscosity in Pa·s (water ≈ 1 × 10⁻³ Pa·s at 20°C)
- **r** = hydrodynamic radius in meters

## Mean Squared Displacement (MSD)

The MSD grows linearly with time, a hallmark of diffusion:

$$\\text{MSD} = 2d \\cdot D \\cdot t$$

where **d** is the number of spatial dimensions (d=1 for 1D, d=3 for 3D). The root mean square displacement is:

$$r_{\\text{rms}} = \\sqrt{\\text{MSD}}$$

## Time to Diffuse a Distance

Rearranging MSD, the characteristic time to diffuse a distance **L** is:

$$t = \\frac{L^2}{2d \\cdot D}$$

This scales as L², meaning diffusion becomes extremely slow over long distances — a 10× larger cell takes 100× longer to transport molecules by diffusion alone.

## Biological Examples

| Molecule | Radius | D in water (25°C) |
|----------|--------|-------------------|
| Small protein (~1 nm) | 1 nm | ~2.2 × 10⁻¹⁰ m²/s |
| Lipid vesicle (50 nm) | 50 nm | ~4.4 × 10⁻¹² m²/s |
| Bacterium (~1 μm) | 500 nm | ~4.4 × 10⁻¹³ m²/s |

## Your Task

Implement three functions:

1. **\`diffusion_coefficient_m2_s(T_K, eta_Pa_s, r_m)\`** — Stokes-Einstein diffusion coefficient in m²/s
2. **\`msd_m2(D_m2_s, t_s, d=3)\`** — Mean squared displacement in m² (default 3D)
3. **\`diffusion_time_s(L_m, D_m2_s, d=3)\`** — Time to diffuse distance L in seconds (default 3D)
`,
  starterCode: `import math

def diffusion_coefficient_m2_s(T_K, eta_Pa_s, r_m):
    # Stokes-Einstein: D = k_B * T / (6 * pi * eta * r)
    # k_B = 1.381e-23 J/K
    pass

def msd_m2(D_m2_s, t_s, d=3):
    # MSD = 2 * d * D * t
    pass

def diffusion_time_s(L_m, D_m2_s, d=3):
    # t = L^2 / (2 * d * D)
    pass
`,
  solution: `import math

def diffusion_coefficient_m2_s(T_K, eta_Pa_s, r_m):
    k_B = 1.381e-23
    return k_B * T_K / (6 * math.pi * eta_Pa_s * r_m)

def msd_m2(D_m2_s, t_s, d=3):
    return 2 * d * D_m2_s * t_s

def diffusion_time_s(L_m, D_m2_s, d=3):
    return L_m**2 / (2 * d * D_m2_s)
`,
  tests: [
    {
      name: "Diffusion coefficient of a 1 nm protein in water at 25°C",
      expected: "2.1833e-10\n",
      code: `{{FUNC}}
print(f"{diffusion_coefficient_m2_s(298, 1e-3, 1e-9):.4e}")`,
    },
    {
      name: "Diffusion coefficient of a 50 nm vesicle in water at 25°C",
      expected: "4.3666e-12\n",
      code: `{{FUNC}}
print(f"{diffusion_coefficient_m2_s(298, 1e-3, 5e-8):.4e}")`,
    },
    {
      name: "MSD for 1 nm protein (D=2.19e-10) after 1 ms in 3D",
      expected: "1.3140e-12\n",
      code: `{{FUNC}}
print(f"{msd_m2(2.19e-10, 1e-3):.4e}")`,
    },
    {
      name: "Time to diffuse 1 μm with D=2.19e-10 in 3D",
      expected: "7.6104e-04\n",
      code: `{{FUNC}}
print(f"{diffusion_time_s(1e-6, 2.19e-10):.4e}")`,
    },
  ],
};
