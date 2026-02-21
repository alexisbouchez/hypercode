import type { Lesson } from "../../types";

export const plasmaBeta: Lesson = {
  id: "plasma-beta",
  title: "Plasma Beta",
  chapterId: "plasma-fundamentals",
  content: `# Plasma Beta

**Plasma beta** ($\\beta$) is one of the most important dimensionless parameters in plasma physics. It is the ratio of the thermal pressure to the magnetic pressure:

$$\\beta = \\frac{p_{thermal}}{p_{magnetic}} = \\frac{n k_B T}{B^2 / (2\\mu_0)}$$

Where:
- $n$ = number density (m⁻³)
- $k_B = 1.381 \\times 10^{-23}$ J/K
- $T$ = temperature (K)
- $B$ = magnetic field strength (T)
- $\\mu_0 = 4\\pi \\times 10^{-7}$ H/m (permeability of free space)

## Pressure Components

**Thermal pressure** (kinetic pressure of the plasma):
$$p_{thermal} = n k_B T$$

**Magnetic pressure** (energy density of the magnetic field):
$$p_{magnetic} = \\frac{B^2}{2\\mu_0}$$

## Physical Significance

$\\beta$ tells us which pressure dominates the plasma dynamics:

| $\\beta$ value | Regime | Example |
|---|---|---|
| $\\beta \\ll 1$ | Magnetically dominated | Fusion tokamaks ($\\beta \\sim 0.1$) |
| $\\beta \\sim 1$ | Comparable pressures | Solar corona |
| $\\beta \\gg 1$ | Thermally dominated | Accretion disks |

- **Low-$\\beta$ plasmas**: magnetic field controls particle motion; good confinement
- **High-$\\beta$ plasmas**: particles can distort or escape the field

In tokamak fusion reactors, achieving higher $\\beta$ means more plasma pressure (and thus more fusion power) for a given magnetic field — a key engineering goal.

Implement the three pressure functions below. Keep all constants ($k_B$, $\\mu_0$) inside the function bodies.`,
  starterCode: `import math

def plasma_beta(n_m3, T_K, B_T):
    # beta = thermal_pressure / magnetic_pressure
    # k_B = 1.381e-23, mu0 = 4 * pi * 1e-7
    pass

def magnetic_pressure_Pa(B_T):
    # p_mag = B^2 / (2 * mu0)
    pass

def thermal_pressure_Pa(n_m3, T_K):
    # p_th = n * k_B * T
    pass`,
  solution: `import math

def thermal_pressure_Pa(n_m3, T_K):
    k_B = 1.381e-23
    return n_m3 * k_B * T_K

def magnetic_pressure_Pa(B_T):
    mu0 = 4 * math.pi * 1e-7
    return B_T**2 / (2 * mu0)

def plasma_beta(n_m3, T_K, B_T):
    return thermal_pressure_Pa(n_m3, T_K) / magnetic_pressure_Pa(B_T)`,
  tests: [
    {
      name: "Plasma beta in fusion conditions (n=1e20, T=1e7K, B=5T)",
      expected: "1.3883e-03\n",
      code: `{{FUNC}}
print(f"{plasma_beta(1e20, 1e7, 5.0):.4e}")`,
    },
    {
      name: "Magnetic pressure at B=5T",
      expected: "9.9472e+06\n",
      code: `{{FUNC}}
print(f"{magnetic_pressure_Pa(5.0):.4e}")`,
    },
    {
      name: "Thermal pressure at n=1e20, T=1e7K",
      expected: "1.3810e+04\n",
      code: `{{FUNC}}
print(f"{thermal_pressure_Pa(1e20, 1e7):.4e}")`,
    },
    {
      name: "Plasma beta at low density and weak field (n=1e6, T=1e4K, B=1e-8T)",
      expected: "3.4708e-03\n",
      code: `{{FUNC}}
print(f"{plasma_beta(1e6, 1e4, 1e-8):.4e}")`,
    },
  ],
};
