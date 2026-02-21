import type { Lesson } from "../../types";

export const tightBinding: Lesson = {
  id: "tight-binding",
  title: "Tight-Binding Model",
  chapterId: "crystal-structure",
  content: `# Tight-Binding Model

The **tight-binding model** describes the electronic band structure by starting from atomic orbitals and allowing them to overlap weakly between neighbouring sites.

## 1D Chain Dispersion

For a one-dimensional chain with lattice constant a and nearest-neighbour hopping integral t, the band energy is:

$$E(k) = E_0 - 2t \\cos(ka)$$

- At k = 0 (band bottom): E = E₀ − 2t (minimum energy)
- At k = π/a (zone boundary): E = E₀ + 2t (maximum energy)
- **Bandwidth**: W = 4t

The hopping integral t > 0 represents electron delocalisation — larger t means wider bands and more metallic behaviour.

## Effective Mass

Near the band bottom (k ≈ 0), the dispersion is parabolic and defines an effective mass:

$$m^* = \\frac{\\hbar^2}{2 t a^2}$$

As a ratio to the free-electron mass:

$$\\frac{m^*}{m_e} = \\frac{\\hbar^2}{2 t a^2 m_e}$$

## Group Velocity

The electron group velocity is:

$$v_g = \\frac{1}{\\hbar}\\frac{dE}{dk} = \\frac{2ta}{\\hbar}\\sin(ka)$$

Maximum velocity occurs at k = π/(2a): v_max = 2ta/ħ.

## Implement

\`\`\`python
def tight_binding_energy_eV(k_m, t_eV, a_m, E0_eV=0):
    # Returns band energy in eV
    ...

def effective_mass_ratio(t_eV, a_m):
    # Returns m*/m_e (dimensionless)
    ...

def group_velocity_m_s(k_m, t_eV, a_m):
    # Returns group velocity in m/s
    ...
\`\`\`
`,
  starterCode: `import math

def tight_binding_energy_eV(k_m, t_eV, a_m, E0_eV=0):
    pass

def effective_mass_ratio(t_eV, a_m):
    pass

def group_velocity_m_s(k_m, t_eV, a_m):
    pass
`,
  solution: `import math

def tight_binding_energy_eV(k_m, t_eV, a_m, E0_eV=0):
    return E0_eV - 2 * t_eV * math.cos(k_m * a_m)

def effective_mass_ratio(t_eV, a_m):
    e = 1.602e-19
    hbar = 1.055e-34
    m_e = 9.109e-31
    t_J = t_eV * e
    return hbar**2 / (2 * t_J * a_m**2 * m_e)

def group_velocity_m_s(k_m, t_eV, a_m):
    e = 1.602e-19
    hbar = 1.055e-34
    t_J = t_eV * e
    return (2 * t_J * a_m / hbar) * math.sin(k_m * a_m)
`,
  tests: [
    {
      name: "Band energy at k=0 (band bottom), t=1 eV, a=2.5 Å",
      expected: "-2.0000\n",
      code: `{{FUNC}}\nprint(f"{tight_binding_energy_eV(0, 1.0, 2.5e-10):.4f}")`,
    },
    {
      name: "Band energy at zone boundary k=π/a, t=1 eV, a=2.5 Å",
      expected: "2.0000\n",
      code: `{{FUNC}}\nprint(f"{tight_binding_energy_eV(3.14159/(2.5e-10), 1.0, 2.5e-10):.4f}")`,
    },
    {
      name: "Effective mass ratio m*/m_e for t=1 eV, a=2.5 Å",
      expected: "0.6102\n",
      code: `{{FUNC}}\nprint(f"{effective_mass_ratio(1.0, 2.5e-10):.4f}")`,
    },
    {
      name: "Group velocity at k=π/2a (maximum), t=1 eV, a=2.5 Å",
      expected: "7.5924e+05\n",
      code: `{{FUNC}}\nprint(f"{group_velocity_m_s(3.14159/(2*2.5e-10), 1.0, 2.5e-10):.4e}")`,
    },
  ],
};
