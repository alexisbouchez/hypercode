import type { Lesson } from "../../types";

export const phononDispersion: Lesson = {
  id: "phonon-dispersion",
  title: "Phonon Dispersion",
  chapterId: "phonons",
  content: `# Phonon Dispersion

Phonons are quantized lattice vibrations — the collective oscillations of atoms in a crystal. In a **1D monatomic chain** with atomic mass M, spring constant K, and lattice constant a, the dispersion relation is:

$$\\omega(k) = 2\\sqrt{\\frac{K}{M}} \\left|\\sin\\left(\\frac{ka}{2}\\right)\\right|$$

This is an **acoustic branch**: ω → 0 as k → 0 (long-wavelength sound waves).

## Group and Phase Velocity

The **group velocity** (energy transport speed) is:

$$v_g = \\frac{d\\omega}{dk} = a\\sqrt{\\frac{K}{M}}\\cos\\left(\\frac{ka}{2}\\right)$$

The **phase velocity** is:

$$v_{ph} = \\frac{\\omega}{k}$$

## Brillouin Zone Boundary

At the zone boundary k = π/a, the sine equals 1, giving the maximum frequency:

$$\\omega_{max} = 2\\sqrt{\\frac{K}{M}}$$

At this point, cos(π/2) = 0, so **v_g = 0** — the wave becomes a standing wave with no net energy transport.

## Key Insight

Near k = 0, the dispersion is linear: ω ≈ a√(K/M) · k, so the group velocity equals the speed of sound. As k increases toward the zone boundary, the dispersion flattens and the group velocity decreases to zero.

## Your Task

Implement three functions:

\`\`\`python
import math

def phonon_frequency_rad_s(k_m, K_N_m, M_kg, a_m):
    # omega = 2*sqrt(K/M) * |sin(k*a/2)|
    pass

def group_velocity_m_s(k_m, K_N_m, M_kg, a_m):
    # v_g = a * sqrt(K/M) * cos(k*a/2)
    pass

def zone_boundary_frequency_rad_s(K_N_m, M_kg):
    # omega_max = 2 * sqrt(K/M)
    pass
\`\`\`
`,
  starterCode: `import math

def phonon_frequency_rad_s(k_m, K_N_m, M_kg, a_m):
    # Return omega = 2*sqrt(K/M) * |sin(k*a/2)|
    pass

def group_velocity_m_s(k_m, K_N_m, M_kg, a_m):
    # Return v_g = a * sqrt(K/M) * cos(k*a/2)
    pass

def zone_boundary_frequency_rad_s(K_N_m, M_kg):
    # Return omega_max = 2 * sqrt(K/M)
    pass
`,
  solution: `import math

def phonon_frequency_rad_s(k_m, K_N_m, M_kg, a_m):
    return 2 * math.sqrt(K_N_m / M_kg) * abs(math.sin(k_m * a_m / 2))

def group_velocity_m_s(k_m, K_N_m, M_kg, a_m):
    return a_m * math.sqrt(K_N_m / M_kg) * math.cos(k_m * a_m / 2)

def zone_boundary_frequency_rad_s(K_N_m, M_kg):
    return 2 * math.sqrt(K_N_m / M_kg)
`,
  tests: [
    {
      name: "Acoustic mode at k=0 has zero frequency",
      expected: "0.0000e+00\n",
      code: `{{FUNC}}
print(f"{phonon_frequency_rad_s(0, 10.0, 1.673e-26, 2.5e-10):.4e}")`,
    },
    {
      name: "Zone boundary maximum frequency for K=10 N/m, M=1.673e-26 kg",
      expected: "4.8897e+13\n",
      code: `{{FUNC}}
print(f"{zone_boundary_frequency_rad_s(10.0, 1.673e-26):.4e}")`,
    },
    {
      name: "Group velocity at k=0 equals a*sqrt(K/M)",
      expected: "6.1121e+03\n",
      code: `{{FUNC}}
print(f"{group_velocity_m_s(0, 10.0, 1.673e-26, 2.5e-10):.4e}")`,
    },
    {
      name: "Group velocity vanishes at zone boundary (standing wave)",
      expected: "3.7426e-13\n",
      code: `{{FUNC}}
import math
print(f"{group_velocity_m_s(math.pi / 2.5e-10, 10.0, 1.673e-26, 2.5e-10):.4e}")`,
    },
  ],
};
