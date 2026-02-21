import type { Lesson } from "../../types";

export const debyeLength: Lesson = {
  id: "debye-length",
  title: "Debye Length",
  chapterId: "plasma-fundamentals",
  content: `# Debye Length

One of the most fundamental concepts in plasma physics is **Debye shielding**. A plasma is an ionized gas, and unlike a neutral gas, it can rearrange its charges to screen out electric fields.

When you place a test charge inside a plasma, the surrounding electrons (and ions) redistribute themselves to partially cancel the electric field of the charge. The characteristic distance over which this shielding occurs is called the **Debye length**:

$$\\lambda_D = \\sqrt{\\frac{\\varepsilon_0 k_B T}{n e^2}}$$

Where:
- $\\varepsilon_0 = 8.854 \\times 10^{-12}$ F/m (permittivity of free space)
- $k_B = 1.381 \\times 10^{-23}$ J/K (Boltzmann constant)
- $T$ = electron temperature in Kelvin
- $n$ = number density in m⁻³
- $e = 1.602 \\times 10^{-19}$ C (elementary charge)

## Debye Sphere

The **Debye sphere** is the sphere of radius $\\lambda_D$ around a test charge. For a plasma to behave collectively (rather than as individual particles), there must be many particles inside the Debye sphere:

$$N_D = n \\cdot \\frac{4}{3} \\pi \\lambda_D^3 \\gg 1$$

This is the **plasma parameter** condition. When $N_D \\gg 1$, collective behavior dominates over binary collisions.

## Physical Intuition

- **Higher temperature** → larger Debye length (faster electrons can overcome the shielding)
- **Higher density** → smaller Debye length (more charges available to screen the field)
- Solar wind plasma: $\\lambda_D \\sim$ micrometers
- Fusion plasma: $\\lambda_D \\sim$ micrometers to millimeters
- Interstellar plasma: $\\lambda_D \\sim$ meters to kilometers

Implement \`debye_length_m(n_m3, T_K)\` using the formula above, and \`number_of_particles_in_debye_sphere(n_m3, lambda_D_m)\` to compute the plasma parameter.`,
  starterCode: `import math

def debye_length_m(n_m3, T_K):
    # eps0 = 8.854e-12, k_B = 1.381e-23, e = 1.602e-19
    # lambda_D = sqrt(eps0 * k_B * T / (n * e^2))
    pass

def number_of_particles_in_debye_sphere(n_m3, lambda_D_m):
    # N_D = n * (4/3) * pi * lambda_D^3
    pass`,
  solution: `import math

def debye_length_m(n_m3, T_K):
    eps0 = 8.854e-12
    k_B = 1.381e-23
    e = 1.602e-19
    return math.sqrt(eps0 * k_B * T_K / (n_m3 * e**2))

def number_of_particles_in_debye_sphere(n_m3, lambda_D_m):
    return n_m3 * (4/3) * math.pi * lambda_D_m**3`,
  tests: [
    {
      name: "Debye length in solar wind conditions (n=1e19, T=10000K)",
      expected: "2.1828e-06\n",
      code: `{{FUNC}}
print(f"{debye_length_m(1e19, 10000):.4e}")`,
    },
    {
      name: "Debye length in fusion plasma (n=1e20, T=1e6 K)",
      expected: "6.9025e-06\n",
      code: `{{FUNC}}
print(f"{debye_length_m(1e20, 1e6):.4e}")`,
    },
    {
      name: "Number of particles in Debye sphere (n=1e19, T=10000K)",
      expected: "4.3561e+02\n",
      code: `{{FUNC}}
print(f"{number_of_particles_in_debye_sphere(1e19, debye_length_m(1e19, 10000)):.4e}")`,
    },
    {
      name: "Debye length at low density (n=1e6, T=1e4 K)",
      expected: "6.9025e+00\n",
      code: `{{FUNC}}
print(f"{debye_length_m(1e6, 1e4):.4e}")`,
    },
  ],
};
