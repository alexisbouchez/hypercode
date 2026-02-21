import type { Lesson } from "../../types";

export const thermalVelocity: Lesson = {
  id: "thermal-velocity",
  title: "Thermal Velocity",
  chapterId: "plasma-fundamentals",
  content: `# Thermal Velocity

Particles in a plasma follow a **Maxwell-Boltzmann distribution** of speeds. Several characteristic speeds describe this distribution.

## Thermal Velocity

The thermal velocity (root-mean-square of one velocity component):

$$v_{th} = \\sqrt{\\frac{k_B T}{m}}$$

## Most Probable Speed

The speed at the peak of the Maxwell-Boltzmann distribution:

$$v_p = \\sqrt{\\frac{2 k_B T}{m}}$$

## Mean Speed

The average speed of particles:

$$\\bar{v} = \\sqrt{\\frac{8 k_B T}{\\pi m}}$$

Where $k_B = 1.381 \\times 10^{-23}$ J/K, and the masses are:
- Electron: $m_e = 9.109 \\times 10^{-31}$ kg
- Proton: $m_p = 1.673 \\times 10^{-27}$ kg

## Comparing Electrons and Ions

Since $v_{th} \\propto 1/\\sqrt{m}$, electrons are much faster than ions at the same temperature:

$$\\frac{v_{th,e}}{v_{th,i}} = \\sqrt{\\frac{m_i}{m_e}} \\approx 43 \\text{ (for hydrogen)}$$

## Physical Significance

- Thermal velocities determine transport properties (diffusion, conductivity)
- Particles with $v > v_{th}$ can escape confinement (loss cone in mirrors)
- Comparing $v_{th}$ with wave phase velocities determines wave-particle interactions (Landau damping)

Implement the three thermal speed functions — all constants must live inside the function bodies.`,
  starterCode: `import math

def thermal_velocity_m_s(T_K, m_kg):
    # v_th = sqrt(k_B * T / m)
    # k_B = 1.381e-23
    pass

def mean_speed_m_s(T_K, m_kg):
    # v_mean = sqrt(8 * k_B * T / (pi * m))
    pass

def most_probable_speed_m_s(T_K, m_kg):
    # v_p = sqrt(2 * k_B * T / m)
    pass`,
  solution: `import math

def thermal_velocity_m_s(T_K, m_kg):
    k_B = 1.381e-23
    return math.sqrt(k_B * T_K / m_kg)

def mean_speed_m_s(T_K, m_kg):
    k_B = 1.381e-23
    return math.sqrt(8 * k_B * T_K / (math.pi * m_kg))

def most_probable_speed_m_s(T_K, m_kg):
    k_B = 1.381e-23
    return math.sqrt(2 * k_B * T_K / m_kg)`,
  tests: [
    {
      name: "Electron thermal velocity at T=10000K",
      expected: "3.8937e+05\n",
      code: `{{FUNC}}
m_e = 9.109e-31
print(f"{thermal_velocity_m_s(10000, m_e):.4e}")`,
    },
    {
      name: "Proton thermal velocity at T=10000K",
      expected: "9.0855e+03\n",
      code: `{{FUNC}}
m_p = 1.673e-27
print(f"{thermal_velocity_m_s(10000, m_p):.4e}")`,
    },
    {
      name: "Electron mean speed at T=10000K",
      expected: "6.2134e+05\n",
      code: `{{FUNC}}
m_e = 9.109e-31
print(f"{mean_speed_m_s(10000, m_e):.4e}")`,
    },
    {
      name: "Ratio of electron to proton thermal velocity (should be ~sqrt(m_p/m_e))",
      expected: "42.8561\n",
      code: `{{FUNC}}
m_e = 9.109e-31
m_p = 1.673e-27
print(f"{thermal_velocity_m_s(10000, m_e) / thermal_velocity_m_s(10000, m_p):.4f}")`,
    },
  ],
};
