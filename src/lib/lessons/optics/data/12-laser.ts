import type { Lesson } from "../../types";

export const laser: Lesson = {
  id: "laser",
  title: "Lasers and Beam Intensity",
  chapterId: "modern",
  content: `# Lasers and Beam Intensity

**LASER** stands for Light Amplification by Stimulated Emission of Radiation. Unlike ordinary light, laser light is:
- **Monochromatic**: single wavelength
- **Coherent**: all photons in phase
- **Collimated**: very low divergence

## Stimulated Emission

An atom in an excited state can be triggered by an incoming photon to emit an identical photon — same wavelength, direction, and phase. This is **stimulated emission**, the mechanism behind laser amplification.

In a laser:
1. A **gain medium** (gas, crystal, semiconductor) provides atoms to excite.
2. A **pump** (electrical discharge, flash lamp, another laser) creates a **population inversion** — more atoms in the excited state than the ground state.
3. An **optical resonator** (two mirrors) traps the light, building up stimulated emission.
4. One mirror is partially transparent — the **output coupler** — letting the beam exit.

## Laser Intensity

The intensity of a laser beam is the power per unit area:

$$I = \\frac{P}{A}$$

For a circular Gaussian beam with diameter $D$ (radius $r = D/2$):

$$A = \\pi r^2 = \\pi \\left(\\frac{D}{2}\\right)^2$$

$$I = \\frac{P}{\\pi (D/2)^2}$$

**Example:** A 5 mW laser pointer with beam diameter 2 mm:

$$A = \\pi (1 \\times 10^{-3})^2 = \\pi \\times 10^{-6}\\,\\text{m}^2 \\approx 3.14 \\times 10^{-6}\\,\\text{m}^2$$

$$I = \\frac{5 \\times 10^{-3}}{3.14 \\times 10^{-6}} \\approx 1592\\,\\text{W/m}^2$$

Even a low-power laser can reach intensities dangerous to the eye because the beam is so narrow.

## Beam Area

The cross-sectional area of a circular beam of diameter $D$ (in mm) in mm²:

$$A_{\\text{mm}^2} = \\pi \\left(\\frac{D}{2}\\right)^2$$

## Your Task

Implement the intensity and area functions. Use $\\pi$ from the \`math\` module.
`,
  starterCode: `import math

def beam_area(diameter_mm):
    # Return the cross-sectional area of a circular beam in mm^2
    pass

def laser_intensity(power_mW, beam_diameter_mm):
    # Return the laser intensity in W/m^2
    # power_mW: laser power in milliwatts, beam_diameter_mm: beam diameter in mm
    pass
`,
  solution: `import math

def beam_area(diameter_mm):
    return math.pi * (diameter_mm / 2) ** 2

def laser_intensity(power_mW, beam_diameter_mm):
    P = power_mW * 1e-3
    r_m = (beam_diameter_mm / 2) * 1e-3
    A = math.pi * r_m ** 2
    return P / A
`,
  tests: [
    {
      name: "beam_area(2 mm) = 3.1416 mm²",
      code: `{{FUNC}}
print(round(beam_area(2), 4))`,
      expected: "3.1416\n",
    },
    {
      name: "beam_area(1 mm) = 0.7854 mm²",
      code: `{{FUNC}}
print(round(beam_area(1), 4))`,
      expected: "0.7854\n",
    },
    {
      name: "laser_intensity(5 mW, 2 mm) = 1591.55 W/m²",
      code: `{{FUNC}}
print(round(laser_intensity(5, 2), 2))`,
      expected: "1591.55\n",
    },
    {
      name: "laser_intensity(1 mW, 1 mm) = 1273.24 W/m²",
      code: `{{FUNC}}
print(round(laser_intensity(1, 1), 2))`,
      expected: "1273.24\n",
    },
  ],
};
