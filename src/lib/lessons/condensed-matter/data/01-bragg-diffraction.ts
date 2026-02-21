import type { Lesson } from "../../types";

export const braggDiffraction: Lesson = {
  id: "bragg-diffraction",
  title: "Bragg Diffraction",
  chapterId: "crystal-structure",
  content: `## Bragg Diffraction

X-ray diffraction is the primary tool for determining crystal structures. When X-rays scatter off parallel planes of atoms in a crystal, constructive interference occurs only at specific angles.

**Bragg's Law:**

$$2d \\sin(\\theta) = n\\lambda$$

Where:
- **d** = interplanar spacing (distance between parallel crystal planes)
- **θ** = glancing angle (angle between the incident beam and the crystal plane)
- **n** = diffraction order (positive integer, usually 1)
- **λ** = X-ray wavelength

### Interplanar Spacing for Cubic Crystals

For a cubic crystal with lattice constant **a** and Miller indices **(h, k, l)**:

$$d_{hkl} = \\frac{a}{\\sqrt{h^2 + k^2 + l^2}}$$

Common crystal planes:
- **(100)**: d = a
- **(110)**: d = a/√2
- **(111)**: d = a/√3

### Finding the Bragg Angle

Rearranging Bragg's law:

$$\\theta = \\arcsin\\left(\\frac{n\\lambda}{2d}\\right)$$

This is the glancing angle at which a diffraction peak (reflection) will be observed.

### Example: NaCl with Cu Kα X-rays

Copper Kα X-rays have λ ≈ 1.54 Å. NaCl has a rock-salt structure with lattice constant a = 5.64 Å, giving d(100) = 2.82 Å. The first-order Bragg angle is about 15.8°.
`,
  starterCode: `import math

def bragg_wavelength_m(d_m, theta_deg, n=1):
    pass

def d_spacing_m(a_m, h, k, l):
    pass

def bragg_angle_deg(d_m, lam_m, n=1):
    pass
`,
  solution: `import math

def bragg_wavelength_m(d_m, theta_deg, n=1):
    return 2 * d_m * math.sin(math.radians(theta_deg)) / n

def d_spacing_m(a_m, h, k, l):
    return a_m / math.sqrt(h**2 + k**2 + l**2)

def bragg_angle_deg(d_m, lam_m, n=1):
    return math.degrees(math.asin(n * lam_m / (2 * d_m)))
`,
  tests: [
    {
      name: "Bragg wavelength for NaCl (100) planes at 15.8°",
      expected: "1.5357e-10\n",
      code: `{{FUNC}}
print(f"{bragg_wavelength_m(2.82e-10, 15.8):.4e}")`,
    },
    {
      name: "d-spacing for FCC Cu (111) planes",
      expected: "2.0346e-10\n",
      code: `{{FUNC}}
print(f"{d_spacing_m(3.524e-10, 1, 1, 1):.4e}")`,
    },
    {
      name: "d-spacing for BCC Fe (110) planes",
      expected: "2.0266e-10\n",
      code: `{{FUNC}}
print(f"{d_spacing_m(2.866e-10, 1, 1, 0):.4e}")`,
    },
    {
      name: "Bragg angle for NaCl with Cu Kα X-rays",
      expected: "15.8458\n",
      code: `{{FUNC}}
print(f"{bragg_angle_deg(2.82e-10, 1.54e-10):.4f}")`,
    },
  ],
};
