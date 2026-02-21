import type { Lesson } from "../../types";

export const dispersion: Lesson = {
  id: "dispersion",
  title: "Dispersion and the Cauchy Equation",
  chapterId: "modern",
  content: `# Dispersion and the Cauchy Equation

**Dispersion** is the variation of a material's refractive index with wavelength. It is the reason a prism separates white light into a spectrum, and the reason rainbows exist.

## Why Dispersion Occurs

In a dielectric material, the refractive index arises from the interaction of light with bound electrons. The electrons have natural resonance frequencies; light near these frequencies is affected more strongly. In the visible range (away from resonances), shorter wavelengths (blue/violet) experience a higher refractive index than longer wavelengths (red) — this is called **normal dispersion**.

## The Cauchy Equation

An empirical formula that fits the refractive index of many transparent materials across the visible spectrum:

$$n(\\lambda) = A + \\frac{B}{\\lambda^2}$$

Where $\\lambda$ is the wavelength in nm and $A$, $B$ are material constants:

| Material | $A$ | $B$ (nm²) |
|----------|-----|----------|
| Crown glass | 1.5220 | 4590 |
| Flint glass | 1.6038 | 10700 |
| Fused silica | 1.4580 | 3540 |

The Cauchy equation shows that $n$ increases as $\\lambda$ decreases — blue light bends more than red light, explaining prismatic dispersion.

## Dispersion: Rate of Change of n

The dispersion $dn/d\\lambda$ tells us how rapidly the refractive index changes with wavelength:

$$\\frac{dn}{d\\lambda} = -\\frac{2B}{\\lambda^3}$$

The negative sign confirms that $n$ decreases as $\\lambda$ increases (normal dispersion). A larger magnitude of $dn/d\\lambda$ means stronger dispersion — colors separate more.

**Example:** Crown glass at $\\lambda = 550\\,\\text{nm}$:

$$n(550) = 1.5 + \\frac{5000}{550^2} \\approx 1.5165$$

$$\\frac{dn}{d\\lambda}\\bigg|_{550} = -\\frac{2 \\times 5000}{550^3} \\approx -6.01 \\times 10^{-5}\\,\\text{nm}^{-1}$$

## Your Task

Implement the Cauchy equation and its derivative. Wavelengths are in nm; $B$ has units of nm².
`,
  starterCode: `def cauchy_n(A, B, lam_nm):
    # Return the refractive index at wavelength lam_nm (nm) using the Cauchy equation
    # A: dimensionless offset, B: Cauchy coefficient in nm^2, lam_nm: wavelength in nm
    pass

def dispersion_at(A, B, lam_nm):
    # Return dn/d(lambda) at wavelength lam_nm, in units of nm^-1
    pass
`,
  solution: `def cauchy_n(A, B, lam_nm):
    return A + B / lam_nm ** 2

def dispersion_at(A, B, lam_nm):
    return -2 * B / lam_nm ** 3
`,
  tests: [
    {
      name: "cauchy_n(1.5, 5000, 550) = 1.5165",
      code: `{{FUNC}}
print(round(cauchy_n(1.5, 5000, 550), 4))`,
      expected: "1.5165\n",
    },
    {
      name: "cauchy_n(1.45, 3500, 700) = 1.4571",
      code: `{{FUNC}}
print(round(cauchy_n(1.45, 3500, 700), 4))`,
      expected: "1.4571\n",
    },
    {
      name: "dispersion_at(1.5, 5000, 550) ≈ -6.011e-05 nm⁻¹",
      code: `{{FUNC}}
print(round(dispersion_at(1.5, 5000, 550), 8))`,
      expected: "-6.011e-05\n",
    },
    {
      name: "dispersion_at(1.5, 5000, 400) = -0.00015625 nm⁻¹",
      code: `{{FUNC}}
print(round(dispersion_at(1.5, 5000, 400), 8))`,
      expected: "-0.00015625\n",
    },
  ],
};
