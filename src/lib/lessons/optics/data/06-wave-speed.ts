import type { Lesson } from "../../types";

export const waveSpeed: Lesson = {
  id: "wave-speed",
  title: "Speed of Light in Media",
  chapterId: "wave",
  content: `# Speed of Light in Media

Light travels at $c = 3 \\times 10^8\\,\\text{m/s}$ in vacuum. Inside a transparent medium, it slows down. The ratio of the vacuum speed to the medium speed defines the **refractive index**:

$$n = \\frac{c}{v} \\quad \\Longrightarrow \\quad v = \\frac{c}{n}$$

## Speed in Various Media

| Medium | $n$ | Speed (m/s) |
|--------|-----|-------------|
| Vacuum | 1.000 | $3.000 \\times 10^8$ |
| Air | 1.0003 | $2.999 \\times 10^8$ |
| Water | 1.333 | $2.25 \\times 10^8$ |
| Glass | 1.5 | $2.00 \\times 10^8$ |
| Diamond | 2.42 | $1.24 \\times 10^8$ |

## Wavelength Change

Light's frequency $f$ does not change when it enters a medium (it's set by the source). But since $v = f\\lambda$, a lower speed means a shorter wavelength:

$$\\lambda_n = \\frac{\\lambda_0}{n}$$

Where $\\lambda_0$ is the wavelength in vacuum and $\\lambda_n$ is the wavelength in the medium.

**Example:** Yellow sodium light ($\\lambda_0 = 589\\,\\text{nm}$) entering glass ($n = 1.5$):

$$\\lambda_n = \\frac{589}{1.5} \\approx 393\\,\\text{nm}$$

The wavelength shortens, but the color we perceive is determined by frequency — so the light still looks yellow when it exits.

## Finding the Refractive Index

If you measure the speed of light inside a medium:

$$n = \\frac{c}{v}$$

## Your Task

Implement the three functions below. Use $c = 3 \\times 10^8\\,\\text{m/s}$.
`,
  starterCode: `def wave_speed(n):
    # Return the speed of light in m/s in a medium with refractive index n
    pass

def wavelength_in_medium(lambda0_nm, n):
    # Return the wavelength in nm inside the medium
    pass

def refractive_index(v):
    # Return the refractive index given the speed v (m/s) of light in the medium
    pass
`,
  solution: `def wave_speed(n):
    c = 3e8
    return c / n

def wavelength_in_medium(lambda0_nm, n):
    return lambda0_nm / n

def refractive_index(v):
    c = 3e8
    return c / v
`,
  tests: [
    {
      name: "wave_speed(1.5) = 2e8 m/s",
      code: `{{FUNC}}
print(wave_speed(1.5))`,
      expected: "200000000.0\n",
    },
    {
      name: "wave_speed(1.0) = 3e8 m/s",
      code: `{{FUNC}}
print(wave_speed(1.0))`,
      expected: "300000000.0\n",
    },
    {
      name: "wavelength_in_medium(589, 1.5) = 392.6667 nm",
      code: `{{FUNC}}
print(round(wavelength_in_medium(589, 1.5), 4))`,
      expected: "392.6667\n",
    },
    {
      name: "refractive_index(2e8) = 1.5",
      code: `{{FUNC}}
print(round(refractive_index(2e8), 4))`,
      expected: "1.5\n",
    },
  ],
};
