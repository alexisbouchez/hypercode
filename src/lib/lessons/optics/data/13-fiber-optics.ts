import type { Lesson } from "../../types";

export const fiberOptics: Lesson = {
  id: "fiber-optics",
  title: "Fiber Optics and Numerical Aperture",
  chapterId: "modern",
  content: `# Fiber Optics and Numerical Aperture

Optical fibers transmit light over long distances with minimal loss by exploiting **total internal reflection**. They are the backbone of modern telecommunications, carrying internet traffic across oceans.

## Structure of an Optical Fiber

An optical fiber has two concentric layers:
- **Core**: higher refractive index $n_1$. Light propagates here.
- **Cladding**: lower refractive index $n_2 < n_1$. Surrounds the core.

Because $n_1 > n_2$, light hitting the core-cladding interface beyond the critical angle undergoes total internal reflection and remains confined in the core.

## Numerical Aperture (NA)

The **numerical aperture** characterizes how much light a fiber can accept. It depends on the difference in refractive indices:

$$\\text{NA} = \\sqrt{n_1^2 - n_2^2}$$

A larger NA means the fiber accepts light from a wider cone of angles.

**Example:** Core $n_1 = 1.5$, cladding $n_2 = 1.4$:

$$\\text{NA} = \\sqrt{1.5^2 - 1.4^2} = \\sqrt{2.25 - 1.96} = \\sqrt{0.29} \\approx 0.539$$

## Acceptance Angle

The **acceptance angle** $\\theta_a$ is the half-angle of the cone of light that will be accepted into the fiber (measured in air outside the fiber):

$$\\sin\\theta_a = \\text{NA} \\quad \\Rightarrow \\quad \\theta_a = \\arcsin(\\text{NA})$$

The full acceptance cone has a full angle of $2\\theta_a$.

**Example:** For the fiber above, $\\theta_a = \\arcsin(0.539) \\approx 32.6°$.

## Single-Mode vs Multi-Mode Fibers

- **Single-mode fiber**: very thin core (~9 µm), $n_1 \\approx n_2$, NA ≈ 0.12, carries one mode of light. Used for long-distance telecommunications.
- **Multi-mode fiber**: thicker core (~50–62.5 µm), NA ≈ 0.2–0.5, carries many modes. Used for short distances (data centers, buildings).

## Your Task

Implement the numerical aperture and acceptance angle using \`math.sqrt\` and \`math.asin\`.
`,
  starterCode: `import math

def numerical_aperture(n1, n2):
    # Return the numerical aperture of a fiber with core index n1 and cladding index n2
    pass

def acceptance_angle_deg(n1, n2):
    # Return the acceptance angle in degrees
    pass
`,
  solution: `import math

def numerical_aperture(n1, n2):
    return math.sqrt(n1 ** 2 - n2 ** 2)

def acceptance_angle_deg(n1, n2):
    NA = numerical_aperture(n1, n2)
    return math.degrees(math.asin(NA))
`,
  tests: [
    {
      name: "numerical_aperture(1.5, 1.4) = 0.5385",
      code: `{{FUNC}}
print(round(numerical_aperture(1.5, 1.4), 4))`,
      expected: "0.5385\n",
    },
    {
      name: "acceptance_angle_deg(1.5, 1.4) = 32.5827°",
      code: `{{FUNC}}
print(round(acceptance_angle_deg(1.5, 1.4), 4))`,
      expected: "32.5827\n",
    },
    {
      name: "numerical_aperture(1.46, 1.44) = 0.2408",
      code: `{{FUNC}}
print(round(numerical_aperture(1.46, 1.44), 4))`,
      expected: "0.2408\n",
    },
    {
      name: "acceptance_angle_deg(1.46, 1.44) = 13.9356°",
      code: `{{FUNC}}
print(round(acceptance_angle_deg(1.46, 1.44), 4))`,
      expected: "13.9356\n",
    },
  ],
};
