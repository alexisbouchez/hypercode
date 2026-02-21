import type { Lesson } from "../../types";

export const magneticMirror: Lesson = {
  id: "magnetic-mirror",
  title: "Magnetic Mirror",
  chapterId: "single-particle",
  content: `## Magnetic Mirror

A **magnetic mirror** is a region where the magnetic field strength increases along field lines, trapping charged particles through a magnetic force.

### The Mirror Effect

As a charged particle moves into a region of stronger **B**, its perpendicular velocity \\(v_{\\perp}\\) increases (to conserve the magnetic moment \\(\\mu = mv_{\\perp}^2 / 2B\\)). By conservation of kinetic energy, the parallel velocity \\(v_{\\parallel}\\) must decrease. If \\(v_{\\parallel}\\) reaches zero, the particle is reflected.

### Mirror Ratio

The **mirror ratio** characterizes how strong the mirror is:

$$R_m = \\frac{B_{max}}{B_{min}}$$

A higher mirror ratio means a stronger mirror that traps more particles.

### Loss Cone

Particles whose pitch angle \\(\\theta\\) (angle between velocity and **B**) satisfies:

$$\\sin^2(\\theta_{lc}) = \\frac{B_{min}}{B_{max}} = \\frac{1}{R_m}$$

are on the **loss cone boundary**. Particles with \\(\\theta < \\theta_{lc}\\) (too much parallel velocity) escape the mirror. Those with \\(\\theta > \\theta_{lc}\\) are trapped.

### Fraction Trapped

The fraction of an isotropic distribution that is trapped:

$$f_{trapped} = 1 - \\frac{1}{\\sqrt{R_m}}$$

### Pitch Angle

The pitch angle of a particle with velocities \\(v_{\\perp}\\) and \\(v_{\\parallel}\\):

$$\\theta = \\arctan\\left(\\frac{v_{\\perp}}{v_{\\parallel}}\\right)$$

### Applications

Magnetic mirrors appear in:
- **Earth's Van Allen belts** — natural magnetic mirrors trapping high-energy particles
- **Fusion devices** — mirror machines attempt to confine plasma for energy production
- **Solar corona** — particles are trapped between magnetic loops
`,
  starterCode: `import math

def loss_cone_angle_deg(R_m):
    # Return loss cone angle in degrees
    pass

def fraction_trapped(R_m):
    # Return fraction of isotropic distribution that is trapped
    pass

def pitch_angle_deg(v_perp_m_s, v_par_m_s):
    # Return pitch angle in degrees
    pass`,
  solution: `import math

def loss_cone_angle_deg(R_m):
    sin2 = 1.0 / R_m
    return math.degrees(math.asin(math.sqrt(sin2)))

def fraction_trapped(R_m):
    return 1 - 1 / math.sqrt(R_m)

def pitch_angle_deg(v_perp_m_s, v_par_m_s):
    return math.degrees(math.atan2(v_perp_m_s, v_par_m_s))`,
  tests: [
    {
      name: "Loss cone angle for mirror ratio R_m=5",
      expected: "26.5651\n",
      code: `{{FUNC}}
print(f"{loss_cone_angle_deg(5):.4f}")`,
    },
    {
      name: "Loss cone angle for mirror ratio R_m=10",
      expected: "18.4349\n",
      code: `{{FUNC}}
print(f"{loss_cone_angle_deg(10):.4f}")`,
    },
    {
      name: "Fraction trapped for R_m=5",
      expected: "0.5528\n",
      code: `{{FUNC}}
print(f"{fraction_trapped(5):.4f}")`,
    },
    {
      name: "Pitch angle: equal perpendicular and parallel velocities (45°)",
      expected: "45.0000\n",
      code: `{{FUNC}}
print(f"{pitch_angle_deg(1e6, 1e6):.4f}")`,
    },
  ],
};
