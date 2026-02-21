import type { Lesson } from "../../types";

export const hallEffect: Lesson = {
  id: "hall-effect",
  title: "Hall Effect",
  chapterId: "semiconductors",
  content: `# Hall Effect

The **Hall effect** occurs when a current-carrying conductor is placed in a perpendicular magnetic field. The Lorentz force deflects charge carriers, building up a transverse electric field — the **Hall voltage**.

## Setup

- Current I flows in the **x-direction**
- Magnetic field B points in the **z-direction**
- Hall voltage V_H develops in the **y-direction**

## Hall Coefficient

The **Hall coefficient** characterizes the sign and density of charge carriers:

$$R_H = \\frac{1}{ne}$$

where n is the carrier density (m⁻³) and e = 1.602×10⁻¹⁹ C. For electron carriers, R_H is negative (electrons deflect opposite to holes), but the magnitude is |R_H| = 1/(n|e|).

## Hall Voltage

For a sample of thickness t (in the B-direction):

$$V_H = \\frac{IB}{net}$$

This follows from equilibrium between the Lorentz force (evB) and the Hall electric force (eE_H).

## Measuring Carrier Density

Rearranging: n = 1/(R_H · e), so a Hall measurement directly gives the carrier density.

## Applications

The Hall effect is used to:
- Determine carrier type (electrons vs. holes)
- Measure carrier density in metals and semiconductors
- Build Hall-effect sensors for magnetic field measurement

## Your Task

\`\`\`python
def hall_coefficient_m3_C(n_m3):
    # R_H = 1 / (n * e), e = 1.602e-19
    pass

def hall_voltage_V(I_A, B_T, n_m3, t_m):
    # V_H = I * B / (n * e * t)
    pass

def carrier_density_from_hall(R_H_m3_C):
    # n = 1 / (R_H * e)
    pass
\`\`\`
`,
  starterCode: `def hall_coefficient_m3_C(n_m3):
    # R_H = 1 / (n * e), use e = 1.602e-19 C
    pass

def hall_voltage_V(I_A, B_T, n_m3, t_m):
    # V_H = I * B / (n * e * t)
    pass

def carrier_density_from_hall(R_H_m3_C):
    # n = 1 / (R_H * e)
    pass
`,
  solution: `def hall_coefficient_m3_C(n_m3):
    e = 1.602e-19
    return 1.0 / (n_m3 * e)

def hall_voltage_V(I_A, B_T, n_m3, t_m):
    e = 1.602e-19
    return (I_A * B_T) / (n_m3 * e * t_m)

def carrier_density_from_hall(R_H_m3_C):
    e = 1.602e-19
    return 1.0 / (R_H_m3_C * e)
`,
  tests: [
    {
      name: "Hall coefficient for copper (n=8.49e28 m⁻³)",
      expected: "7.3524e-11\n",
      code: `{{FUNC}}
print(f"{hall_coefficient_m3_C(8.49e28):.4e}")`,
    },
    {
      name: "Hall voltage in copper: I=1A, B=1T, t=1mm",
      expected: "7.3524e-08\n",
      code: `{{FUNC}}
print(f"{hall_voltage_V(1.0, 1.0, 8.49e28, 1e-3):.4e}")`,
    },
    {
      name: "Carrier density from measured Hall coefficient R_H=6e-11",
      expected: "1.0404e+29\n",
      code: `{{FUNC}}
print(f"{carrier_density_from_hall(6.0e-11):.4e}")`,
    },
    {
      name: "Hall voltage in semiconductor: I=10mA, B=0.5T, n=1e22, t=0.1mm",
      expected: "3.1211e-02\n",
      code: `{{FUNC}}
print(f"{hall_voltage_V(0.01, 0.5, 1e22, 1e-4):.4e}")`,
    },
  ],
};
