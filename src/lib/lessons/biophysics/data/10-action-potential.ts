import type { Lesson } from "../../types";

export const actionPotential: Lesson = {
  id: "action-potential",
  title: "Action Potential & Cable Equation",
  chapterId: "neural",
  content: `# Action Potential & Cable Equation

## Neuronal Signaling

Neurons transmit electrical signals along axons. The **cable equation** describes how voltage spreads along a cylindrical axon, balancing membrane leakage against axial current flow.

## Space Constant (λ)

The **space constant** λ determines how far a subthreshold voltage deflection spreads:

$$\\lambda = \\sqrt{\\frac{R_m \\cdot a}{2 R_i}}$$

Where:
- **a** — axon radius (m)
- **R_m** — specific membrane resistance (Ω·m²); typical: 0.1 Ω·m²
- **R_i** — intracellular resistivity (Ω·m); typical: 1.0 Ω·m

A larger λ means signals travel farther without decaying.

## Time Constant (τ)

The **time constant** τ governs how quickly the membrane voltage responds:

$$\\tau = R_m \\cdot C_m$$

Where:
- **C_m** — specific membrane capacitance (F/m²); typical: 0.01 F/m²

Typical τ ≈ 1 ms for unmyelinated axons.

## Propagation Velocity (rough estimate)

$$v \\approx \\frac{\\lambda}{\\tau}$$

This approximation shows that thicker axons (larger a → larger λ) conduct faster.

## Giant Squid Axon

The squid giant axon (diameter ~1 mm, radius ~500 μm) is a classic model system:
- λ ≈ 5 mm
- Conduction velocity ~20 m/s

## Myelination

Myelin sheaths increase effective R_m and decrease C_m dramatically, increasing both λ and decreasing τ, yielding saltatory conduction at speeds up to 120 m/s in humans.

## Typical Parameters

| Parameter | Value |
|-----------|-------|
| R_m | 0.1 Ω·m² |
| R_i | 1.0 Ω·m |
| C_m | 0.01 F/m² |
| a (squid axon) | 500 μm |
| a (mammalian) | 0.5–5 μm |

## Functions to Implement

- \`space_constant_m(a_m, R_m_ohm_m2=0.1, R_i_ohm_m=1.0)\` — λ in meters
- \`time_constant_s(R_m_ohm_m2=0.1, C_m_F_m2=0.01)\` — τ in seconds
- \`propagation_velocity_m_s(a_m, R_m_ohm_m2=0.1, R_i_ohm_m=1.0, C_m_F_m2=0.01)\` — v in m/s
`,
  starterCode: `import math

def space_constant_m(a_m, R_m_ohm_m2=0.1, R_i_ohm_m=1.0):
    pass

def time_constant_s(R_m_ohm_m2=0.1, C_m_F_m2=0.01):
    pass

def propagation_velocity_m_s(a_m, R_m_ohm_m2=0.1, R_i_ohm_m=1.0, C_m_F_m2=0.01):
    pass
`,
  solution: `import math

def space_constant_m(a_m, R_m_ohm_m2=0.1, R_i_ohm_m=1.0):
    return math.sqrt(R_m_ohm_m2 * a_m / (2 * R_i_ohm_m))

def time_constant_s(R_m_ohm_m2=0.1, C_m_F_m2=0.01):
    return R_m_ohm_m2 * C_m_F_m2

def propagation_velocity_m_s(a_m, R_m_ohm_m2=0.1, R_i_ohm_m=1.0, C_m_F_m2=0.01):
    lam = space_constant_m(a_m, R_m_ohm_m2, R_i_ohm_m)
    tau = time_constant_s(R_m_ohm_m2, C_m_F_m2)
    return lam / tau
`,
  tests: [
    {
      name: "space_constant_m for axon radius 5 μm",
      expected: "5.0000e-04\n",
      code: `{{FUNC}}\nprint(f"{space_constant_m(5e-6):.4e}")`,
    },
    {
      name: "space_constant_m for squid giant axon (500 μm radius)",
      expected: "5.0000e-03\n",
      code: `{{FUNC}}\nprint(f"{space_constant_m(500e-6):.4e}")`,
    },
    {
      name: "time_constant_s with default parameters",
      expected: "0.0010\n",
      code: `{{FUNC}}\nprint(f"{time_constant_s():.4f}")`,
    },
    {
      name: "propagation_velocity_m_s for squid axon",
      expected: "5.0000\n",
      code: `{{FUNC}}\nprint(f"{propagation_velocity_m_s(500e-6):.4f}")`,
    },
  ],
};
