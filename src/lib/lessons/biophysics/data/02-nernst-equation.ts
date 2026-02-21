import type { Lesson } from "../../types";

export const nernstEquation: Lesson = {
  id: "nernst-equation",
  title: "Nernst Equation",
  chapterId: "molecular",
  content: `# Nernst Equation

## Electrochemical Equilibrium

Biological membranes maintain ion concentration gradients that drive electrical signaling. The **Nernst equation** gives the equilibrium membrane potential for a single ion species.

## The Nernst Equation

$$E = \\frac{RT}{zF} \\ln\\left(\\frac{[\\text{ion}]_{\\text{out}}}{[\\text{ion}]_{\\text{in}}}\\right)$$

- **R** = 8.314 J/mol·K (gas constant)
- **T** = temperature in Kelvin (body temperature ≈ 310 K)
- **z** = ion valence (e.g., +1 for K⁺, +2 for Ca²⁺, −1 for Cl⁻)
- **F** = 96485 C/mol (Faraday constant)

At 37°C: RT/F ≈ 26.7 mV

## Typical Ion Concentrations (mammalian neuron)

| Ion | [out] mM | [in] mM | E_Nernst |
|-----|----------|---------|----------|
| K⁺  | 5        | 140     | −89 mV   |
| Na⁺ | 145      | 12      | +67 mV   |
| Cl⁻ | 120      | 4       | −91 mV   |

## Goldman-Hodgkin-Katz Equation

The resting membrane potential depends on the **permeability** of each ion. The Goldman equation accounts for K⁺, Na⁺, and Cl⁻:

$$V_m = \\frac{RT}{F} \\ln\\left(\\frac{P_K[K^+]_o + P_{Na}[Na^+]_o + P_{Cl}[Cl^-]_i}{P_K[K^+]_i + P_{Na}[Na^+]_i + P_{Cl}[Cl^-]_o}\\right)$$

Typical permeability ratios at rest: **P_K : P_Na : P_Cl = 1 : 0.04 : 0.45**

Note that Cl⁻ appears **flipped** (intracellular on top for Cl⁻) because it carries negative charge.

## Your Task

Implement three functions:

1. **\`nernst_potential_V(z, c_out, c_in, T_K=310)\`** — Nernst potential in Volts
2. **\`nernst_potential_mV(z, c_out, c_in, T_K=310)\`** — Nernst potential in millivolts
3. **\`goldman_potential_mV(K_o, K_i, Na_o, Na_i, Cl_o, Cl_i, P_K=1, P_Na=0.04, P_Cl=0.45, T_K=310)\`** — Goldman membrane potential in mV
`,
  starterCode: `import math

def nernst_potential_V(z, c_out, c_in, T_K=310):
    # E = (R * T) / (z * F) * ln(c_out / c_in)
    # R = 8.314, F = 96485
    pass

def nernst_potential_mV(z, c_out, c_in, T_K=310):
    # Convert V to mV
    pass

def goldman_potential_mV(K_o, K_i, Na_o, Na_i, Cl_o, Cl_i,
                         P_K=1, P_Na=0.04, P_Cl=0.45, T_K=310):
    # Vm = (RT/F) * ln((P_K*K_o + P_Na*Na_o + P_Cl*Cl_i) /
    #                  (P_K*K_i + P_Na*Na_i + P_Cl*Cl_o)) * 1000
    pass
`,
  solution: `import math

def nernst_potential_V(z, c_out, c_in, T_K=310):
    R = 8.314
    F = 96485
    return (R * T_K) / (z * F) * math.log(c_out / c_in)

def nernst_potential_mV(z, c_out, c_in, T_K=310):
    R = 8.314
    F = 96485
    return (R * T_K) / (z * F) * math.log(c_out / c_in) * 1000

def goldman_potential_mV(K_o, K_i, Na_o, Na_i, Cl_o, Cl_i,
                         P_K=1, P_Na=0.04, P_Cl=0.45, T_K=310):
    R = 8.314
    F = 96485
    num = P_K * K_o + P_Na * Na_o + P_Cl * Cl_i
    den = P_K * K_i + P_Na * Na_i + P_Cl * Cl_o
    return (R * T_K / F) * math.log(num / den) * 1000
`,
  tests: [
    {
      name: "K+ Nernst potential ([K+]_out=5 mM, [K+]_in=140 mM)",
      expected: "-89.01\n",
      code: `{{FUNC}}
print(f"{nernst_potential_mV(1, 5, 140):.2f}")`,
    },
    {
      name: "Na+ Nernst potential ([Na+]_out=145 mM, [Na+]_in=12 mM)",
      expected: "66.56\n",
      code: `{{FUNC}}
print(f"{nernst_potential_mV(1, 145, 12):.2f}")`,
    },
    {
      name: "Cl- Nernst potential ([Cl-]_out=120 mM, [Cl-]_in=4 mM)",
      expected: "-90.85\n",
      code: `{{FUNC}}
print(f"{nernst_potential_mV(-1, 120, 4):.2f}")`,
    },
    {
      name: "Goldman resting membrane potential (typical neuron)",
      expected: "-73.10\n",
      code: `{{FUNC}}
print(f"{goldman_potential_mV(5, 140, 145, 12, 120, 4):.2f}")`,
    },
  ],
};
