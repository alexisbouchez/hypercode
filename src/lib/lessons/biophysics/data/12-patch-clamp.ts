import type { Lesson } from "../../types";

export const patchClamp: Lesson = {
  id: "patch-clamp",
  title: "Patch Clamp & Ion Channels",
  chapterId: "neural",
  content: `# Patch Clamp & Ion Channels

## Patch Clamp Electrophysiology

Patch clamp is the gold-standard technique for measuring electrical currents through individual ion channels. Developed by Neher and Sakmann (Nobel Prize 1991), it allows recording of single-channel events in the picoampere range.

## Single-Channel Current

Each open ion channel carries a current **i** determined by Ohm's law:

$$i = \\gamma \\cdot (V_m - E_{\\text{rev}})$$

- **γ** = single-channel conductance (picosiemens, pS = 10⁻¹² S)
- **V_m** = membrane potential (mV)
- **E_rev** = reversal potential — the voltage at which no net current flows (mV)

The reversal potential is given by the Nernst equation. For K⁺ it is typically **−90 mV**; for Na⁺ it is typically **+60 mV**.

## Boltzmann Open Probability

Voltage-gated channels open and close with a probability that depends on membrane potential. The **open probability P_open** follows a Boltzmann distribution:

$$P_{\\text{open}} = \\frac{1}{1 + \\exp\\left(-\\dfrac{V_m - V_{1/2}}{k_{\\text{slope}}}\\right)}$$

- **V_{1/2}** = half-activation voltage (mV) — membrane potential at which P_open = 0.5
- **k_slope** = slope factor (mV) — steepness of voltage dependence (~10 mV)

## Whole-Cell Current

In whole-cell mode, the total macroscopic current from **N** identical channels is:

$$I = N \\cdot P_{\\text{open}} \\cdot i$$

## Your Task

Implement three functions:

1. **\`single_channel_current_pA(gamma_pS, V_m_mV, E_rev_mV)\`** — Single-channel current in pA
2. **\`open_probability(V_m_mV, V_half_mV, k_slope_mV=10)\`** — Channel open probability (0–1)
3. **\`whole_cell_current_pA(N_channels, gamma_pS, V_m_mV, E_rev_mV, V_half_mV, k_slope_mV=10)\`** — Total whole-cell current in pA
`,
  starterCode: `import math

def single_channel_current_pA(gamma_pS, V_m_mV, E_rev_mV):
    # i = gamma * (V_m - E_rev), convert units carefully
    # gamma in pS (1e-12 S), V in mV (1e-3 V), return current in pA (1e-12 A)
    pass

def open_probability(V_m_mV, V_half_mV, k_slope_mV=10):
    # Boltzmann: P = 1 / (1 + exp(-(V_m - V_half) / k_slope))
    pass

def whole_cell_current_pA(N_channels, gamma_pS, V_m_mV, E_rev_mV, V_half_mV, k_slope_mV=10):
    # I = N * P_open * i
    pass
`,
  solution: `import math

def single_channel_current_pA(gamma_pS, V_m_mV, E_rev_mV):
    gamma_S = gamma_pS * 1e-12
    V_m_V = V_m_mV * 1e-3
    E_rev_V = E_rev_mV * 1e-3
    i_A = gamma_S * (V_m_V - E_rev_V)
    return i_A / 1e-12

def open_probability(V_m_mV, V_half_mV, k_slope_mV=10):
    return 1 / (1 + math.exp(-(V_m_mV - V_half_mV) / k_slope_mV))

def whole_cell_current_pA(N_channels, gamma_pS, V_m_mV, E_rev_mV, V_half_mV, k_slope_mV=10):
    i = single_channel_current_pA(gamma_pS, V_m_mV, E_rev_mV)
    P = open_probability(V_m_mV, V_half_mV, k_slope_mV)
    return N_channels * P * i
`,
  tests: [
    {
      name: "Single-channel current: 20 pS K+ channel at -70 mV (E_K=-90 mV)",
      expected: "0.4000\n",
      code: `{{FUNC}}
print(f"{single_channel_current_pA(20, -70, -90):.4f}")`,
    },
    {
      name: "Single-channel current: 40 pS Na+ channel at 0 mV (E_Na=+60 mV)",
      expected: "-2.4000\n",
      code: `{{FUNC}}
print(f"{single_channel_current_pA(40, 0, 60):.4f}")`,
    },
    {
      name: "Open probability at V_half (should be 0.5)",
      expected: "0.5000\n",
      code: `{{FUNC}}
print(f"{open_probability(-40, -40):.4f}")`,
    },
    {
      name: "Whole-cell current: 1000 K+ channels, 20 pS, -70 mV, E_K=-90 mV, V_half=-50 mV",
      expected: "47.6812\n",
      code: `{{FUNC}}
print(f"{whole_cell_current_pA(1000, 20, -70, -90, -50):.4f}")`,
    },
  ],
};
