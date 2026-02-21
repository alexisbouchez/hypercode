import type { Lesson } from "../../types";

export const langmuirProbe: Lesson = {
  id: "langmuir-probe",
  title: "Langmuir Probe",
  chapterId: "diagnostics",
  content: `# Langmuir Probe Diagnostics

A **Langmuir probe** is a small electrode inserted into a plasma to measure its local properties — electron temperature, electron density, and plasma potential. It is one of the most widely used plasma diagnostic tools.

## I-V Characteristic

When a bias voltage \\(V\\) is applied to the probe, the current has contributions from ions and electrons:

$$I(V) = I_{\\text{sat,i}} + I_{e,\\text{sat}} \\exp\\!\\left(\\frac{e(V - V_p)}{k_B T_e}\\right)$$

Below the plasma potential \\(V_p\\), ions flow freely and electrons are repelled, giving a characteristic exponential shape.

## Bohm Velocity and Ion Saturation Current

Ions enter the probe's sheath at the **Bohm velocity**:

$$v_{\\text{Bohm}} = \\sqrt{\\frac{k_B T_e}{m_i}}$$

The **ion saturation current** is:

$$I_{\\text{sat}} = 0.5 \\cdot n \\cdot e \\cdot A \\cdot v_{\\text{Bohm}}$$

where \\(A\\) is the probe area and \\(e = 1.602 \\times 10^{-19}\\) C.

## Electron Temperature from Two Points

On the electron-retardation part of the I-V curve, the current grows exponentially with voltage. Measuring two points \\((V_1, I_1)\\) and \\((V_2, I_2)\\) gives:

$$T_e = \\frac{V_1 - V_2}{\\ln(I_1 / I_2)} \\quad \\text{[eV]}$$

This is the standard method for extracting electron temperature from a Langmuir probe trace.
`,
  starterCode: `import math

def bohm_velocity_m_s(T_e_K, m_i_kg):
    # Bohm velocity in m/s
    pass

def ion_saturation_current_A(n_m3, T_e_K, m_i_kg, A_m2):
    # Ion saturation current in Amperes
    pass

def electron_temperature_eV(V1, V2, I1, I2):
    # Electron temperature in eV from two I-V points
    pass`,
  solution: `import math

def bohm_velocity_m_s(T_e_K, m_i_kg):
    k_B = 1.381e-23
    return math.sqrt(k_B * T_e_K / m_i_kg)

def ion_saturation_current_A(n_m3, T_e_K, m_i_kg, A_m2):
    e = 1.602e-19
    k_B = 1.381e-23
    v_bohm = math.sqrt(k_B * T_e_K / m_i_kg)
    return 0.5 * n_m3 * e * A_m2 * v_bohm

def electron_temperature_eV(V1, V2, I1, I2):
    return (V1 - V2) / math.log(I1 / I2)`,
  tests: [
    {
      name: "Bohm velocity in hydrogen plasma at T=10,000 K",
      expected: "9.0855e+03\n",
      code: `{{FUNC}}
print(f"{bohm_velocity_m_s(10000, 1.673e-27):.4e}")`,
    },
    {
      name: "Bohm velocity in hot hydrogen plasma at T=1e7 K",
      expected: "2.8731e+05\n",
      code: `{{FUNC}}
print(f"{bohm_velocity_m_s(1e7, 1.673e-27):.4e}")`,
    },
    {
      name: "Ion saturation current",
      expected: "7.2775e-02\n",
      code: `{{FUNC}}
print(f"{ion_saturation_current_A(1e18, 10000, 1.673e-27, 1e-4):.4e}")`,
    },
    {
      name: "Electron temperature from two I-V points",
      expected: "1.4427\n",
      code: `{{FUNC}}
print(f"{electron_temperature_eV(0, -1, 1e-3, 5e-4):.4f}")`,
    },
  ],
};
