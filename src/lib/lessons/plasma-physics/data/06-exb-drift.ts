import type { Lesson } from "../../types";

export const exbDrift: Lesson = {
  id: "exb-drift",
  title: "E×B Drift",
  chapterId: "single-particle",
  content: `## E×B Drift

When a plasma particle moves in crossed electric and magnetic fields, it undergoes a **E×B drift** — a bulk drift perpendicular to both **E** and **B**.

### E×B Drift Velocity

For electric field **E** perpendicular to magnetic field **B**, the drift velocity magnitude is:

$$v_{E \\times B} = \\frac{E}{B}$$

This drift is **species-independent**: electrons and ions drift at the same velocity in the same direction. This is a key feature that distinguishes E×B drift from other drifts.

### Grad-B Drift

When the magnetic field has a spatial gradient, particles also drift due to the varying Larmor radius:

$$v_{\\nabla B} = \\frac{v_{\\perp}^2}{2} \\cdot \\frac{m}{q B^2} \\cdot \\nabla B$$

Here \\(v_{\\perp}\\) is the perpendicular velocity, \\(m\\) is the particle mass, \\(q\\) is the charge, and \\(\\nabla B\\) is the magnitude of the magnetic field gradient.

Unlike E×B drift, grad-B drift is **charge-dependent** — electrons and ions drift in opposite directions, generating a current.

### Polarization Drift

When the electric field changes in time, there is an additional polarization drift:

$$v_{pol} = \\frac{m}{q B^2} \\frac{dE}{dt}$$

This drift is also charge-dependent and is important in time-varying fields.

### Physical Intuition

- **E×B drift**: The particle gains energy from **E** on one half of its orbit and loses it on the other. The net effect is a sideways drift.
- **Grad-B drift**: The Larmor radius is larger where **B** is weaker, so the orbit is asymmetric and the particle drifts.
- **Polarization drift**: A changing **E** field shifts the guiding center in the direction of **E**.
`,
  starterCode: `import math

def exb_drift_m_s(E_V_m, B_T):
    # Return E×B drift speed in m/s
    pass

def gradb_drift_m_s(v_perp_m_s, m_kg, q_C, B_T, grad_B_T_m):
    # Return grad-B drift speed in m/s
    pass

def polarization_drift_m_s(m_kg, q_C, B_T, dE_dt_V_m_s):
    # Return polarization drift speed in m/s
    pass`,
  solution: `import math

def exb_drift_m_s(E_V_m, B_T):
    return E_V_m / B_T

def gradb_drift_m_s(v_perp_m_s, m_kg, q_C, B_T, grad_B_T_m):
    return (v_perp_m_s**2 / 2) * (m_kg / (q_C * B_T**2)) * grad_B_T_m

def polarization_drift_m_s(m_kg, q_C, B_T, dE_dt_V_m_s):
    return m_kg / (q_C * B_T**2) * dE_dt_V_m_s`,
  tests: [
    {
      name: "E×B drift: E=1000 V/m, B=0.1 T",
      expected: "1.0000e+04\n",
      code: `{{FUNC}}
print(f"{exb_drift_m_s(1000, 0.1):.4e}")`,
    },
    {
      name: "E×B drift: fusion-scale (E=100 V/m, B=5 T)",
      expected: "2.0000e+01\n",
      code: `{{FUNC}}
print(f"{exb_drift_m_s(100, 5.0):.4e}")`,
    },
    {
      name: "Grad-B drift: electron (v_perp=1e6 m/s, B=1 T, gradB=0.1 T/m)",
      expected: "2.8430e-01\n",
      code: `{{FUNC}}
print(f"{gradb_drift_m_s(1e6, 9.109e-31, 1.602e-19, 1.0, 0.1):.4e}")`,
    },
    {
      name: "Grad-B drift: proton same conditions",
      expected: "5.2216e+02\n",
      code: `{{FUNC}}
print(f"{gradb_drift_m_s(1e6, 1.673e-27, 1.602e-19, 1.0, 0.1):.4e}")`,
    },
  ],
};
