import type { Lesson } from "../../types";

export const fret: Lesson = {
  id: "fret",
  title: "Förster Resonance Energy Transfer",
  chapterId: "molecular",
  content: `# Förster Resonance Energy Transfer (FRET)

## What is FRET?

FRET is a distance-dependent energy transfer between two fluorescent molecules — a **donor** and an **acceptor**. When they are close (1–10 nm), excited donor molecules transfer energy non-radiatively to the acceptor via dipole-dipole coupling.

FRET is called a "molecular ruler" because the transfer efficiency depends sharply on the donor-acceptor distance.

## Transfer Efficiency

$$E = \\frac{1}{1 + \\left(\\frac{r}{R_0}\\right)^6}$$

- **r** = donor-acceptor distance (nm)
- **R_0** = Förster radius — the distance at which E = 0.5 (typically 2–10 nm)

The **sixth-power** dependence makes FRET highly sensitive to distance near R_0:

| r / R_0 | E |
|---------|---|
| 0.5 | 0.98 |
| 1.0 | 0.50 |
| 1.5 | 0.09 |
| 2.0 | 0.015 |

## Distance from Efficiency

Rearranging the efficiency equation:

$$r = R_0 \\left(\\frac{1}{E} - 1\\right)^{1/6}$$

## FRET Rate

The rate of energy transfer from donor to acceptor is:

$$k_T = \\frac{1}{\\tau_D} \\left(\\frac{R_0}{r}\\right)^6$$

where **τ_D** is the donor fluorescence lifetime **without** acceptor (typically 1–10 ns). The FRET efficiency can also be expressed as:

$$E = \\frac{k_T}{k_T + 1/\\tau_D}$$

## Biological Applications

- Protein conformational changes (intramolecular FRET)
- Protein-protein interactions
- DNA hybridization assays
- Live-cell imaging of signaling events

## Your Task

Implement three functions:

1. **\`fret_efficiency(r_nm, R0_nm)\`** — FRET transfer efficiency (0 to 1)
2. **\`fret_distance_nm(E, R0_nm)\`** — donor-acceptor distance from measured efficiency
3. **\`fret_rate_s(r_nm, R0_nm, tau_D_s)\`** — FRET rate constant in s⁻¹
`,
  starterCode: `def fret_efficiency(r_nm, R0_nm):
    # E = 1 / (1 + (r / R0)^6)
    pass

def fret_distance_nm(E, R0_nm):
    # r = R0 * (1/E - 1)^(1/6)
    pass

def fret_rate_s(r_nm, R0_nm, tau_D_s):
    # k_T = (1 / tau_D) * (R0 / r)^6
    pass
`,
  solution: `def fret_efficiency(r_nm, R0_nm):
    return 1 / (1 + (r_nm / R0_nm)**6)

def fret_distance_nm(E, R0_nm):
    return R0_nm * ((1 / E - 1) ** (1 / 6))

def fret_rate_s(r_nm, R0_nm, tau_D_s):
    return (1 / tau_D_s) * (R0_nm / r_nm)**6
`,
  tests: [
    {
      name: "FRET efficiency at r = R_0 (should be 0.5)",
      expected: "0.5000\n",
      code: `{{FUNC}}
print(f"{fret_efficiency(5.0, 5.0):.4f}")`,
    },
    {
      name: "FRET efficiency at r = 3 nm, R_0 = 5 nm (high efficiency)",
      expected: "0.9554\n",
      code: `{{FUNC}}
print(f"{fret_efficiency(3.0, 5.0):.4f}")`,
    },
    {
      name: "Distance from efficiency E=0.5, R_0=6 nm (should return R_0=6)",
      expected: "6.0000\n",
      code: `{{FUNC}}
print(f"{fret_distance_nm(0.5, 6.0):.4f}")`,
    },
    {
      name: "FRET rate at r=R_0=5 nm, tau_D=4 ns",
      expected: "2.5000e+08\n",
      code: `{{FUNC}}
print(f"{fret_rate_s(5.0, 5.0, 4e-9):.4e}")`,
    },
  ],
};
