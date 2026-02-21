import type { Lesson } from "../../types";

export const osmoticPressure: Lesson = {
  id: "osmotic-pressure",
  title: "Osmotic Pressure",
  chapterId: "cell-kinetics",
  content: `# Osmotic Pressure

## Osmosis

**Osmosis** is the net movement of water across a semipermeable membrane from a region of low solute concentration to high solute concentration. The pressure required to halt this flow is the **osmotic pressure**.

## Van't Hoff Equation

$$\\Pi = i \\cdot M \\cdot R \\cdot T$$

Where:
- **Π** — osmotic pressure (Pa)
- **i** — van't Hoff factor (number of ions per formula unit)
- **M** — molar concentration (mol/m³; 1 mM = 1 mol/m³)
- **R** = 8.314 J·mol⁻¹·K⁻¹
- **T** — absolute temperature (K)

### Van't Hoff Factors

| Solute | i |
|--------|---|
| Glucose, urea | 1 |
| NaCl | 2 |
| CaCl₂ | 3 |
| Na₂SO₄ | 3 |

## Physiological Osmotic Pressure

Normal blood plasma osmolarity ≈ 308 mOsm/L, giving Π ≈ 5700–6000 mmHg (~7.8 atm) at body temperature.

## Osmolarity

$$\\text{Osmolarity (Osm/L)} = i \\times M \\text{ (mol/L)}$$

0.9% NaCl (physiological saline): 154 mM NaCl × i=2 = **308 mOsm/L**.

## Water Potential

In plant physiology, water potential:

$$\\Psi = \\Psi_s + \\Psi_p$$

Where Ψ_s = −Π (solute potential) and Ψ_p is turgor pressure.

## Unit Conversion

- 1 mmHg = 133.322 Pa
- 1 atm = 101,325 Pa = 760 mmHg

## Functions to Implement

- \`osmotic_pressure_Pa(M_mol_m3, T_K=310, i=1)\` — osmotic pressure in Pascals
- \`osmotic_pressure_mmHg(M_mol_m3, T_K=310, i=1)\` — osmotic pressure in mmHg
- \`osmolarity_osmol_L(M_mol_L, i=1)\` — osmolarity in Osm/L
`,
  starterCode: `def osmotic_pressure_Pa(M_mol_m3, T_K=310, i=1):
    pass

def osmotic_pressure_mmHg(M_mol_m3, T_K=310, i=1):
    pass

def osmolarity_osmol_L(M_mol_L, i=1):
    pass
`,
  solution: `def osmotic_pressure_Pa(M_mol_m3, T_K=310, i=1):
    R = 8.314
    return i * M_mol_m3 * R * T_K

def osmotic_pressure_mmHg(M_mol_m3, T_K=310, i=1):
    Pa = osmotic_pressure_Pa(M_mol_m3, T_K, i)
    return Pa / 133.322

def osmolarity_osmol_L(M_mol_L, i=1):
    return i * M_mol_L
`,
  tests: [
    {
      name: "osmotic_pressure_mmHg for physiological NaCl (154 mol/m³, i=2)",
      expected: "5954.16\n",
      code: `{{FUNC}}\nprint(f"{osmotic_pressure_mmHg(154, i=2):.2f}")`,
    },
    {
      name: "osmotic_pressure_mmHg for 1 mol/m³ non-electrolyte at 310K",
      expected: "19.3317\n",
      code: `{{FUNC}}\nprint(f"{osmotic_pressure_mmHg(1, i=1):.4f}")`,
    },
    {
      name: "osmolarity_osmol_L for 0.154 mol/L NaCl",
      expected: "0.3080\n",
      code: `{{FUNC}}\nprint(f"{osmolarity_osmol_L(0.154, i=2):.4f}")`,
    },
    {
      name: "osmotic_pressure_Pa for 1 mol/m³ at 298K",
      expected: "2477.5720\n",
      code: `{{FUNC}}\nprint(f"{osmotic_pressure_Pa(1, T_K=298, i=1):.4f}")`,
    },
  ],
};
