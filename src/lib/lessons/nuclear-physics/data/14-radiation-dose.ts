import type { Lesson } from "../../types";

export const radiationDose: Lesson = {
	id: "radiation-dose",
	title: "Radiation Dose",
	chapterId: "nuclear-energy",
	content: `## Radiation Dose

When radiation passes through tissue, it deposits energy. Quantifying this energy deposition is essential for radiation protection, medical imaging, and reactor safety.

### Absorbed Dose

The **absorbed dose** $D$ is the energy deposited per unit mass:

$$D = \\frac{E}{m} \\quad [\\text{Gray, Gy} = \\text{J/kg}]$$

### Equivalent Dose

Not all radiation types cause equal biological damage for the same absorbed dose. The **equivalent dose** $H$ accounts for this via the **radiation weighting factor** $w_R$:

$$H = D \\times w_R \\quad [\\text{Sievert, Sv}]$$

| Radiation type | $w_R$ |
|----------------|-------|
| Photons / electrons | 1 |
| Protons | 2 |
| Neutrons | ~10 |
| Alpha particles | 20 |

### Annual Dose Rate

For a radioactive source with activity $A$ (Bq = decays per second), depositing energy $E_d$ per decay into a mass $m$:

$$\\dot{H} = \\frac{A \\cdot E_d}{m} \\times 3.156 \\times 10^7 \\text{ s/year} \\times 10^3 \\text{ mSv/Sv}$$

### Exposure Limits

- General public: **1 mSv/year**
- Radiation workers: **20 mSv/year**
- Single diagnostic CT scan: ~2–10 mSv

### Your Task

All constants must be defined **inside** each function.

- \`absorbed_dose_Gy(energy_J, mass_kg)\` — absorbed dose in Gray
- \`equivalent_dose_Sv(absorbed_Gy, weighting_factor)\` — equivalent dose in Sievert
- \`annual_dose_rate_mSv(activity_Bq, dose_per_decay_J, mass_kg)\` — annual dose rate in mSv/year ($3.156 \\times 10^7$ s/year)`,

	starterCode: `import math

def absorbed_dose_Gy(energy_J, mass_kg):
    # D = energy / mass [Gy = J/kg]
    pass

def equivalent_dose_Sv(absorbed_Gy, weighting_factor):
    # H = D * w_R [Sv]
    pass

def annual_dose_rate_mSv(activity_Bq, dose_per_decay_J, mass_kg):
    # seconds_per_year = 3.156e7
    # result in mSv/year
    pass
`,

	solution: `import math

def absorbed_dose_Gy(energy_J, mass_kg):
    return energy_J / mass_kg

def equivalent_dose_Sv(absorbed_Gy, weighting_factor):
    return absorbed_Gy * weighting_factor

def annual_dose_rate_mSv(activity_Bq, dose_per_decay_J, mass_kg):
    seconds_per_year = 3.156e7
    mSv_per_Sv = 1e3
    return activity_Bq * dose_per_decay_J / mass_kg * seconds_per_year * mSv_per_Sv
`,

	tests: [
		{
			name: "absorbed_dose_Gy(1e-3, 1.0) = 0.001 Gy",
			code: `{{FUNC}}
print(absorbed_dose_Gy(1e-3, 1.0))`,
			expected: "0.001\n",
		},
		{
			name: "equivalent_dose_Sv(0.001, 20) = 0.02 Sv (alpha particle)",
			code: `{{FUNC}}
print(equivalent_dose_Sv(0.001, 20))`,
			expected: "0.02\n",
		},
		{
			name: "equivalent_dose_Sv(0.05, 1) = 0.05 Sv (photon)",
			code: `{{FUNC}}
print(equivalent_dose_Sv(0.05, 1))`,
			expected: "0.05\n",
		},
		{
			name: "annual_dose_rate_mSv(1000, 1e-13, 70) ≈ 0.045086 mSv/year",
			code: `{{FUNC}}
print(round(annual_dose_rate_mSv(1000, 1e-13, 70), 6))`,
			expected: "0.045086\n",
		},
	],
};
