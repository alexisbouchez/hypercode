import type { Lesson } from "../../types";

export const activity: Lesson = {
	id: "activity",
	title: "Radioactive Activity",
	chapterId: "radioactive-decay",
	content: `## Radioactive Activity

**Activity** $A$ is the number of decays per second. It is directly proportional to the number of radioactive nuclei $N$ present:

$$A = \\lambda N \\quad [\\text{Becquerel, Bq}]$$

One Becquerel equals one decay per second. The older unit, the **Curie (Ci)**, was defined as the activity of 1 gram of radium-226:

$$1 \\text{ Ci} = 3.7 \\times 10^{10} \\text{ Bq}$$

### Specific Activity

The **specific activity** is the activity per unit mass. For a pure isotope:

$$A_{\\text{spec}} = \\frac{\\lambda N_A}{M} = \\frac{\\ln 2}{t_{1/2}} \\cdot \\frac{N_A}{M} \\quad [\\text{Bq/kg}]$$

where $N_A = 6.022 \\times 10^{23}$ mol⁻¹ is Avogadro's number and $M$ is the molar mass in kg/mol.

### Examples

- **C-14** ($t_{1/2} = 5730$ yr, $M = 14 \\times 10^{-3}$ kg/mol): high specific activity due to short half-life
- **Ra-226** ($t_{1/2} = 1600$ yr, $M = 226 \\times 10^{-3}$ kg/mol): ~1 Ci/g by definition
- **U-238** ($t_{1/2} = 4.468 \\times 10^9$ yr): very low specific activity

### Your Task

Implement:
- \`activity_bq(N, lambda_s)\` — activity in Bq given number of nuclei and decay constant in s⁻¹
- \`bq_to_curie(activity_bq)\` — convert Bq to Curie
- \`specific_activity(half_life_s, molar_mass_kg_per_mol)\` — specific activity in Bq/kg

All constants ($N_A$, Ci definition) must be inside each function.`,

	starterCode: `import math

def activity_bq(N, lambda_s):
    # A = lambda * N
    pass

def bq_to_curie(activity_bq):
    # 1 Ci = 3.7e10 Bq
    pass

def specific_activity(half_life_s, molar_mass_kg_per_mol):
    NA = 6.022e23
    # A_spec = ln(2) / t_half * NA / M
    pass
`,

	solution: `import math

def activity_bq(N, lambda_s):
    return lambda_s * N

def bq_to_curie(activity_bq):
    return activity_bq / 3.7e10

def specific_activity(half_life_s, molar_mass_kg_per_mol):
    NA = 6.022e23
    return math.log(2) / half_life_s * NA / molar_mass_kg_per_mol
`,

	tests: [
		{
			name: "specific_activity of C-14 ≈ 1.6488e+14 Bq/kg",
			code: `{{FUNC}}
import math
c14_hl_s = 5730 * 365.25 * 24 * 3600
print(f"{specific_activity(c14_hl_s, 14e-3):.4e}")`,
			expected: "1.6488e+14\n",
		},
		{
			name: "specific_activity of Ra-226 ≈ 3.6579e+13 Bq/kg",
			code: `{{FUNC}}
import math
ra226_hl_s = 1600 * 365.25 * 24 * 3600
print(f"{specific_activity(ra226_hl_s, 226e-3):.4e}")`,
			expected: "3.6579e+13\n",
		},
		{
			name: "bq_to_curie(3.7e10) = 1.0",
			code: `{{FUNC}}
print(bq_to_curie(3.7e10))`,
			expected: "1.0\n",
		},
		{
			name: "activity_bq(1e20, 1e-8) = 1e12",
			code: `{{FUNC}}
print(activity_bq(1e20, 1e-8))`,
			expected: "1000000000000.0\n",
		},
	],
};
