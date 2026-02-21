import type { Lesson } from "../../types";

export const radiometricDating: Lesson = {
	id: "radiometric-dating",
	title: "Radiometric Dating",
	chapterId: "radioactive-decay",
	content: `## Radiometric Dating

Radioactive decay provides a natural clock. If we know the initial amount $N_0$ of a radioactive isotope and measure the fraction $N/N_0$ remaining today, we can calculate the age of the sample.

Starting from $N(t) = N_0 e^{-\\lambda t}$, solving for $t$:

$$t = -\\frac{\\ln(N/N_0)}{\\lambda} = \\frac{t_{1/2} \\cdot \\ln(N_0/N)}{\\ln 2}$$

### Common Radiometric Clocks

| Isotope | Half-life | Application |
|---------|-----------|-------------|
| Carbon-14 | 5,730 years | Organic material up to ~50,000 years |
| Uranium-238 | 4.468 × 10⁹ years | Rocks and minerals (billions of years) |
| Potassium-40 | 1.25 × 10⁹ years | Volcanic rocks |

### Carbon-14 Dating

Carbon-14 is produced continuously in the upper atmosphere by cosmic rays. Living organisms exchange carbon with the environment, maintaining a constant $^{14}$C/$^{12}$C ratio. When an organism dies, the exchange stops and $^{14}$C decays with $t_{1/2} = 5730$ years.

### Uranium-238 Dating

With $t_{1/2} = 4.468 \\times 10^9$ years, U-238 is used to date the oldest rocks and meteorites. The decay chain ends at stable Pb-206.

### Your Task

Implement three functions. All constants (half-lives) must be defined **inside** each function body.

- \`age_from_fraction(fraction_remaining, half_life_years)\` — general formula
- \`carbon14_age(fraction_remaining)\` — uses $t_{1/2} = 5730$ years internally
- \`uranium238_age(fraction_remaining)\` — uses $t_{1/2} = 4.468 \\times 10^9$ years internally`,

	starterCode: `import math

def age_from_fraction(fraction_remaining, half_life_years):
    # t = half_life * log(1/fraction) / log(2)
    pass

def carbon14_age(fraction_remaining):
    half_life_years = 5730
    # TODO: apply formula with C-14 half-life
    pass

def uranium238_age(fraction_remaining):
    half_life_years = 4.468e9
    # TODO: apply formula with U-238 half-life
    pass
`,

	solution: `import math

def age_from_fraction(fraction_remaining, half_life_years):
    return half_life_years * math.log(1 / fraction_remaining) / math.log(2)

def carbon14_age(fraction_remaining):
    half_life_years = 5730
    return half_life_years * math.log(1 / fraction_remaining) / math.log(2)

def uranium238_age(fraction_remaining):
    half_life_years = 4.468e9
    return half_life_years * math.log(1 / fraction_remaining) / math.log(2)
`,

	tests: [
		{
			name: "age_from_fraction(0.5, 5730) = 5730.0 (one half-life)",
			code: `{{FUNC}}
print(age_from_fraction(0.5, 5730))`,
			expected: "5730.0\n",
		},
		{
			name: "age_from_fraction(0.25, 5730) = 11460.0 (two half-lives)",
			code: `{{FUNC}}
print(age_from_fraction(0.25, 5730))`,
			expected: "11460.0\n",
		},
		{
			name: "carbon14_age(0.1) ≈ 19034.65 years",
			code: `{{FUNC}}
print(round(carbon14_age(0.1), 2))`,
			expected: "19034.65\n",
		},
		{
			name: "uranium238_age(0.5) = 4.468e9 years",
			code: `{{FUNC}}
print(round(uranium238_age(0.5), 0))`,
			expected: "4468000000.0\n",
		},
	],
};
