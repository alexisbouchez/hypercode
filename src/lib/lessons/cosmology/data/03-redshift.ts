import type { Lesson } from "../../types";

export const redshiftScaleFactor: Lesson = {
	id: "redshift-scale-factor",
	title: "Redshift and the Scale Factor",
	chapterId: "expansion",
	content: `## Redshift and the Scale Factor

As the universe expands, photons travelling through space are stretched to longer wavelengths — a phenomenon called **cosmological redshift**. This is distinct from Doppler redshift; it is the fabric of space itself that is expanding.

### The Scale Factor

The **scale factor** $a(t)$ describes the relative size of the universe at time $t$, normalised so that $a_0 = 1$ today. When a photon was emitted at time $t_e$, the scale factor was $a_e < 1$.

### Redshift Definition

The cosmological redshift $z$ is defined by:

$$1 + z = \\frac{\\lambda_{\\text{obs}}}{\\lambda_{\\text{emit}}} = \\frac{a_0}{a_e} = \\frac{1}{a_e}$$

So:
$$z = \\frac{1}{a} - 1 \\qquad \\text{and} \\qquad a = \\frac{1}{1+z}$$

### Key Redshifts in Cosmology

| Event | Redshift $z$ | Scale factor $a$ |
|-------|-------------|------------------|
| Today | 0 | 1.000 |
| Dark energy domination begins | ~0.3 | ~0.77 |
| Matter-radiation equality | ~3400 | ~0.00029 |
| Recombination (CMB) | ~1089 | ~0.00092 |
| Big Bang | ∞ | 0 |

### CMB Temperature

The CMB photons were emitted at recombination ($z \\approx 1089$) when the universe was opaque and hot. As space expanded by a factor of $1+z$, the photon wavelengths stretched by the same factor, cooling the radiation:

$$T(z) = T_0 \\cdot (1+z)$$

where $T_0 = 2.725$ K is the CMB temperature today. At recombination, the temperature was $T \\approx 2970$ K.

### Your Task

Implement the following functions. The constant $T_0$ must be defined **inside** \`cmb_temperature_at_z\`.

- \`redshift_from_scale(a)\` — returns $z = 1/a - 1$
- \`scale_from_redshift(z)\` — returns $a = 1/(1+z)$
- \`cmb_temperature_at_z(z)\` — returns $T(z) = T_0(1+z)$ in Kelvin, with $T_0 = 2.725$ K`,

	starterCode: `def redshift_from_scale(a):
    # z = 1/a - 1
    pass

def scale_from_redshift(z):
    # a = 1/(1+z)
    pass

def cmb_temperature_at_z(z):
    T0 = 2.725  # K, CMB temperature today
    # return T0 * (1+z)
    pass

print(redshift_from_scale(0.5))
print(scale_from_redshift(1))
print(round(cmb_temperature_at_z(1089), 3))
`,

	solution: `def redshift_from_scale(a):
    return 1/a - 1

def scale_from_redshift(z):
    return 1/(1+z)

def cmb_temperature_at_z(z):
    T0 = 2.725  # K
    return T0 * (1+z)

print(redshift_from_scale(0.5))
print(scale_from_redshift(1))
print(round(cmb_temperature_at_z(1089), 3))
`,

	tests: [
		{
			name: "redshift_from_scale(0.5) = 1.0 (universe half its current size)",
			code: `{{FUNC}}
print(redshift_from_scale(0.5))`,
			expected: "1.0\n",
		},
		{
			name: "scale_from_redshift(1) = 0.5 (z=1 corresponds to a=0.5)",
			code: `{{FUNC}}
print(scale_from_redshift(1))`,
			expected: "0.5\n",
		},
		{
			name: "cmb_temperature_at_z(1089) ≈ 2970.25 K (recombination temperature)",
			code: `{{FUNC}}
print(round(cmb_temperature_at_z(1089), 3))`,
			expected: "2970.25\n",
		},
		{
			name: "scale_from_redshift(0) = 1.0 (today)",
			code: `{{FUNC}}
print(scale_from_redshift(0))`,
			expected: "1.0\n",
		},
	],
};
