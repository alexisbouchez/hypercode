import type { Lesson } from "../../types";

export const crossSection: Lesson = {
	id: "cross-section",
	title: "Nuclear Cross Section",
	chapterId: "nuclear-reactions",
	content: `## Nuclear Cross Section

The **cross section** $\\sigma$ quantifies how likely a nuclear reaction is. Imagine the target nucleus presenting an effective area to an incoming particle — the larger the cross section, the more probable the reaction.

$$\\sigma \\quad [\\text{barn}], \\quad 1 \\text{ barn} = 10^{-28} \\text{ m}^2$$

The barn is a surprisingly large unit: it was named during the Manhattan Project because uranium nuclei seemed "as big as a barn."

### Key Quantities

**Reaction rate** per target nucleus:
$$R = \\Phi \\cdot \\sigma \\quad [\\text{reactions/s}]$$
where $\\Phi$ is the particle flux (particles/m²/s).

**Mean free path** — average distance a projectile travels before reacting:
$$\\lambda_{\\text{mfp}} = \\frac{1}{N \\sigma}$$
where $N$ is the number density of target nuclei [m⁻³].

**Attenuation** of a beam passing through material of thickness $x$:
$$I(x) = I_0 \\, e^{-N \\sigma x}$$

### Example: Thermal Neutrons in Water

Water has $N \\approx 3.34 \\times 10^{28}$ molecules/m³. The total neutron cross section for water near $\\sigma \\approx 49$ barn gives a mean free path of about 6 mm — thermal neutrons are moderated very efficiently.

### Your Task

Implement:
- \`mean_free_path(N_density, sigma_m2)\` — mean free path in meters
- \`attenuation(I0, N_density, sigma_m2, x)\` — transmitted intensity
- \`reaction_rate(flux, sigma_m2)\` — reaction rate per target nucleus`,

	starterCode: `import math

def mean_free_path(N_density, sigma_m2):
    # lambda = 1 / (N * sigma)
    pass

def attenuation(I0, N_density, sigma_m2, x):
    # I(x) = I0 * exp(-N * sigma * x)
    pass

def reaction_rate(flux, sigma_m2):
    # R = flux * sigma
    pass
`,

	solution: `import math

def mean_free_path(N_density, sigma_m2):
    return 1 / (N_density * sigma_m2)

def attenuation(I0, N_density, sigma_m2, x):
    return I0 * math.exp(-N_density * sigma_m2 * x)

def reaction_rate(flux, sigma_m2):
    return flux * sigma_m2
`,

	tests: [
		{
			name: "mean_free_path thermal neutrons in water ≈ 0.0061 m",
			code: `{{FUNC}}
print(round(mean_free_path(3.34e28, 49e-28), 4))`,
			expected: "0.0061\n",
		},
		{
			name: "attenuation through 1 cm of water ≈ 0.1946",
			code: `{{FUNC}}
print(round(attenuation(1.0, 3.34e28, 49e-28, 0.01), 4))`,
			expected: "0.1946\n",
		},
		{
			name: "reaction_rate with 1e18 n/m²/s flux and 100 barn cross section",
			code: `{{FUNC}}
print(reaction_rate(1e18, 100e-28))`,
			expected: "1e-08\n",
		},
		{
			name: "mean_free_path with small cross section = 100.0 m",
			code: `{{FUNC}}
print(round(mean_free_path(1e28, 1e-30), 4))`,
			expected: "100.0\n",
		},
	],
};
