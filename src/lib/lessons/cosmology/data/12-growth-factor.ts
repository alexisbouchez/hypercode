import type { Lesson } from "../../types";

export const growthFactor: Lesson = {
	id: "growth-factor",
	title: "Structure Growth Factor",
	chapterId: "structure",
	content: `## Structure Growth Factor

The large-scale structure we see today — galaxies, clusters, filaments, voids — grew from tiny quantum fluctuations seeded during inflation. Understanding how these perturbations grow is central to modern cosmology.

### Linear Growth in Matter Domination

In the matter-dominated era, the density contrast $\\delta = \\delta\\rho / \\rho$ grows as the scale factor:

$$D(a) = a$$

This is the **growing mode** of the linear perturbation equation. It tells us that a perturbation that was $\\delta = 10^{-5}$ at $a = 10^{-3}$ (redshift $z = 999$) grows to $\\delta = 0.1$ today ($a = 1$).

### Growth From Initial Conditions

For matter domination, the density contrast scales as:

$$\\delta(a) = \\delta_0 \\cdot \\frac{a}{a_0} = \\delta_0 \\cdot \\frac{a_{\\rm final}}{a_{\\rm initial}}$$

### The Growth Rate

The growth rate $f = d\\ln D / d\\ln a$ quantifies how fast structure grows. A useful approximation (Linder 2005) is:

$$f \\approx \\Omega_m(a)^{0.55}$$

For $\\Omega_m = 0.3$ (today's value in ΛCDM), $f \\approx 0.52$, meaning structure grows at about 52% of the Hubble rate.

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`growth_factor_matter_dom(a)\` — returns $D(a) = a$ (growing mode, normalized to 1 at $a=1$)
- \`delta_growth(delta0, a_initial, a_final)\` — returns $\\delta_0 \\cdot a_{\\rm final} / a_{\\rm initial}$
- \`growth_rate_approx(Omega_m_at_a)\` — returns $\\Omega_m^{0.55}$ (Linder approximation)`,

	starterCode: `import math

def growth_factor_matter_dom(a):
    # TODO: return the growing mode D(a) = a
    pass

def delta_growth(delta0, a_initial, a_final):
    # TODO: return delta0 * (a_final / a_initial)
    pass

def growth_rate_approx(Omega_m_at_a):
    # TODO: return Omega_m^0.55
    pass

print(growth_factor_matter_dom(0.5))
print(round(delta_growth(1e-4, 1e-3, 1.0), 6))
print(round(growth_rate_approx(0.3), 6))
print(round(delta_growth(1e-5, 1e-3, 0.5), 8))
`,

	solution: `import math

def growth_factor_matter_dom(a):
    return a

def delta_growth(delta0, a_initial, a_final):
    return delta0 * (a_final / a_initial)

def growth_rate_approx(Omega_m_at_a):
    return Omega_m_at_a ** 0.55

print(growth_factor_matter_dom(0.5))
print(round(delta_growth(1e-4, 1e-3, 1.0), 6))
print(round(growth_rate_approx(0.3), 6))
print(round(delta_growth(1e-5, 1e-3, 0.5), 8))
`,

	tests: [
		{
			name: "growth_factor_matter_dom(0.5) = 0.5",
			code: `{{FUNC}}
print(growth_factor_matter_dom(0.5))`,
			expected: "0.5\n",
		},
		{
			name: "delta_growth(1e-4, 1e-3, 1.0) = 0.1 (factor 1000 growth from z=999)",
			code: `{{FUNC}}
print(round(delta_growth(1e-4, 1e-3, 1.0), 6))`,
			expected: "0.1\n",
		},
		{
			name: "growth_rate_approx(0.3) ≈ 0.515723 (Linder approximation for ΛCDM)",
			code: `{{FUNC}}
print(round(growth_rate_approx(0.3), 6))`,
			expected: "0.515723\n",
		},
		{
			name: "delta_growth(1e-5, 1e-3, 0.5) = 0.005 (half-way growth)",
			code: `{{FUNC}}
print(round(delta_growth(1e-5, 1e-3, 0.5), 8))`,
			expected: "0.005\n",
		},
	],
};
