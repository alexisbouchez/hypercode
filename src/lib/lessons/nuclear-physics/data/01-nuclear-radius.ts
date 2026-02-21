import type { Lesson } from "../../types";

export const nuclearRadius: Lesson = {
	id: "nuclear-radius",
	title: "Nuclear Radius",
	chapterId: "nuclear-structure",
	content: `## Nuclear Radius

Nuclei are extraordinarily small — on the order of femtometres (1 fm = 10⁻¹⁵ m). Despite this, a remarkably simple empirical formula describes their size.

### The Nuclear Radius Formula

The nuclear radius scales with the cube root of the mass number $A$ (the total number of protons and neutrons):

$$R = R_0 \\cdot A^{1/3}$$

where $R_0 \\approx 1.2 \\text{ fm} = 1.2 \\times 10^{-15}$ m is the nuclear radius constant.

### Nuclear Volume

Since $R \\propto A^{1/3}$, the nuclear volume $V = \\frac{4}{3}\\pi R^3$ scales **linearly** with $A$:

$$V = \\frac{4}{3}\\pi R_0^3 \\cdot A$$

### Nuclear Density

The nuclear density is strikingly **constant** for all nuclei:

$$\\rho = \\frac{A \\cdot m_u}{V} = \\frac{m_u}{\\frac{4}{3}\\pi R_0^3} \\approx 2.3 \\times 10^{17} \\text{ kg/m}^3$$

where $m_u = 1.66054 \\times 10^{-27}$ kg is the atomic mass unit. This density is roughly $10^{14}$ times denser than water — a teaspoon of nuclear matter would weigh about 500 million tonnes.

| Nucleus | $A$ | $R$ (fm) |
|---------|-----|----------|
| Proton  | 1   | 1.20     |
| He-4    | 4   | 1.90     |
| C-12    | 12  | 2.75     |
| U-238   | 238 | 7.44     |

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`nuclear_radius(A)\` — returns $R = R_0 \\cdot A^{1/3}$ in metres
- \`nuclear_volume(A)\` — returns $V = \\frac{4}{3}\\pi R^3$ in m³
- \`nuclear_density(A)\` — returns $\\rho = A \\cdot m_u / V$ in kg/m³

Use $R_0 = 1.2 \\times 10^{-15}$ m and $m_u = 1.66054 \\times 10^{-27}$ kg.`,

	starterCode: `import math

def nuclear_radius(A):
    R0 = 1.2e-15  # m
    # TODO: return R0 * A^(1/3)
    pass

def nuclear_volume(A):
    R0 = 1.2e-15  # m
    # TODO: return (4/3) * pi * R^3
    pass

def nuclear_density(A):
    u = 1.66054e-27  # kg
    R0 = 1.2e-15    # m
    # TODO: return A * u / V
    pass

print(round(nuclear_radius(1) * 1e15, 4))
print(round(nuclear_radius(12) * 1e15, 4))
print(round(nuclear_density(12) / 1e17, 4))
print(round(nuclear_radius(238) * 1e15, 4))
`,

	solution: `import math

def nuclear_radius(A):
    R0 = 1.2e-15  # m
    return R0 * A ** (1/3)

def nuclear_volume(A):
    R0 = 1.2e-15  # m
    R = R0 * A ** (1/3)
    return (4/3) * math.pi * R ** 3

def nuclear_density(A):
    u = 1.66054e-27  # kg
    R0 = 1.2e-15    # m
    R = R0 * A ** (1/3)
    V = (4/3) * math.pi * R ** 3
    return A * u / V

print(round(nuclear_radius(1) * 1e15, 4))
print(round(nuclear_radius(12) * 1e15, 4))
print(round(nuclear_density(12) / 1e17, 4))
print(round(nuclear_radius(238) * 1e15, 4))
`,

	tests: [
		{
			name: "nuclear_radius(1) = 1.2 fm (proton)",
			code: `{{FUNC}}
print(round(nuclear_radius(1) * 1e15, 4))`,
			expected: "1.2\n",
		},
		{
			name: "nuclear_radius(12) ≈ 2.7473 fm (C-12)",
			code: `{{FUNC}}
print(round(nuclear_radius(12) * 1e15, 4))`,
			expected: "2.7473\n",
		},
		{
			name: "nuclear_density(12) ≈ 2.2941e17 kg/m³ (constant for all nuclei)",
			code: `{{FUNC}}
print(round(nuclear_density(12) / 1e17, 4))`,
			expected: "2.2941\n",
		},
		{
			name: "nuclear_radius(238) ≈ 7.4366 fm (U-238)",
			code: `{{FUNC}}
print(round(nuclear_radius(238) * 1e15, 4))`,
			expected: "7.4366\n",
		},
	],
};
