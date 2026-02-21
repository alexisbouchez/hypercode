import type { Lesson } from "../../types";

export const naturalUnits: Lesson = {
	id: "natural-units",
	title: "Natural Units",
	chapterId: "kinematics",
	content: `## Natural Units

Particle physicists use a system of **natural units** where the speed of light $c$ and the reduced Planck constant $\\hbar$ are both set to 1. This eliminates factors of $c$ and $\\hbar$ from every equation and keeps numbers at a human scale.

### Key Conversion Constants

The bridge between natural units and SI is the product $\\hbar c$:

$$\\hbar c = 197.3269804 \\text{ MeV·fm}$$

where 1 fm (femtometre) $= 10^{-15}$ m is the typical scale of a nucleus.

### Energy

The electron-volt (eV) and its multiples are the natural currency of particle physics:

$$1 \\text{ MeV} = 10^6 \\text{ eV} = 1.602 \\times 10^{-13} \\text{ J}$$
$$1 \\text{ GeV} = 10^3 \\text{ MeV} = 1.602 \\times 10^{-10} \\text{ J}$$

### Mass

Using $E = mc^2$, mass is quoted in GeV/$c^2$. To convert to kg:

$$m_{\\text{kg}} = m_{\\text{GeV}} \\cdot \\frac{1.602 \\times 10^{-10}}{c^2}$$

The proton has mass $m_p \\approx 0.938272$ GeV/$c^2 \\approx 1.673 \\times 10^{-27}$ kg.

### Your Task

Implement three unit-conversion helpers. All physical constants must be defined **inside** each function body.

- \`MeV_to_joules(E_MeV)\` — converts MeV to Joules using $1\\text{ MeV} = 1.602 \\times 10^{-13}$ J
- \`GeV_to_kg(m_GeV)\` — converts GeV/$c^2$ to kg using $c = 299792458$ m/s
- \`hbar_c_MeV_fm()\` — returns the constant $\\hbar c = 197.3269804$ MeV·fm`,

	starterCode: `import math

def MeV_to_joules(E_MeV):
    # 1 MeV = 1.602e-13 J
    pass

def GeV_to_kg(m_GeV):
    # 1 GeV = 1.602e-10 J, c = 299792458.0 m/s
    pass

def hbar_c_MeV_fm():
    # return hbar*c in MeV·fm (no parameters)
    pass

print(MeV_to_joules(1.0))
print(MeV_to_joules(938.272))
print(round(GeV_to_kg(0.938272), 31))
print(hbar_c_MeV_fm())
`,

	solution: `import math

def MeV_to_joules(E_MeV):
    MeV_to_J = 1.602e-13
    return E_MeV * MeV_to_J

def GeV_to_kg(m_GeV):
    GeV_to_J = 1.602e-10
    c = 299792458.0
    return m_GeV * GeV_to_J / (c**2)

def hbar_c_MeV_fm():
    return 197.3269804

print(MeV_to_joules(1.0))
print(MeV_to_joules(938.272))
print(round(GeV_to_kg(0.938272), 31))
print(hbar_c_MeV_fm())
`,

	tests: [
		{
			name: "MeV_to_joules(1.0) = 1.602e-13 J",
			code: `{{FUNC}}
print(MeV_to_joules(1.0))`,
			expected: "1.602e-13\n",
		},
		{
			name: "MeV_to_joules(938.272) — proton rest energy in Joules",
			code: `{{FUNC}}
print(MeV_to_joules(938.272))`,
			expected: "1.503111744e-10\n",
		},
		{
			name: "GeV_to_kg(0.938272) — proton mass ≈ 1.6724e-27 kg",
			code: `{{FUNC}}
print(round(GeV_to_kg(0.938272), 31))`,
			expected: "1.6724e-27\n",
		},
		{
			name: "hbar_c_MeV_fm() = 197.3269804 MeV·fm",
			code: `{{FUNC}}
print(hbar_c_MeV_fm())`,
			expected: "197.3269804\n",
		},
	],
};
