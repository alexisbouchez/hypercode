import type { Lesson } from "../../types";

export const eddingtonLuminosity: Lesson = {
	id: "eddington-luminosity",
	title: "Eddington Luminosity",
	chapterId: "stellar-physics",
	content: `## Eddington Luminosity

Every luminous object powered by accretion has a natural upper limit — the **Eddington luminosity**. Above this limit, the outward radiation pressure on the surrounding ionised hydrogen plasma exceeds the inward pull of gravity, and the accreting material is blown away.

### Derivation

For a fully ionised hydrogen plasma, radiation exerts pressure via **Thomson scattering** off free electrons. Balancing radiation pressure against gravity for a proton-electron pair at radius $r$:

$$\\frac{L \\kappa_{\\text{es}}}{4\\pi r^2 c} = \\frac{G M}{r^2}$$

Solving for $L$:

$$L_{\\text{Edd}} = \\frac{4\\pi G M c}{\\kappa_{\\text{es}}}$$

where:
- $G = 6.674 \\times 10^{-11}$ m³ kg⁻¹ s⁻²
- $c = 2.998 \\times 10^8$ m/s
- $\\kappa_{\\text{es}} = 0.020$ m² kg⁻¹ (electron scattering opacity for solar composition)

### Physical Significance

| Object | Mass | $L_{\\text{Edd}}$ |
|--------|------|-----------------|
| Sun    | 1 $M_\\odot$ | ~65,000 $L_\\odot$ |
| 10 $M_\\odot$ star | 10 $M_\\odot$ | ~650,000 $L_\\odot$ |
| Supermassive BH | $10^8 M_\\odot$ | ~$6.5 \\times 10^{12}$ $L_\\odot$ |

The Eddington luminosity sets the maximum brightness of X-ray binaries, determines mass accretion rates onto black holes, and limits the growth of supermassive black holes in the early universe.

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`eddington_luminosity_W(M_kg)\` — returns $L_{\\text{Edd}}$ in watts
- \`eddington_luminosity_solar(M_solar)\` — returns $L_{\\text{Edd}} / L_\\odot$
- \`mass_from_eddington(L_W)\` — returns $M$ in kg given luminosity in watts

Use $G = 6.674 \\times 10^{-11}$, $c = 2.998 \\times 10^8$ m/s, $\\kappa_{\\text{es}} = 0.020$ m² kg⁻¹, $L_\\odot = 3.828 \\times 10^{26}$ W, $M_\\odot = 1.989 \\times 10^{30}$ kg.`,

	starterCode: `import math

def eddington_luminosity_W(M_kg):
    G = 6.674e-11
    c = 2.998e8
    kappa = 0.020
    # TODO: return 4 * pi * G * M_kg * c / kappa
    pass

def eddington_luminosity_solar(M_solar):
    G = 6.674e-11
    c = 2.998e8
    kappa = 0.020
    L_sun = 3.828e26
    M_sun = 1.989e30
    # TODO: convert M_solar to kg, compute L_Edd in W, divide by L_sun
    pass

def mass_from_eddington(L_W):
    G = 6.674e-11
    c = 2.998e8
    kappa = 0.020
    # TODO: return L_W * kappa / (4 * pi * G * c)
    pass

print(round(eddington_luminosity_solar(1.0), 0))
print(round(eddington_luminosity_solar(10.0), 0))
print(round(mass_from_eddington(eddington_luminosity_W(1.989e30)) / 1.989e30, 2))
print(round(eddington_luminosity_solar(1e8) / 1e12, 4))
`,

	solution: `import math

def eddington_luminosity_W(M_kg):
    G = 6.674e-11
    c = 2.998e8
    kappa = 0.020
    return 4 * math.pi * G * M_kg * c / kappa

def eddington_luminosity_solar(M_solar):
    G = 6.674e-11
    c = 2.998e8
    kappa = 0.020
    L_sun = 3.828e26
    M_sun = 1.989e30
    M_kg = M_solar * M_sun
    return 4 * math.pi * G * M_kg * c / kappa / L_sun

def mass_from_eddington(L_W):
    G = 6.674e-11
    c = 2.998e8
    kappa = 0.020
    return L_W * kappa / (4 * math.pi * G * c)

print(round(eddington_luminosity_solar(1.0), 0))
print(round(eddington_luminosity_solar(10.0), 0))
print(round(mass_from_eddington(eddington_luminosity_W(1.989e30)) / 1.989e30, 2))
print(round(eddington_luminosity_solar(1e8) / 1e12, 4))
`,

	tests: [
		{
			name: "eddington_luminosity_solar(1.0) ≈ 65322 L_sun (one solar mass)",
			code: `{{FUNC}}
print(round(eddington_luminosity_solar(1.0), 0))`,
			expected: "65322.0\n",
		},
		{
			name: "eddington_luminosity_solar(10.0) ≈ 653222 L_sun (ten solar masses)",
			code: `{{FUNC}}
print(round(eddington_luminosity_solar(10.0), 0))`,
			expected: "653222.0\n",
		},
		{
			name: "mass_from_eddington roundtrip: recover 1 M_sun from its Eddington luminosity",
			code: `{{FUNC}}
print(round(mass_from_eddington(eddington_luminosity_W(1.989e30)) / 1.989e30, 2))`,
			expected: "1.0\n",
		},
		{
			name: "eddington_luminosity_solar(1e8) ≈ 6.5322×10¹² L_sun (supermassive black hole)",
			code: `{{FUNC}}
print(round(eddington_luminosity_solar(1e8) / 1e12, 4))`,
			expected: "6.5322\n",
		},
	],
};
