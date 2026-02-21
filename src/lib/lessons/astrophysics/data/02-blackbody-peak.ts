import type { Lesson } from "../../types";

export const blackbodyPeak: Lesson = {
	id: "blackbody-peak",
	title: "Blackbody Peak & Wien's Law",
	chapterId: "stellar-physics",
	content: `## Blackbody Peak & Wien's Law

Stars emit radiation with a spectrum that closely follows a **blackbody**. The wavelength at which the spectrum peaks is inversely proportional to the surface temperature — this is **Wien's displacement law**:

$$\\lambda_{\\max} = \\frac{b}{T}$$

where $b = 2.898 \\times 10^{-3}$ m·K is Wien's displacement constant.

### Stellar Colors

| Star type | $T$ (K) | $\\lambda_{\\max}$ | Color |
|-----------|---------|-----------------|-------|
| O-type    | 30,000  | ~97 nm (UV)     | Blue  |
| Sun (G2)  | 5,778   | ~502 nm         | Yellow-white |
| Red dwarf | 3,000   | ~966 nm (near-IR) | Red |

The Sun peaks in green light — but it also emits strongly across the entire visible spectrum, which is why sunlight appears white.

### Luminosity Ratio

For two stars of the same radius at temperatures $T_1$ and $T_2$, the Stefan-Boltzmann law gives:

$$\\frac{L_1}{L_2} = \\left(\\frac{T_1}{T_2}\\right)^4$$

A star at 30,000 K is over 700 times more luminous than a star of equal size at 5,778 K.

### Inverse: Temperature from Peak

Given an observed peak wavelength, you can recover the temperature:

$$T = \\frac{b}{\\lambda_{\\max}}$$

This technique — **spectrophotometry** — is used to determine stellar temperatures from observed spectra.

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`peak_wavelength_nm(T_K)\` — returns $\\lambda_{\\max}$ in nanometres
- \`stellar_color_ratio(T1_K, T2_K)\` — returns $L_1/L_2 = (T_1/T_2)^4$ for equal-radius stars
- \`temperature_from_peak(lambda_nm)\` — returns $T$ in Kelvin from peak wavelength in nm

Use $b = 2.898 \\times 10^{-3}$ m·K.`,

	starterCode: `import math

def peak_wavelength_nm(T_K):
    b = 2.898e-3  # m·K
    # TODO: return b / T_K in nanometres (multiply by 1e9)
    pass

def stellar_color_ratio(T1_K, T2_K):
    # TODO: return (T1_K / T2_K)^4
    pass

def temperature_from_peak(lambda_nm):
    b = 2.898e-3  # m·K
    # TODO: convert lambda_nm to metres, then return b / lambda_m
    pass

print(round(peak_wavelength_nm(5778), 1))
print(round(peak_wavelength_nm(3000), 1))
print(round(stellar_color_ratio(30000, 5778), 4))
print(round(temperature_from_peak(501.6), 0))
`,

	solution: `import math

def peak_wavelength_nm(T_K):
    b = 2.898e-3  # m·K
    return b / T_K * 1e9

def stellar_color_ratio(T1_K, T2_K):
    return (T1_K / T2_K)**4

def temperature_from_peak(lambda_nm):
    b = 2.898e-3  # m·K
    return b / (lambda_nm * 1e-9)

print(round(peak_wavelength_nm(5778), 1))
print(round(peak_wavelength_nm(3000), 1))
print(round(stellar_color_ratio(30000, 5778), 4))
print(round(temperature_from_peak(501.6), 0))
`,

	tests: [
		{
			name: "peak_wavelength_nm(5778) ≈ 501.6 nm (Sun peaks in green)",
			code: `{{FUNC}}
print(round(peak_wavelength_nm(5778), 1))`,
			expected: "501.6\n",
		},
		{
			name: "peak_wavelength_nm(3000) ≈ 966.0 nm (cool red dwarf, near-IR)",
			code: `{{FUNC}}
print(round(peak_wavelength_nm(3000), 1))`,
			expected: "966.0\n",
		},
		{
			name: "stellar_color_ratio(30000, 5778) ≈ 726.733 (O-star vs Sun, same radius)",
			code: `{{FUNC}}
print(round(stellar_color_ratio(30000, 5778), 4))`,
			expected: "726.733\n",
		},
		{
			name: "temperature_from_peak(501.6) ≈ 5778 K (Wien roundtrip for the Sun)",
			code: `{{FUNC}}
print(round(temperature_from_peak(501.6), 0))`,
			expected: "5778.0\n",
		},
	],
};
