import type { Lesson } from "../../types";

export const neutrinoTemperature: Lesson = {
	id: "neutrino-temperature",
	title: "Neutrino Temperature",
	chapterId: "dark-sector",
	content: `## Neutrino Temperature

The universe is filled not just with the Cosmic Microwave Background (CMB) photons, but also with a **Cosmic Neutrino Background** (CνB). These relic neutrinos decoupled from the thermal bath when the universe was about 1 second old.

### Neutrino Decoupling

Neutrinos decoupled at $T \\approx 2$–$3$ MeV, just before electron-positron annihilation. When $e^+ e^-$ pairs annihilated (at $T \\approx 0.5$ MeV), they dumped entropy into the photon bath — but not into the already-decoupled neutrinos.

By conservation of entropy in the photon-electron plasma:

$$\\frac{T_\\nu}{T_\\gamma} = \\left(\\frac{4}{11}\\right)^{1/3} \\approx 0.7138$$

### Neutrino Temperature Today

The CMB temperature today is $T_\\gamma = 2.725$ K, so the cosmic neutrino background temperature is:

$$T_\\nu = T_\\gamma \\left(\\frac{4}{11}\\right)^{1/3} \\approx 1.945 \\text{ K}$$

These neutrinos have never been directly detected (they are extremely weakly interacting), but their gravitational effects are well-established.

### Effective Relativistic Degrees of Freedom

The total radiation energy density is written in terms of an effective number of degrees of freedom $g_{\\rm eff}$:

$$g_{\\rm eff} = 2 + \\frac{7}{8} \\cdot 2 \\cdot N_{\\rm eff} \\cdot \\left(\\frac{T_\\nu}{T_\\gamma}\\right)^4 = 2 + \\frac{7}{8} \\cdot 2 \\cdot N_{\\rm eff} \\cdot \\left(\\frac{4}{11}\\right)^{4/3}$$

The factor 2 comes from photon polarisations. Each neutrino family contributes $\\frac{7}{8} \\times 2$ (fermion factor × particle + antiparticle). For the standard 3 neutrino families: $g_{\\rm eff} \\approx 3.36$.

### Your Task

Implement three functions. All constants must be defined **inside** each function.

- \`neutrino_to_photon_temp_ratio()\` — returns $(4/11)^{1/3}$
- \`neutrino_temperature_K(T_cmb_K)\` — returns $T_\\nu = T_\\gamma \\cdot (4/11)^{1/3}$
- \`effective_relativistic_dof(N_eff)\` — returns $g_{\\rm eff} = 2 + \\frac{7}{8} \\cdot 2 \\cdot N_{\\rm eff} \\cdot (4/11)^{4/3}$`,

	starterCode: `import math

def neutrino_to_photon_temp_ratio():
    # TODO: return (4/11)^(1/3)
    pass

def neutrino_temperature_K(T_cmb_K):
    # TODO: return T_cmb_K * (4/11)^(1/3)
    pass

def effective_relativistic_dof(N_eff):
    # TODO: return 2 + (7/8) * 2 * N_eff * (4/11)^(4/3)
    pass

print(round(neutrino_to_photon_temp_ratio(), 6))
print(round(neutrino_temperature_K(2.725), 4))
print(round(effective_relativistic_dof(3), 6))
print(round(effective_relativistic_dof(0), 4))
`,

	solution: `import math

def neutrino_to_photon_temp_ratio():
    return (4/11) ** (1/3)

def neutrino_temperature_K(T_cmb_K):
    return T_cmb_K * (4/11)**(1/3)

def effective_relativistic_dof(N_eff):
    return 2 + (7/8) * 2 * N_eff * (4/11)**(4/3)

print(round(neutrino_to_photon_temp_ratio(), 6))
print(round(neutrino_temperature_K(2.725), 4))
print(round(effective_relativistic_dof(3), 6))
print(round(effective_relativistic_dof(0), 4))
`,

	tests: [
		{
			name: "neutrino_to_photon_temp_ratio() ≈ 0.713766",
			code: `{{FUNC}}
print(round(neutrino_to_photon_temp_ratio(), 6))`,
			expected: "0.713766\n",
		},
		{
			name: "neutrino_temperature_K(2.725) ≈ 1.945 K",
			code: `{{FUNC}}
print(round(neutrino_temperature_K(2.725), 4))`,
			expected: "1.945\n",
		},
		{
			name: "effective_relativistic_dof(3) ≈ 3.362644 (standard model N_eff=3)",
			code: `{{FUNC}}
print(round(effective_relativistic_dof(3), 6))`,
			expected: "3.362644\n",
		},
		{
			name: "effective_relativistic_dof(0) = 2.0 (photons only)",
			code: `{{FUNC}}
print(round(effective_relativistic_dof(0), 4))`,
			expected: "2.0\n",
		},
	],
};
