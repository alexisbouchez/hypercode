import type { Lesson } from "../../types";

export const bremsstrahlung: Lesson = {
  id: "bremsstrahlung",
  title: "Bremsstrahlung Radiation",
  chapterId: "mhd",
  content: `## Bremsstrahlung Radiation

**Bremsstrahlung** (German for "braking radiation") is electromagnetic radiation emitted when a charged particle — typically an electron — is accelerated (or decelerated) by the Coulomb field of another charged particle, usually an ion.

### Physics of Bremsstrahlung

In a plasma, electrons constantly encounter ions and are deflected by their electric fields. Each deflection causes the electron to radiate. This is also called **free-free emission** because the electron is unbound before and after the encounter.

### Power Emitted Per Unit Volume

The total bremsstrahlung power radiated per unit volume is:

$$P_{brem} = C_{brem} \\cdot n_e^2 \\cdot Z^2 \\cdot \\sqrt{T}$$

where:
- \\(C_{brem} = 1.69 \\times 10^{-38}\\) W m³ K\\(^{-1/2}\\)
- \\(n_e\\) is the electron number density (m\\(^{-3}\\))
- \\(Z\\) is the ion charge number
- \\(T\\) is the plasma temperature (K)

The \\(\\sqrt{T}\\) dependence arises because higher temperatures mean higher electron velocities, but shorter interaction times — the net effect scales as \\(v \\propto T^{1/2}\\).

### Cooling Time

The energy density of the plasma is \\(U = \\frac{3}{2} n k_B T\\). The **radiative cooling time** is:

$$t_{cool} = \\frac{\\frac{3}{2} n k_B T}{P_{brem}}$$

This tells us how long it takes for bremsstrahlung to radiate away the plasma's thermal energy.

### Significance for Fusion

Bremsstrahlung is a major energy loss mechanism in fusion plasmas. For a reactor to achieve **ignition**, the fusion power must exceed bremsstrahlung losses. This sets a minimum temperature requirement for the fuel.

For a deuterium-tritium plasma, the crossover occurs near 5 keV (\\(\\sim 58\\) million K), which is why fusion reactors must operate at extremely high temperatures.
`,
  starterCode: `import math

def bremsstrahlung_power_W_m3(n_e_m3, T_K, Z=1):
    # Return bremsstrahlung power in W/m³
    # C_brem = 1.69e-38 W m³ K^(-1/2)
    pass

def cooling_time_s(n_e_m3, T_K, Z=1):
    # Return radiative cooling time in seconds
    # k_B = 1.380649e-23 J/K
    pass`,
  solution: `import math

def bremsstrahlung_power_W_m3(n_e_m3, T_K, Z=1):
    C_brem = 1.69e-38
    return C_brem * n_e_m3**2 * Z**2 * math.sqrt(T_K)

def cooling_time_s(n_e_m3, T_K, Z=1):
    k_B = 1.380649e-23
    P = bremsstrahlung_power_W_m3(n_e_m3, T_K, Z)
    return (1.5 * n_e_m3 * k_B * T_K) / P`,
  tests: [
    {
      name: "Bremsstrahlung power at n=1e20 m⁻³, T=10 MK",
      expected: "5.3442e+05\n",
      code: `{{FUNC}}
print(f"{bremsstrahlung_power_W_m3(1e20, 1e7):.4e}")`,
    },
    {
      name: "Bremsstrahlung power at n=1e20 m⁻³, T=100 MK",
      expected: "1.6900e+06\n",
      code: `{{FUNC}}
print(f"{bremsstrahlung_power_W_m3(1e20, 1e8):.4e}")`,
    },
    {
      name: "Cooling time at n=1e20 m⁻³, T=10 MK",
      expected: "3.8751e-02\n",
      code: `{{FUNC}}
print(f"{cooling_time_s(1e20, 1e7):.4e}")`,
    },
    {
      name: "Cooling time at n=1e19 m⁻³, T=100 MK",
      expected: "1.2254e+00\n",
      code: `{{FUNC}}
print(f"{cooling_time_s(1e19, 1e8):.4e}")`,
    },
  ],
};
