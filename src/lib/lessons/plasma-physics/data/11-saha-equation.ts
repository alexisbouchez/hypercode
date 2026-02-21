import type { Lesson } from "../../types";

export const sahaEquation: Lesson = {
  id: "saha-equation",
  title: "Saha Equation",
  chapterId: "mhd",
  content: `# Saha Equation

The **Saha equation** describes the ionization equilibrium of a gas in thermal equilibrium. It is fundamental to understanding how plasmas form from neutral gases as temperature rises.

## Ionization Equilibrium

For hydrogen, the Saha equation relates the number densities of electrons \\(n_e\\), protons \\(n_p\\), and neutral atoms \\(n_H\\):

$$\\frac{n_e \\cdot n_p}{n_H} = \\frac{(2\\pi m_e k_B T)^{3/2}}{h^3} \\cdot 2 \\cdot e^{-\\chi / (k_B T)}$$

where:
- \\(m_e = 9.109 \\times 10^{-31}\\) kg is the electron mass
- \\(k_B = 1.381 \\times 10^{-23}\\) J/K is Boltzmann's constant
- \\(h = 6.626 \\times 10^{-34}\\) J·s is Planck's constant
- \\(\\chi = 13.6\\) eV \\(= 2.179 \\times 10^{-18}\\) J is the hydrogen ionization energy

The factor of 2 accounts for the two spin states of the electron.

## Ionization Fraction

For a plasma with total number density \\(n_{\\text{total}}\\), define the ionization fraction \\(x = n_e / n_{\\text{total}}\\). Since \\(n_e = n_p = x \\cdot n\\) and \\(n_H = (1-x) \\cdot n\\):

$$\\frac{x^2 n}{1 - x} = S(T)$$

where \\(S(T)\\) is the right-hand side of the Saha equation. This rearranges to the quadratic:

$$n x^2 + S x - S = 0$$

Solving with the quadratic formula (taking the positive root) gives the ionization fraction at any temperature and density.

## Physical Interpretation

- At low temperatures (\\(T \\ll 10{,}000\\) K for hydrogen), most atoms are neutral: \\(x \\approx 0\\)
- At temperatures above ~15,000 K, hydrogen becomes nearly fully ionized: \\(x \\approx 1\\)
- The transition is sharp due to the exponential dependence on temperature
`,
  starterCode: `import math

def saha_rhs(T_K):
    # Returns the RHS of the Saha equation in m^-3
    pass

def ionization_fraction(n_total_m3, T_K):
    # Returns x = n_e / n_total
    pass`,
  solution: `import math

def saha_rhs(T_K):
    m_e = 9.109e-31
    k_B = 1.381e-23
    h = 6.626e-34
    chi = 2.179e-18
    return (2 * math.pi * m_e * k_B * T_K / h**2)**(1.5) * 2 * math.exp(-chi / (k_B * T_K))

def ionization_fraction(n_total_m3, T_K):
    S = saha_rhs(T_K)
    a = n_total_m3
    b = S
    c = -S
    x = (-b + math.sqrt(b**2 - 4 * a * c)) / (2 * a)
    return x`,
  tests: [
    {
      name: "Saha RHS at 10,000 K",
      expected: "6.7852e+20\n",
      code: `{{FUNC}}
print(f"{saha_rhs(10000):.4e}")`,
    },
    {
      name: "Saha RHS at 20,000 K",
      expected: "5.1209e+24\n",
      code: `{{FUNC}}
print(f"{saha_rhs(20000):.4e}")`,
    },
    {
      name: "Ionization fraction at 10,000 K (partial ionization)",
      expected: "0.8847\n",
      code: `{{FUNC}}
print(f"{ionization_fraction(1e20, 10000):.4f}")`,
    },
    {
      name: "Ionization fraction at 20,000 K (nearly fully ionized)",
      expected: "1.0000\n",
      code: `{{FUNC}}
print(f"{ionization_fraction(1e20, 20000):.4f}")`,
    },
  ],
};
