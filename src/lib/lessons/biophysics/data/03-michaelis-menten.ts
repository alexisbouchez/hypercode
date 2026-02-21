import type { Lesson } from "../../types";

export const michaelisMenten: Lesson = {
  id: "michaelis-menten",
  title: "Michaelis-Menten Kinetics",
  chapterId: "cell-kinetics",
  content: `# Michaelis-Menten Kinetics

## Enzyme Catalysis

Enzymes are biological catalysts that accelerate reactions by binding substrates and lowering the activation energy. The **Michaelis-Menten model** describes the steady-state kinetics of a single-substrate enzyme reaction:

$$E + S \\rightleftharpoons ES \\rightarrow E + P$$

## The Michaelis-Menten Equation

$$v = \\frac{V_{\\max} [S]}{K_m + [S]}$$

- **v** = reaction velocity (e.g., mM/s)
- **V_max** = maximum velocity (when enzyme is saturated)
- **[S]** = substrate concentration
- **K_m** = Michaelis constant (substrate concentration at v = V_max/2)

**Key insight:** When [S] = K_m, then v = V_max/2. A lower K_m means higher affinity for substrate.

## Catalytic Parameters

**Turnover number (k_cat):** reactions per enzyme per second
$$k_{\\text{cat}} = \\frac{V_{\\max}}{[E]_{\\text{total}}}$$

**Catalytic efficiency:** how well the enzyme works at low [S]
$$\\frac{k_{\\text{cat}}}{K_m} \\quad (\\text{units: M}^{-1}\\text{s}^{-1})$$

Diffusion-limited enzymes approach ~10⁹ M⁻¹s⁻¹ (the "catalytic perfection" limit).

## Lineweaver-Burk Plot

The double-reciprocal form linearizes the data:

$$\\frac{1}{v} = \\frac{K_m}{V_{\\max}} \\cdot \\frac{1}{[S]} + \\frac{1}{V_{\\max}}$$

x-intercept = −1/K_m, y-intercept = 1/V_max, slope = K_m/V_max.

## Competitive Inhibition

A competitive inhibitor (I) competes with substrate for the active site, effectively raising the apparent K_m:

$$v = \\frac{V_{\\max}[S]}{K_m\\left(1 + \\frac{[I]}{K_i}\\right) + [S]}$$

V_max is unchanged; K_m_app = K_m(1 + [I]/K_i).

## Your Task

Implement three functions:

1. **\`mm_velocity(S_mM, V_max_mM_s, K_m_mM)\`** — Michaelis-Menten reaction velocity
2. **\`catalytic_efficiency(k_cat_s, K_m_mM)\`** — k_cat/K_m in M⁻¹s⁻¹ (convert K_m from mM to M)
3. **\`mm_competitive_inhibition(S_mM, V_max_mM_s, K_m_mM, I_mM, K_i_mM)\`** — velocity with competitive inhibitor
`,
  starterCode: `def mm_velocity(S_mM, V_max_mM_s, K_m_mM):
    # v = V_max * [S] / (K_m + [S])
    pass

def catalytic_efficiency(k_cat_s, K_m_mM):
    # k_cat / K_m, with K_m converted from mM to M
    # Returns M^-1 s^-1
    pass

def mm_competitive_inhibition(S_mM, V_max_mM_s, K_m_mM, I_mM, K_i_mM):
    # K_m_app = K_m * (1 + I / K_i)
    # v = V_max * S / (K_m_app + S)
    pass
`,
  solution: `def mm_velocity(S_mM, V_max_mM_s, K_m_mM):
    return V_max_mM_s * S_mM / (K_m_mM + S_mM)

def catalytic_efficiency(k_cat_s, K_m_mM):
    K_m_M = K_m_mM * 1e-3
    return k_cat_s / K_m_M

def mm_competitive_inhibition(S_mM, V_max_mM_s, K_m_mM, I_mM, K_i_mM):
    K_m_app = K_m_mM * (1 + I_mM / K_i_mM)
    return V_max_mM_s * S_mM / (K_m_app + S_mM)
`,
  tests: [
    {
      name: "MM velocity at [S]=10 mM, V_max=100, K_m=5 mM",
      expected: "66.6667\n",
      code: `{{FUNC}}
print(f"{mm_velocity(10, 100, 5):.4f}")`,
    },
    {
      name: "MM velocity at [S]=K_m (should be V_max/2 = 50)",
      expected: "50.0000\n",
      code: `{{FUNC}}
print(f"{mm_velocity(5, 100, 5):.4f}")`,
    },
    {
      name: "Catalytic efficiency: k_cat=1000/s, K_m=0.1 mM",
      expected: "1.0000e+07\n",
      code: `{{FUNC}}
print(f"{catalytic_efficiency(1000, 0.1):.4e}")`,
    },
    {
      name: "Competitive inhibition: [I]=5 mM, K_i=5 mM (doubles K_m)",
      expected: "50.0000\n",
      code: `{{FUNC}}
print(f"{mm_competitive_inhibition(10, 100, 5, 5, 5):.4f}")`,
    },
  ],
};
