import type { Lesson } from "../../types";

export const hillEquation: Lesson = {
  id: "hill-equation",
  title: "Hill Equation & Cooperative Binding",
  chapterId: "cell-kinetics",
  content: `# Hill Equation & Cooperative Binding

## Cooperative Binding

Many biological receptors and proteins do not bind ligands independently — binding at one site influences affinity at other sites. This is called **cooperative binding** and is described by the **Hill equation**.

## Fractional Saturation

The fraction of binding sites occupied (θ) at ligand concentration [L]:

$$\\theta = \\frac{[L]^n}{K_d^n + [L]^n}$$

Where:
- **[L]** — free ligand concentration
- **K_d** — dissociation constant (= EC50, the concentration at half-maximal binding)
- **n** — Hill coefficient

### Hill Coefficient Interpretation

| n | Cooperativity |
|---|---------------|
| n = 1 | No cooperativity (Michaelis-Menten) |
| n > 1 | Positive cooperativity (sigmoidal curve) |
| n < 1 | Negative cooperativity |

## EC50

At EC50, θ = 0.5. From the Hill equation: **EC50 = K_d**.

## Hill Plot Linearization

Taking the logit transform:

$$\\log\\left(\\frac{\\theta}{1-\\theta}\\right) = n \\cdot \\log([L]) - n \\cdot \\log(K_d)$$

A plot of log(θ/(1-θ)) vs log([L]) gives a straight line with slope = n.

## Oxygen Binding to Hemoglobin

Hemoglobin is a classic example of cooperative binding:
- **n ≈ 2.8** (4 subunits, strong cooperativity)
- **P50 ≈ 26 mmHg** (half-saturation at pO2 = 26 mmHg)
- Arterial blood (pO2 ≈ 100 mmHg): ~97% saturated
- Venous blood (pO2 ≈ 40 mmHg): ~75% saturated

This sigmoidal oxygen-dissociation curve enables efficient O2 loading in the lungs and release in tissues.

## Functions to Implement

- \`hill_saturation(L, K_d, n=1)\` — fractional saturation θ
- \`hill_plot_y(L, K_d, n)\` — Hill plot y-axis: log(θ/(1-θ))
- \`ec50(K_d)\` — returns K_d (EC50 equals K_d by definition)
- \`oxygen_saturation(pO2_mmHg, n=2.8, P50=26)\` — hemoglobin saturation
`,
  starterCode: `import math

def hill_saturation(L, K_d, n=1):
    pass

def hill_plot_y(L, K_d, n):
    pass

def ec50(K_d):
    pass

def oxygen_saturation(pO2_mmHg, n=2.8, P50=26):
    pass
`,
  solution: `import math

def hill_saturation(L, K_d, n=1):
    return L**n / (K_d**n + L**n)

def hill_plot_y(L, K_d, n):
    theta = hill_saturation(L, K_d, n)
    return math.log10(theta / (1 - theta))

def ec50(K_d):
    return K_d

def oxygen_saturation(pO2_mmHg, n=2.8, P50=26):
    return hill_saturation(pO2_mmHg, P50, n)
`,
  tests: [
    {
      name: "hill_saturation at EC50 equals 0.5",
      expected: "0.5000\n",
      code: `{{FUNC}}\nprint(f"{hill_saturation(26, 26, 2.8):.4f}")`,
    },
    {
      name: "hill_saturation at high ligand concentration",
      expected: "0.9775\n",
      code: `{{FUNC}}\nprint(f"{hill_saturation(100, 26, 2.8):.4f}")`,
    },
    {
      name: "oxygen_saturation at arterial pO2 (100 mmHg)",
      expected: "0.9775\n",
      code: `{{FUNC}}\nprint(f"{oxygen_saturation(100):.4f}")`,
    },
    {
      name: "oxygen_saturation at venous pO2 (40 mmHg)",
      expected: "0.7696\n",
      code: `{{FUNC}}\nprint(f"{oxygen_saturation(40):.4f}")`,
    },
  ],
};
