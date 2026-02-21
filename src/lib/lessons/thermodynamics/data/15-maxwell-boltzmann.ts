import type { Lesson } from "../../types";

export const maxwellBoltzmann: Lesson = {
  id: "maxwell-boltzmann",
  title: "Maxwell-Boltzmann Speed Distribution",
  chapterId: "statistical",
  content: `## Maxwell-Boltzmann Speed Distribution

In an ideal gas at temperature $T$, molecules move with a wide range of speeds. The Maxwell-Boltzmann distribution describes how those speeds are distributed. Rather than all molecules moving at the same speed, the distribution is a broad bell-shaped curve with a characteristic spread determined by $T$ and the molecular mass $m$.

Three characteristic speeds summarise the distribution:

### Most Probable Speed

The speed at the peak of the distribution — the speed most molecules travel near:

$$v_{\\text{mp}} = \\sqrt{\\frac{2k_B T}{m}} = \\sqrt{\\frac{2RT}{M}}$$

### Mean Speed

The arithmetic average speed:

$$\\bar{v} = \\sqrt{\\frac{8k_B T}{\\pi m}} = \\sqrt{\\frac{8RT}{\\pi M}}$$

### Root-Mean-Square Speed

The square root of the average of the squared speeds. This is directly related to the average kinetic energy $\\langle KE \\rangle = \\tfrac{3}{2} k_B T$:

$$v_{\\text{rms}} = \\sqrt{\\frac{3k_B T}{m}} = \\sqrt{\\frac{3RT}{M}}$$

Here $R = 8.314\\,\\text{J/(mol·K)}$ is the gas constant and $M$ is the molar mass in **kg/mol**.

### Speed Ordering

The three speeds always satisfy:

$$v_{\\text{mp}} < \\bar{v} < v_{\\text{rms}}$$

This ordering reflects the asymmetry of the distribution: the high-speed tail pulls the mean and RMS above the peak.

### Example Molar Masses

| Gas | $M$ (kg/mol) |
|-----|-------------|
| H₂  | 0.002016    |
| N₂  | 0.028014    |
| O₂  | 0.032000    |

Higher molar mass → slower speeds at the same temperature.
`,
  starterCode: `import math

R = 8.314

def v_most_probable(M, T):
    # Return the most probable speed in m/s
    # M is molar mass in kg/mol, T is temperature in K
    pass

def v_mean(M, T):
    # Return the mean speed in m/s
    pass

def v_rms(M, T):
    # Return the root-mean-square speed in m/s
    pass
`,
  solution: `import math

R = 8.314

def v_most_probable(M, T):
    R = 8.314
    return math.sqrt(2 * R * T / M)

def v_mean(M, T):
    R = 8.314
    return math.sqrt(8 * R * T / (math.pi * M))

def v_rms(M, T):
    R = 8.314
    return math.sqrt(3 * R * T / M)
`,
  tests: [
    {
      name: "N₂ at 300K: most probable speed ≈ 422.0 m/s",
      code: `import math
{{FUNC}}
print(round(v_most_probable(0.028014, 300), 1))`,
      expected: "422.0\n",
    },
    {
      name: "N₂ at 300K: mean speed ≈ 476.2 m/s",
      code: `import math
{{FUNC}}
print(round(v_mean(0.028014, 300), 1))`,
      expected: "476.2\n",
    },
    {
      name: "N₂ at 300K: rms speed ≈ 516.8 m/s",
      code: `import math
{{FUNC}}
print(round(v_rms(0.028014, 300), 1))`,
      expected: "516.8\n",
    },
    {
      name: "Speed ordering: v_mp < v_mean < v_rms",
      code: `import math
{{FUNC}}
print(v_most_probable(0.028014, 300) < v_mean(0.028014, 300) < v_rms(0.028014, 300))`,
      expected: "True\n",
    },
  ],
};
