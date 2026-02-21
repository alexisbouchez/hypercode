import type { Lesson } from "../../types";

export const bifurcation: Lesson = {
  id: "bifurcation",
  title: "Bifurcation Theory",
  chapterId: "dynamical-systems",
  content: `# Bifurcation Theory

A **bifurcation** occurs when a small change in a parameter causes a qualitative change in a dynamical system's behavior — new fixed points appear, disappear, or change stability.

## Saddle-Node Bifurcation

The normal form is:
$$\\frac{dx}{dt} = \\mu + x^2$$

Fixed points satisfy $\\mu + x^{*2} = 0 \\Rightarrow x^* = \\pm\\sqrt{-\\mu}$:
- $\\mu < 0$: two fixed points (one stable, one unstable)
- $\\mu = 0$: one fixed point (bifurcation point)
- $\\mu > 0$: no real fixed points — they annihilate

## Transcritical Bifurcation

$$\\frac{dx}{dt} = \\mu x - x^2$$

Fixed points: $x^* = 0$ and $x^* = \\mu$ (always two). They **exchange stability** at $\\mu = 0$.

## Pitchfork Bifurcation (Supercritical)

$$\\frac{dx}{dt} = \\mu x - x^3$$

Fixed points:
- $\\mu \\leq 0$: only $x^* = 0$ (stable)
- $\\mu > 0$: $x^* = 0$ (unstable) and $x^* = \\pm\\sqrt{\\mu}$ (stable)

The stable state splits into two — like a pitchfork — as $\\mu$ crosses zero.

## Period-Doubling in the Logistic Map

$$x_{n+1} = r x_n (1 - x_n)$$

As $r$ increases, the fixed point loses stability and period-2 cycles appear at $r = 3$ (first period-doubling bifurcation). This cascade continues to chaos at $r \\approx 3.569$.

## Implementation

\`\`\`python
import math

def saddle_node_fps(mu):
    # Return list of real fixed points: [sqrt(-mu), -sqrt(-mu)] if mu < 0
    # Return [0.0] if mu == 0, [] if mu > 0
    ...

def transcritical_fps(mu):
    # Always return [0.0, float(mu)]
    ...

def pitchfork_fps(mu):
    # Return [0.0] if mu <= 0
    # Return [0.0, sqrt(mu), -sqrt(mu)] if mu > 0
    ...

def logistic_bifurcation_r():
    # Return the first period-doubling bifurcation value
    ...
\`\`\`
`,
  starterCode: `import math

def saddle_node_fps(mu):
    pass

def transcritical_fps(mu):
    pass

def pitchfork_fps(mu):
    pass

def logistic_bifurcation_r():
    pass
`,
  solution: `import math

def saddle_node_fps(mu):
    if mu < 0:
        val = math.sqrt(-mu)
        return [val, -val]
    elif mu == 0:
        return [0.0]
    else:
        return []

def transcritical_fps(mu):
    return [0.0, float(mu)]

def pitchfork_fps(mu):
    if mu <= 0:
        return [0.0]
    else:
        val = math.sqrt(mu)
        return [0.0, val, -val]

def logistic_bifurcation_r():
    return 3.0
`,
  tests: [
    {
      name: "Saddle-node fixed points (mu=-4)",
      expected: "[2.0, -2.0]\n",
      code: `{{FUNC}}\nprint(sorted(saddle_node_fps(-4), reverse=True))`,
    },
    {
      name: "Saddle-node fixed points (mu=1, no real FPs)",
      expected: "[]\n",
      code: `{{FUNC}}\nprint(saddle_node_fps(1))`,
    },
    {
      name: "Pitchfork fixed points (mu=4)",
      expected: "[2.0, 0.0, -2.0]\n",
      code: `{{FUNC}}\nprint(sorted(pitchfork_fps(4), reverse=True))`,
    },
    {
      name: "Logistic map first bifurcation",
      expected: "3.0000\n",
      code: `{{FUNC}}\nprint(f"{logistic_bifurcation_r():.4f}")`,
    },
  ],
};
