import type { Lesson } from "../../types";

export const soc: Lesson = {
  id: "soc",
  title: "Self-Organized Criticality",
  chapterId: "emergence",
  content: `# Self-Organized Criticality

**Self-organized criticality (SOC)** describes systems that naturally evolve toward a critical state without external tuning. The hallmark is **power-law distributions** of event sizes (avalanches).

## The 1D Bak-Tang-Wiesenfeld Sandpile

Proposed by Bak, Tang, and Wiesenfeld (1987), the sandpile model is the canonical SOC system:

1. Each site $i$ has a height $h_i$ (number of sand grains)
2. A site is **unstable** if $h_i \\geq \\text{threshold}$ (we use threshold $= 2$)
3. **Toppling rule** in 1D: an unstable site loses 2 grains, and each neighbor gains 1:
   $$h_i \\to h_i - 2, \\quad h_{i-1} \\to h_{i-1} + 1, \\quad h_{i+1} \\to h_{i+1} + 1$$
4. Sand falls off the boundaries (open boundary conditions)
5. Add one grain to a random site, then topple until stable
6. **Avalanche size** = total number of topplings

## Why Criticality Emerges

The system self-organizes to a state where:
- Adding one grain triggers avalanches of all sizes
- Avalanche sizes follow a power law: $P(s) \\sim s^{-\\tau}$

This criticality is achieved **without** tuning any external parameter.

## Mean Field Approximation

The mean avalanche size grows as the system approaches its capacity. For a system of size $N$, the mean height at the critical state is approximately $\\text{threshold} - 1$.

## Implementation

\`\`\`python
def sandpile_topple(heights, threshold=2):
    # Repeatedly topple all unstable sites until stable
    # Returns (final_heights, total_topplings)
    # Open boundaries: grains fall off edges
    ...

def sandpile_add_grain(heights, site):
    # Return new heights list with one grain added at site
    ...

def mean_avalanche_size(avalanche_sizes):
    # Return mean of all values (including zeros)
    ...

def power_law_check(avalanche_sizes):
    # MLE exponent estimate for power law on sizes > 0
    # tau = 1 + n / sum(log(s/s_min))
    ...
\`\`\`
`,
  starterCode: `def sandpile_topple(heights, threshold=2):
    pass

def sandpile_add_grain(heights, site):
    pass

def mean_avalanche_size(avalanche_sizes):
    pass

def power_law_check(avalanche_sizes):
    pass
`,
  solution: `import math

def sandpile_topple(heights, threshold=2):
    h = list(heights)
    total_topplings = 0
    while True:
        unstable = [i for i in range(len(h)) if h[i] >= threshold]
        if not unstable:
            break
        for i in unstable:
            h[i] -= 2
            total_topplings += 1
            if i > 0:
                h[i - 1] += 1
            if i < len(h) - 1:
                h[i + 1] += 1
    return (h, total_topplings)

def sandpile_add_grain(heights, site):
    h = list(heights)
    h[site] += 1
    return h

def mean_avalanche_size(avalanche_sizes):
    if not avalanche_sizes:
        return 0.0
    return sum(avalanche_sizes) / len(avalanche_sizes)

def power_law_check(avalanche_sizes):
    positive = [s for s in avalanche_sizes if s > 0]
    if not positive:
        return 0.0
    s_min = min(positive)
    n = len(positive)
    log_sum = sum(math.log(s / s_min) for s in positive)
    if log_sum == 0:
        return 0.0
    return 1.0 + n / log_sum
`,
  tests: [
    {
      name: "Single topple from height 3",
      expected: "1\n",
      code: `{{FUNC}}\nprint(sandpile_topple([0,3,0])[1])`,
    },
    {
      name: "Cascade topplings from [3,3,3]",
      expected: "10\n",
      code: `{{FUNC}}\nprint(sandpile_topple([3,3,3])[1])`,
    },
    {
      name: "Add grain to site",
      expected: "[0, 2, 0]\n",
      code: `{{FUNC}}\nprint(sandpile_add_grain([0,1,0],1))`,
    },
    {
      name: "Mean avalanche size",
      expected: "1.0000\n",
      code: `{{FUNC}}\nprint(f"{mean_avalanche_size([0,1,0,2,1,3,0,1]):.4f}")`,
    },
  ],
};
