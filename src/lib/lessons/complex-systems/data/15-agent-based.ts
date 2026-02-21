import type { Lesson } from "../../types";

export const agentBased: Lesson = {
  id: "agent-based",
  title: "Agent-Based Modeling",
  chapterId: "emergence",
  content: `# Agent-Based Modeling

**Agent-based models (ABMs)** simulate collections of autonomous agents following simple local rules. Global patterns — segregation, flocking, traffic jams — emerge without central control.

## Schelling's Segregation Model

Thomas Schelling (1971) showed that mild individual preferences for same-type neighbors produce **strong global segregation**. Even agents who are happy with 50% same-type neighbors will end up in nearly homogeneous clusters.

### 1D Model

We use a 1D ring (periodic boundary conditions):
- Grid values: \`0\` = empty, \`1\` = type A, \`2\` = type B
- An agent at position $i$ looks at neighbors within distance \`neighborhood\`
- The agent is **happy** if at least fraction $f$ of its non-empty neighbors are the same type
- **Satisfaction**: fraction of agents who are happy

### Segregation Index

The segregation index measures how same-type pairs dominate adjacent slots:

$$\\text{SI} = \\frac{\\text{same-type adjacent pairs}}{\\text{total adjacent non-empty pairs}}$$

For a periodic ring of $N$ sites, there are $N$ adjacent pairs $(i, (i+1) \\% N)$. Only non-empty pairs count.

- Perfectly alternating \`[1,2,1,2,...]\`: $\\text{SI} = 0$
- Perfectly segregated \`[1,1,...,2,2,...]\`: $\\text{SI} \\to 1$ (minus boundary pairs)

## Implementation

\`\`\`python
def segregation_index(grid):
    # Count adjacent same-type pairs / total adjacent non-empty pairs
    # Periodic boundary conditions
    ...

def count_happy(grid, f=0.5, neighborhood=1):
    # Count agents where same-type fraction of neighbors >= f
    # Skip empty sites (value 0)
    # Periodic boundary conditions
    ...

def schelling_satisfaction(grid, f=0.5, neighborhood=1):
    # Return fraction of non-empty agents that are happy
    ...

def interface_density(grid):
    # Fraction of adjacent non-empty pairs that are DIFFERENT type
    # = 1 - segregation_index(grid)
    ...
\`\`\`
`,
  starterCode: `def segregation_index(grid):
    pass

def count_happy(grid, f=0.5, neighborhood=1):
    pass

def schelling_satisfaction(grid, f=0.5, neighborhood=1):
    pass

def interface_density(grid):
    pass
`,
  solution: `def segregation_index(grid):
    n = len(grid)
    same = 0
    total = 0
    for i in range(n):
        j = (i + 1) % n
        a = grid[i]
        b = grid[j]
        if a != 0 and b != 0:
            total += 1
            if a == b:
                same += 1
    if total == 0:
        return 0.0
    return same / total

def count_happy(grid, f=0.5, neighborhood=1):
    n = len(grid)
    happy = 0
    for i in range(n):
        if grid[i] == 0:
            continue
        same = 0
        total_neighbors = 0
        for d in range(1, neighborhood + 1):
            for nb in [grid[(i - d) % n], grid[(i + d) % n]]:
                if nb != 0:
                    total_neighbors += 1
                    if nb == grid[i]:
                        same += 1
        if total_neighbors == 0:
            happy += 1
        elif same / total_neighbors >= f:
            happy += 1
    return happy

def schelling_satisfaction(grid, f=0.5, neighborhood=1):
    n_agents = sum(1 for x in grid if x != 0)
    if n_agents == 0:
        return 0.0
    return count_happy(grid, f, neighborhood) / n_agents

def interface_density(grid):
    return 1.0 - segregation_index(grid)
`,
  tests: [
    {
      name: "Segregation index of alternating pairs",
      expected: "0.5000\n",
      code: `{{FUNC}}\nprint(f"{segregation_index([1,1,2,2,1,1,2,2]):.4f}")`,
    },
    {
      name: "Segregation index of fully segregated grid",
      expected: "0.7500\n",
      code: `{{FUNC}}\nprint(f"{segregation_index([1,1,1,1,2,2,2,2]):.4f}")`,
    },
    {
      name: "Count happy agents (alternating pairs, f=0.5)",
      expected: "8\n",
      code: `{{FUNC}}\nprint(count_happy([1,1,2,2,1,1,2,2], f=0.5, neighborhood=1))`,
    },
    {
      name: "Schelling satisfaction of fully segregated grid",
      expected: "1.0000\n",
      code: `{{FUNC}}\nprint(f"{schelling_satisfaction([1,1,1,1,2,2,2,2]):.4f}")`,
    },
  ],
};
