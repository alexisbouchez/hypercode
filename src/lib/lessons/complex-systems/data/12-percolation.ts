import type { Lesson } from "../../types";

export const percolation: Lesson = {
  id: "percolation",
  title: "Percolation Theory",
  chapterId: "statistical-physics",
  content: `# Percolation Theory

**Percolation theory** studies how connectivity emerges in random systems. It has applications in epidemic spreading, oil recovery, and network resilience.

## Site Percolation on a 1D Chain

In **site percolation**, each site on a lattice is independently occupied with probability $p$. A **cluster** is a maximal connected group of occupied sites.

In 1D, sites are connected only to their immediate left and right neighbors.

## Percolation Threshold

The **percolation threshold** $p_c$ is the minimum $p$ at which an infinite cluster exists:

- **1D chain**: $p_c = 1$ — you must occupy *all* sites for the cluster to span the system
- **2D square lattice**: $p_c \\approx 0.5927$
- **3D cubic lattice**: $p_c \\approx 0.3116$

## Key Quantities

**Mean cluster size** (average over all clusters):
$$\\langle s \\rangle = \\frac{\\sum_i s_i}{\\text{number of clusters}}$$

**Percolation probability** (fraction of sites in the largest cluster):
$$P_{\\infty} = \\frac{s_{\\max}}{N}$$

Near the threshold: $P_{\\infty} \\sim (p - p_c)^\\beta$ with $\\beta = 1$ in 1D.

## Random Number Generator

We use a **Linear Congruential Generator (LCG)**:
$$x_{n+1} = (a \\cdot x_n + c) \\mod m$$
with $a = 1664525$, $c = 1013904223$, $m = 2^{32}$. A site is occupied if $x_{n+1}/m < p$.

## Implementation

\`\`\`python
def generate_1d_lattice(N, p, seed=42):
    # LCG: a=1664525, c=1013904223, m=2**32
    # Each step: state=(a*state+c)%m; occupied if state/m < p
    ...

def find_clusters_1d(lattice):
    # Scan left to right, track contiguous runs of 1s
    # Return list of cluster sizes
    ...

def mean_cluster_size(cluster_sizes):
    # Return average cluster size
    ...

def percolation_probability(cluster_sizes, N):
    # Return max(cluster_sizes) / N
    ...
\`\`\`
`,
  starterCode: `def generate_1d_lattice(N, p, seed=42):
    pass

def find_clusters_1d(lattice):
    pass

def mean_cluster_size(cluster_sizes):
    pass

def percolation_probability(cluster_sizes, N):
    pass
`,
  solution: `def generate_1d_lattice(N, p, seed=42):
    a = 1664525
    c = 1013904223
    m = 2 ** 32
    state = seed
    result = []
    for _ in range(N):
        state = (a * state + c) % m
        result.append(1 if state / m < p else 0)
    return result

def find_clusters_1d(lattice):
    clusters = []
    current = 0
    for site in lattice:
        if site == 1:
            current += 1
        else:
            if current > 0:
                clusters.append(current)
            current = 0
    if current > 0:
        clusters.append(current)
    return clusters

def mean_cluster_size(cluster_sizes):
    if not cluster_sizes:
        return 0.0
    return sum(cluster_sizes) / len(cluster_sizes)

def percolation_probability(cluster_sizes, N):
    if not cluster_sizes:
        return 0.0
    return max(cluster_sizes) / N
`,
  tests: [
    {
      name: "Find clusters in 1D lattice",
      expected: "[2, 1, 3]\n",
      code: `{{FUNC}}\nprint(find_clusters_1d([1,1,0,1,0,1,1,1]))`,
    },
    {
      name: "Mean cluster size",
      expected: "2.0000\n",
      code: `{{FUNC}}\nprint(f"{mean_cluster_size([2,1,3]):.4f}")`,
    },
    {
      name: "Percolation probability",
      expected: "0.3750\n",
      code: `{{FUNC}}\nprint(f"{percolation_probability([2,1,3], 8):.4f}")`,
    },
    {
      name: "Generate 1D lattice length",
      expected: "10\n",
      code: `{{FUNC}}\nprint(len(generate_1d_lattice(10, 1.0, seed=42)))`,
    },
  ],
};
