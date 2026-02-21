import type { Lesson } from "../../types";

export const networkDegree: Lesson = {
  id: "network-degree",
  title: "Network Degree Distribution",
  chapterId: "networks",
  content: `# Network Degree Distribution

A **network** (or graph) consists of **nodes** (vertices) connected by **edges**. Networks appear everywhere in complex systems: social connections, neural circuits, power grids, the internet.

## Representation

We represent networks as **adjacency lists** — a Python dict mapping each node to a set of its neighbours:

\`\`\`python
adj = {
    0: {1, 2, 3},
    1: {0, 2},
    2: {0, 1},
    3: {0},
}
\`\`\`

## Degree

The **degree** $k_i$ of node $i$ is the number of its neighbours:

$$k_i = |\\text{neighbours}(i)|$$

The **mean degree** of a network with $N$ nodes and $E$ edges is:

$$\\langle k \\rangle = \\frac{2E}{N}$$

(each edge contributes 2 to the total degree sum).

## Degree Distribution

The **degree distribution** $P(k)$ gives the fraction of nodes with exactly $k$ neighbours. For:
- **Random (Erdős–Rényi) graphs:** $P(k)$ is Poisson-distributed
- **Scale-free (Barabási–Albert) graphs:** $P(k) \\propto k^{-\\gamma}$ (power law)

## Clustering Coefficient

The **local clustering coefficient** $C_i$ measures how tightly connected node $i$'s neighbours are:

$$C_i = \\frac{\\text{edges among neighbours of } i}{\\binom{k_i}{2}} = \\frac{\\text{edges among neighbours}}{k_i(k_i-1)/2}$$

$C_i = 1$ means all neighbours are connected to each other; $C_i = 0$ means none are.

## Your Task

Implement:
- \`node_degree(adj, node)\` — degree of a single node
- \`mean_degree(adj)\` — average degree over all nodes
- \`degree_distribution(adj)\` — dict \`{k: fraction}\` of nodes with that degree
- \`clustering_coefficient(adj, node)\` — local clustering coefficient (return 0 if degree < 2)
`,
  starterCode: `def node_degree(adj, node):
    pass

def mean_degree(adj):
    pass

def degree_distribution(adj):
    pass

def clustering_coefficient(adj, node):
    pass
`,
  solution: `def node_degree(adj, node):
    return len(adj[node])

def mean_degree(adj):
    total = sum(len(neighbors) for neighbors in adj.values())
    return total / len(adj)

def degree_distribution(adj):
    degrees = [len(adj[n]) for n in adj]
    counts = {}
    for d in degrees:
        counts[d] = counts.get(d, 0) + 1
    n = len(adj)
    return {k: v / n for k, v in counts.items()}

def clustering_coefficient(adj, node):
    neighbors = adj[node]
    k = len(neighbors)
    if k < 2:
        return 0.0
    edges_among = 0
    neighbor_list = list(neighbors)
    for i in range(len(neighbor_list)):
        for j in range(i + 1, len(neighbor_list)):
            if neighbor_list[j] in adj[neighbor_list[i]]:
                edges_among += 1
    return edges_among / (k * (k - 1) / 2)
`,
  tests: [
    {
      name: "node_degree of hub node is 3",
      expected: "3\n",
      code: `{{FUNC}}
adj = {0:{1,2,3}, 1:{0,2}, 2:{0,1}, 3:{0}}
print(node_degree(adj, 0))`,
    },
    {
      name: "mean_degree of the graph is 2.0",
      expected: "2.0000\n",
      code: `{{FUNC}}
adj = {0:{1,2,3}, 1:{0,2}, 2:{0,1}, 3:{0}}
print(f"{mean_degree(adj):.4f}")`,
    },
    {
      name: "degree_distribution: fraction of degree-1 nodes is 0.25",
      expected: "0.2500\n",
      code: `{{FUNC}}
adj = {0:{1,2,3}, 1:{0,2}, 2:{0,1}, 3:{0}}
print(f"{degree_distribution(adj)[1]:.4f}")`,
    },
    {
      name: "clustering_coefficient of hub node is 1/3",
      expected: "0.3333\n",
      code: `{{FUNC}}
adj = {0:{1,2,3}, 1:{0,2}, 2:{0,1}, 3:{0}}
print(f"{clustering_coefficient(adj, 0):.4f}")`,
    },
  ],
};
