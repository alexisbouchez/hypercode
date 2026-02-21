import type { Lesson } from "../../types";

export const smallWorld: Lesson = {
  id: "small-world",
  title: "Small-World Networks",
  chapterId: "networks",
  content: `# Small-World Networks

In 1998, Watts and Strogatz proposed a network model that captures a striking property observed in real-world networks: **high clustering** combined with **short average path lengths** — the "small-world" phenomenon.

## The Ring Lattice

Start with a **ring lattice**: $N$ nodes arranged in a circle, each connected to its $K$ nearest neighbours on each side (total degree $2K$).

\`\`\`
N=6, K=2: node 0 connects to {1, 2, 4, 5}
\`\`\`

Properties of the regular ring lattice:
- **High clustering:** nearby nodes share many common neighbours
- **Long path lengths:** to get across the ring takes $O(N/K)$ steps

## Average Path Length

The **average path length** $L$ is the mean shortest-path distance over all pairs of nodes:

$$L = \\frac{1}{N(N-1)} \\sum_{i \\neq j} d(i, j)$$

Shortest paths are computed using **breadth-first search (BFS)**.

## Global Clustering Coefficient

The **global clustering coefficient** $C$ is the mean local clustering coefficient across all nodes:

$$C = \\frac{1}{N} \\sum_i C_i$$

## Small-World Signature

A network is "small-world" when, compared to a random graph with the same $N$ and $\\langle k \\rangle$:
- $C \\gg C_{\\text{random}}$ (much higher clustering)
- $L \\approx L_{\\text{random}}$ (similar short paths)

Real examples: social networks (six degrees of separation), the brain's connectome, the power grid.

## Your Task

Implement:
- \`ring_lattice_adj(N, K)\` — build the ring lattice adjacency dict; node $i$ connects to $(i \\pm 1) \\% N, (i \\pm 2) \\% N, \\ldots, (i \\pm K) \\% N$
- \`bfs_distances(adj, source)\` — BFS from \`source\`, returns \`{node: distance}\` dict
- \`average_path_length(adj)\` — mean shortest path over all ordered pairs
- \`global_clustering(adj)\` — mean local clustering coefficient

Use \`from collections import deque\` for BFS.
`,
  starterCode: `from collections import deque

def ring_lattice_adj(N, K):
    pass

def bfs_distances(adj, source):
    pass

def average_path_length(adj):
    pass

def global_clustering(adj):
    pass
`,
  solution: `from collections import deque

def ring_lattice_adj(N, K):
    adj = {}
    for i in range(N):
        neighbors = set()
        for k in range(1, K + 1):
            neighbors.add((i + k) % N)
            neighbors.add((i - k) % N)
        adj[i] = neighbors
    return adj

def bfs_distances(adj, source):
    dist = {source: 0}
    queue = deque([source])
    while queue:
        node = queue.popleft()
        for neighbor in adj[node]:
            if neighbor not in dist:
                dist[neighbor] = dist[node] + 1
                queue.append(neighbor)
    return dist

def average_path_length(adj):
    N = len(adj)
    total = 0
    for node in adj:
        dists = bfs_distances(adj, node)
        total += sum(dists.values())
    return total / (N * (N - 1))

def global_clustering(adj):
    coeffs = []
    for node in adj:
        neighbors = adj[node]
        k = len(neighbors)
        if k < 2:
            coeffs.append(0.0)
            continue
        edges_among = 0
        neighbor_list = list(neighbors)
        for i in range(len(neighbor_list)):
            for j in range(i + 1, len(neighbor_list)):
                if neighbor_list[j] in adj[neighbor_list[i]]:
                    edges_among += 1
        coeffs.append(edges_among / (k * (k - 1) / 2))
    return sum(coeffs) / len(coeffs)
`,
  tests: [
    {
      name: "ring_lattice_adj(6,2): each node has degree 4",
      expected: "4\n",
      code: `{{FUNC}}
print(len(ring_lattice_adj(6, 2)[0]))`,
    },
    {
      name: "average_path_length of ring lattice N=6 K=2",
      expected: "1.2000\n",
      code: `{{FUNC}}
print(f"{average_path_length(ring_lattice_adj(6, 2)):.4f}")`,
    },
    {
      name: "global_clustering of ring lattice N=6 K=2",
      expected: "0.6667\n",
      code: `{{FUNC}}
print(f"{global_clustering(ring_lattice_adj(6, 2)):.4f}")`,
    },
    {
      name: "bfs_distances from node 0 to node 3 in ring N=6 K=2",
      expected: "2\n",
      code: `{{FUNC}}
print(bfs_distances(ring_lattice_adj(6, 2), 0)[3])`,
    },
  ],
};
