import type { Lesson } from "../../types";

export const correlationDimension: Lesson = {
  id: "correlation-dimension",
  title: "Correlation Dimension",
  chapterId: "fractal-geometry",
  content: `## Correlation Dimension

The **Grassberger-Procaccia correlation dimension** (D₂) is a fractal dimension that can be estimated directly from a time series, making it invaluable for analyzing real-world chaotic systems.

### The Correlation Sum

Given N points from an attractor, the **correlation sum** C(r) measures the fraction of point pairs within distance r:

\`\`\`
C(r) = (2 / N(N-1)) · #{pairs (i,j) with |xᵢ - xⱼ| < r}
\`\`\`

### The Correlation Dimension

For a fractal set, C(r) scales as a power law:
\`\`\`
C(r) ~ r^D₂
\`\`\`

So:
\`\`\`
D₂ = lim_{r→0} log C(r) / log r
\`\`\`

We estimate D₂ as the slope of log C(r) vs log r.

### Significance

| Attractor Type | D₂ |
|----------------|-----|
| Fixed point | 0 |
| Limit cycle | 1 |
| 2-torus | 2 |
| Lorenz attractor | ≈ 2.06 |
| Hénon attractor | ≈ 1.22 |

A **non-integer D₂** is strong evidence for a strange attractor (chaos).

### Your Task

Implement:
1. \`correlation_sum(points, r)\` — compute C(r) for 1D points
2. \`correlation_dimension(points, r_values)\` — estimate D₂ via linear regression on log-log plot`,
  starterCode: `import math

def correlation_sum(points, r):
    # Count pairs (i,j) with i<j where |points[i]-points[j]| < r
    # Return count / total_pairs (or 0.0 if no pairs)
    pass

def correlation_dimension(points, r_values):
    # Compute correlation_sum for each r in r_values
    # Estimate slope of log(C) vs log(r) using least squares
    # Skip r values where C(r) = 0
    pass`,
  solution: `import math

def correlation_sum(points, r):
    n = len(points)
    count = 0
    for i in range(n):
        for j in range(i + 1, n):
            if abs(points[i] - points[j]) < r:
                count += 1
    total_pairs = n * (n - 1) // 2
    return count / total_pairs if total_pairs > 0 else 0.0

def correlation_dimension(points, r_values):
    log_r = [math.log(r) for r in r_values]
    log_c = []
    for r in r_values:
        c = correlation_sum(points, r)
        if c > 0:
            log_c.append(math.log(c))
        else:
            log_c.append(float('-inf'))
    valid = [(log_r[i], log_c[i]) for i in range(len(r_values)) if log_c[i] != float('-inf')]
    if len(valid) < 2:
        return 0.0
    xs = [v[0] for v in valid]
    ys = [v[1] for v in valid]
    n = len(xs)
    mean_x = sum(xs) / n
    mean_y = sum(ys) / n
    num = sum((xs[i] - mean_x) * (ys[i] - mean_y) for i in range(n))
    den = sum((xs[i] - mean_x) ** 2 for i in range(n))
    return num / den if den != 0 else 0.0`,
  tests: [
    {
      name: "correlation sum basic",
      code: `{{FUNC}}\npoints = [0.0, 0.5, 1.0]\nprint(round(correlation_sum(points, 0.6), 4))`,
      expected: "0.6667\n",
    },
    {
      name: "line has dimension 1",
      code: `{{FUNC}}\npoints = [i/99.0 for i in range(100)]\nr_vals = [0.05, 0.1, 0.2, 0.3]\nd = correlation_dimension(points, r_vals)\nprint(round(d, 1))`,
      expected: "1.0\n",
    },
    {
      name: "two points correlation sum",
      code: `{{FUNC}}\npoints = [0.0, 1.0]\nprint(correlation_sum(points, 0.5), correlation_sum(points, 2.0))`,
      expected: "0.0 1.0\n",
    },
  ],
};
